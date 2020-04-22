{-
    This module contains the functions required to
    perform the static analysis of a pascal program
    given the AST of such program
-}
module Pascal.Analyzer where

import Control.Monad
import Control.Monad.State
import Data.Maybe
---------------------------------------------
import qualified Data.List          as L
import qualified Data.Map           as M
import qualified Data.Set           as S
---------------------------------------------
import qualified Pascal.SymbolTable as ST
import qualified Pascal.Data        as D
---------------------------------------------

data States = Program 
            | Function{currFun :: String, ok :: Bool} 
            deriving(Eq,Show)

data ErrClass = Ok
              | BuiltFunRedef{ 
                    funRedef  :: String
                    }
              | SymRedef{
                    redefSym :: String,
                    orginSymPos :: (Int,Int)
                    }
              | UndefinedRef{
                    undefSym :: String
                    }
              | UndeFunRef{
                    undefFun :: String
                    }
                deriving(Eq)

data ContextError = ContextError{
                        errPos :: (Int,Int), -- Error position
                        errType:: ErrClass  -- Aditional error info
                    }
                    deriving(Eq)

data ContextState = ContextState{
                        --Symbol table to control definitions
                        symTable :: ST.SymbolTable, 
                        --Error stack
                        errors   :: [ContextError],
                        --analysis state stack
                        analysisState :: [States]
                    }

type RetState a  = StateT ContextState IO a

analyzeAST :: D.MainProgram -> IO (Either String D.MainProgram)
analyzeAST mp = do 
    (p, state) <- runStateT (analyzer mp) (ContextState ST.newTable [] [Program] )

    let 
        errs = errors state

    if null errs
        then return (Right p)
        else return (Left . unlines . map show $ errs)

analyzer :: D.MainProgram ->  RetState D.MainProgram
analyzer (pname, p) = do
        newP <- analyzer' p

        return  (pname, newP)

analyzer' :: D.Program -> RetState D.Program
analyzer' p@D.Program{D.progInstrs = pi, D.progDecl = pd} = do
        newDec  <- forM  pd checkDecl
        newInst <- checkStmnt pi 
        return $ D.Program newInst newDec
 
    where
        --Checks if a declaration makes sense in the current state.
        --If it does, return a clean version of such declaration.
        checkDecl :: D.Declaration -> RetState D.Declaration

        --Check declaration of variable
        checkDecl v@D.Variable{} = do
            state <- get
            let 
                errs = errors state
                varType = D.varType v
                varname = D.varId v
                st      = symTable state
                err     = checkSymAvail varname st
                decpos  = D.vDeclpos v
                newError= ContextError{errPos = decpos, errType = err}
                symType = case varType of
                            D.RealT    -> ST.RealVar{ST.rval = 0}
                            D.BooleanT -> ST.BoolVar{ST.bval = True}

                newSym  = ST.Symbol{
                            ST.symId    = varname,
                            ST.symScope = 0, --insert symbol will fix this
                            ST.symType  = symType,
                            ST.symPos   = decpos 
                            }

                newSt 
                    | err == Ok = fromJust . ST.insertSym newSym $ st
                    | otherwise = st
                
                newErrs 
                    | err == Ok = errs
                    | otherwise = newError:errs 

            put state{ errors=newErrs, symTable = newSt }
            return v
        
        -- check a function
        checkDecl f@D.Function{} = do
            state <- get
            let 
                fname    = D.funcId f
                st       = symTable state
                errs     = errors state
                errFname = checkSymAvail fname st 
                args'    = D.funcArgs f
                decpos   = D.fDeclPos f
                ftype    = D.funcType f
                fbody    = D.funcBody f
                args     = [ D.Variable s t decpos | (s,t) <- args']
                newSType = ST.Function{
                            ST.fbid     = ST.scopeCnt st,
                            ST.funcArgs = args',
                            ST.funcType = ftype,
                            ST.funcBody = fbody
                            }
                newSym   = ST.Symbol{
                            ST.symId    = fname,
                            ST.symScope = head . ST.scopeStk $ st,
                            ST.symType  = newSType,
                            ST.symPos   = decpos
                            }
                newErrs  = case errFname of
                                Ok -> errs
                                _  -> ContextError decpos errFname : errs
                newSt'   = ST.pushEmptyScope $ fromMaybe st (ST.insertSym newSym st)
                 
                topState = case ftype of
                            D.NoneT -> Program
                            _       -> Function fname False
            --simulate
            put state{  errors = newErrs, 
                        symTable = newSt',
                        analysisState = topState:analysisState state
                        }
            -- add the args variables to the current context
            mapM_ checkDecl args
            -- if the function is a function with return value, 
            -- add one more variable, the return variable
            when (ftype /= D.NoneT) (void . checkDecl $ D.Variable fname ftype decpos)
            --run the analyzer in the function body 
            newBody <- analyzer' fbody
            --Pop the current context & the analysis state
            resultState <- get
            put resultState{
                    analysisState = tail . analysisState $ resultState,
                    symTable = ST.popScope . symTable $ resultState
                }
            return f{D.funcBody = newBody}


        --Checks if a statement makes sense in the current state.
        --If it does, return a clean version of such statement
        checkStmnt :: D.Statement  -> RetState D.Statement

        --Check Block
        checkStmnt b@D.Block{D.blockInsts = insts} = do
            newInst <- forM insts checkStmnt
            return b{D.blockInsts = newInst}

        --Check assign
        checkStmnt a@D.Assign{D.assVarId = s, D.assVal = expr, D.assPos = p} = do
            state@ContextState{errors = errs, symTable = st} <- get
            
            newErrs <- checkExpr expr    
            put state{errors = [ContextError p s | s <- newErrs] ++ errs}
            return a

        checkStmnt x = return x

        checkExpr :: D.Exp -> RetState [ErrClass]
        checkExpr D.IdExpr{D.idExpr = s} = do
            ContextState{symTable=st} <- get
            let 
                sym  = ST.findSym s st
                notVarError = [UndeFunRef s | ST.checkSym s ST.isFunc st]
                errs = case sym of 
                        Nothing -> [UndefinedRef s]
                        Just _  -> notVarError
                
            return errs
        checkExpr _ = return  []
        --reduceExpr :: D.Exp -> Either D.BoolExp D.NumExp


-- aux function: tells if a symbol can be added.
-- If not, then return the error generated
checkSymAvail :: String -> ST.SymbolTable -> ErrClass
checkSymAvail s st = err
    where 
        sym       = ST.findSym s st
        sym'      = fromJust sym
        symscope  = ST.symScope sym'
        currScope = head . ST.scopeStk $ st
        sympos    = ST.symPos sym'
        fun       = S.member s D.builtInFuns
        err 
            | fun = BuiltFunRedef s -- Redefinition of built in fun
            | isJust sym &&  -- Redefinition of already existing symbol
              currScope == symscope = SymRedef s sympos
            | otherwise = Ok

------------------------------------------------------------------
-- < Error messages > --------------------------------------------

instance Show ContextError where

    show (ContextError (l,c) err) = 
        "Static Error near of line: " ++ show l ++ 
        ", column: " ++ show c ++ ".\n" ++ 
        (unlines . map ("  "++) . lines . show $ err )

instance Show ErrClass where

    show Ok = "Everything ok"
    show (BuiltFunRedef s) = 
        "Redefinition of built-in function '" ++ s ++ "'"

    show (SymRedef s (ox,oy)) = 
        "Redefinition of symbol '" ++ s ++ "' defined in line: " ++
        show ox ++ ", column: " ++ show oy   

    show (UndefinedRef s) = 
        "Undefined symbol reference: '" ++ s ++ "'"

    show (UndeFunRef s) =
        "Unvalid reference to '" ++ s ++ "'. This is a function, not a variable."

-- Needed to use IO within State monad context
io :: IO a -> StateT ContextState IO a
io = liftIO  
