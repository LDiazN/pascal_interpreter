{-
    This module contains the functions required to
    perform the static analysis of a pascal program
    given the AST of such program
-}
module Pascal.Analyzer where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Either
---------------------------------------------
import qualified Data.List          as L
import qualified Data.Map           as M
import qualified Data.Set           as S
---------------------------------------------
import qualified Pascal.SymbolTable as ST
import qualified Pascal.Data        as D
---------------------------------------------

--All the possible static errors 
data ErrClass = Ok
                --Redefinition of built in function
              | BuiltFunRedef{ 
                    funRedef  :: String
                    }
                --Function without return
              | FunWOutReturn{
                  fworet :: String 
              }
                --Redefinition of symbol in the same context
              | SymRedef{
                    redefSym :: String,
                    orginSymPos :: (Int,Int)
                    }
                --Reference to symbol that does not exists
              | UndefinedRef{
                    undefSym :: String
                    }
                --Unvalid function reference
              | UndeFunRef{
                    undefFun :: String
                    }
                --Unvalid procedure reference
              | UndeProcRef{
                    undefProc :: String
                    }
                --Unvalid variable reference
              | UndeVarRef{
                    undefVar :: String
                    }
                --Unvalid args in function call
              | UnvalidArgs{
                    unvArgsFun :: String,
                    unvArgsMsg :: String
                    }
                --Unmatching types in binary operation
              | UnmatchTypesOper{
                    unmop  :: String,     --operator
                    unmop1 :: D.DataType, --operand 1
                    unmop2 :: D.DataType  --operand 2
                    }
                --Unmatiching types in unary operation
              | UnmatchTypesOper1{ --for unary operators
                    unmopUnry   :: String,     --operator
                    unmopUnryIn :: D.DataType --operand 
                    }
                --Unmatiching types in variable assign
              | UnmatchTypesAssign{
                    unmtVarId  :: String,
                    unmVarType :: D.DataType,
                    unmExpType :: D.DataType
                    }
                --condition in some conditional statement is not a 
                --boolean expression
              | CondNotBool{
                    condPos :: (Int,Int)
              }
                --Iterator in for statement is not a numeric variable
              | IteretNotNum
                --Division by zero in numeric expression
              | DivideByZero 
                -- Unmatching types between case labels and comparing
                -- expression
              | UnmatchingCaseTypes
                --Case label is not a constant expression
              | CaseNotConstLabel
                -- loop control out of context (continue or break
                -- out of loop statement)
              | LoopCntrlOOC 
                --iterator variable assign
              | UnvLoopAssign{
                  itVar :: String
              } 
                deriving(Eq)

-- Main error type
data ContextError = ContextError{
                        errPos :: (Int,Int), -- Error position
                        errType:: ErrClass  -- Aditional error info
                    }
                    deriving(Eq)
                    
--Needed to check if a continue/break statements are placed
--inside a loop
data States = Program 
            | Loop 
            deriving(Eq,Show)

-- Analyzer state
data ContextState = ContextState{
                        --Symbol table to control definitions
                        symTable :: ST.SymbolTable, 
                        --Error stack
                        errors   :: [ContextError],
                        --analysis state stack
                        analysisState :: [States],
                        --if the program being checked is a function.
                        --This is a stack of function names, needed to remember
                        --nested function declarations
                        checkingFun :: [String],
                        --If the function checked is ok
                        funOk :: Bool,
                        --Stack of for variables. You can't assign an
                        --iterator variable
                        iters :: [String]

                    }

type RetState a  = StateT ContextState IO a

analyzeAST :: D.MainProgram -> IO (Either String D.MainProgram)
analyzeAST mp = do 
    (p, state) <- runStateT (analyzer mp) (ContextState ST.newTable [] [Program] [] False [])

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

        --Check variable declaration
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
                newSt'   = ST.pushFunc fname . ST.pushEmptyScope $ fromMaybe st (ST.insertSym newSym st)
                 
            --simulate
            put state{  errors = newErrs, 
                        symTable = newSt',
                        checkingFun = fname:checkingFun state,
                        funOk = False
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
                    symTable    = ST.popFunc . ST.popScope . symTable $ resultState,
                    checkingFun = tail . checkingFun $ resultState,
                    funOk = False
                }
            unless (funOk resultState || ftype == D.NoneT) $ do
                errState <- get
                put errState{
                    errors = ContextError decpos (FunWOutReturn fname) :
                             errors errState }

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
            state@ContextState{errors = errs, symTable = st, checkingFun = cf, iters=its} <- get
            let 
                --Expression checking
                expErrs = case cleaned' of
                            Left errs -> errs
                            _         -> []
                cleaned'= cleanExpr expr st
                cleaned = fromRight expr cleaned'
                expType = getType cleaned st
                --Symbol checking
                sym     = ST.findSym s st
                sym'    = fromJust sym
                stype
                    | ST.isBoolVar sym' = D.BooleanT
                    | ST.isRealVar sym' = D.RealT
                    | otherwise = error "failed to check assign"
                isIter = filter (s==) its
                -- Creating cleaned expression
                newExp = cleaned

                -- Error checking
                newErrs 
                    | isNothing sym = 
                            [UndefinedRef s]
                    | ST.isFunc sym' = 
                            [UndeFunRef s] 
                    | ST.isProc sym' = 
                            [UndeProcRef s]
                    | not (null expErrs) = 
                            expErrs
                    | stype /= expType = 
                            [UnmatchTypesAssign s stype expType]
                    | not (null isIter) =
                            [UnvLoopAssign s]
                    | otherwise = [] 
                newErrs' = [ContextError p s | s <- newErrs] ++ errs
                --Function return checking
                newFunOk 
                    | null cf = False
                    | otherwise = funOk state || head cf == s
            put state{
                        errors = newErrs',
                        funOk = newFunOk
                    }
            
            return a{D.assVal = newExp}

        --Check if statement
        checkStmnt ifstm@D.If{  D.ifCond=expr, 
                                D.succInst=sins, 
                                D.failInst=fins,
                                D.ipos = p} = do
            state@ContextState{symTable = st, errors = errs} <- get
            let 
                newBid  = ST.scopeCnt st
                newSt   = ST.pushEmptyScope st
                cleaned'= cleanExpr expr st 
                cleaned = fromRight expr cleaned' 
                expErrs = fromLeft [] cleaned'
                newErrs 
                    | not (null expErrs) = 
                        [ ContextError p s | s <- expErrs]
                    | getType cleaned st /= D.BooleanT =  
                        [ContextError p (CondNotBool p)]
                    | otherwise = []

            put state{errors = newErrs ++ errs, symTable=newSt}
            newSuccInst <- checkStmnt sins
            newFailInst <- checkStmnt fins
            lastState   <- get  
            put lastState{symTable = ST.popScope . symTable $ lastState}
            
            return ifstm{
                         D.ifCond   = cleaned,  
                         D.succInst = newSuccInst,
                         D.failInst = newFailInst,
                         D.ibid     = newBid
                        }
        
        --Check while
        checkStmnt w@D.While{D.whCond = expr, D.whBody = wbody, D.wpos = p } = do
            state@ContextState{ symTable = st, 
                                errors=errs, 
                                analysisState=states} <- get
            let 
                newStates = Loop:states
                expErrs = fromLeft [] newCond'
                cleaned = case reduceExpr expr st of
                            Left be  -> D.BoolExpr be
                            Right ne -> D.NumExpr ne
                newErrs 
                    | not (null expErrs) = 
                        [ContextError p s | s<-expErrs]
                    | getType cleaned st /= D.BooleanT = 
                        [ContextError p (CondNotBool p)]
                    | otherwise = []
                newBid  = ST.scopeCnt st
                newSt   = ST.pushEmptyScope st
                newCond'= cleanExpr expr st
                newCond = fromRight expr newCond'
                    

            -- Put the state with a new scope pushed, errors updates
            -- and the loop state at the top of the states stack
            put state{
                    errors = newErrs ++ errs,
                    symTable = newSt,
                    analysisState = newStates
                    }
            -- get the corrected while body
            newBody <- checkStmnt wbody

            --The resulting state
            finalState <- get

            --Update the state to pop the scope and the Loop state
            put finalState{
                symTable = ST.popScope . symTable $ finalState,
                analysisState = tail . analysisState $ finalState
                }

            --return a corrected while
            return w{
                    D.wbid = newBid,
                    D.whCond = newCond,
                    D.whBody = newBody 
                    }

        --Check a for loop
        checkStmnt f@D.For{} = do
            state@ContextState{ symTable = st,
                                errors = errs,
                                iters = its,
                                analysisState = states} <- get
            let 
                --For data:
                iter     = D.forIter f
                initExp  = D.forInitVal f
                endExp   = D.forEndVal f
                forBody  = D.forBody f
                initType = getType newInit st
                endType  = getType newEnd st
                p        = D.fpos f
                -- New For Data:
                newBid = ST.scopeCnt st
                cleanedInit'= cleanExpr initExp st
                cleanedEnd' = cleanExpr endExp st
                newInit = fromRight initExp cleanedInit'
                newEnd  = fromRight endExp cleanedEnd'

                newFor = f{
                    D.forInitVal = newInit,
                    D.forEndVal  = newEnd,
                    D.fbid = newBid
                }

                --Error checking:
                expErrors1 = fromLeft [] cleanedInit'
                expErrors2 = fromLeft [] cleanedEnd'
                itSym      = ST.findSym iter st
                itSym'     = fromJust itSym
                newErrs 
                    | not (null expErrors1) || not (null expErrors2) = 
                        [ContextError p s | s <- expErrors1 ++ expErrors2]
                    | initType /= D.RealT || endType /= D.RealT = 
                        [ContextError p IteretNotNum]
                    | otherwise = []
                newErrs' 
                    | isNothing itSym = ContextError p (UndefinedRef iter) : newErrs
                    | otherwise = newErrs
                --new State Data:
                newStates = Loop:states
                newSt = ST.pushEmptyScope st 
                
            
            --Put the new state
            put state{
                    symTable = newSt,
                    analysisState = newStates,
                    errors = newErrs' ++ errs,
                    iters  = iter:its
                }
            newBody <- checkStmnt forBody
            --Get resulting state
            finalState <- get

            --pop loop state and current scope
            put finalState{
                symTable = ST.popScope . symTable $ finalState,
                analysisState = tail . analysisState $ finalState,
                iters = tail . iters $ finalState
            }
            return newFor{D.forBody = newBody}

        --Check case
        checkStmnt cs@D.Case{} = do
            state@ContextState{ symTable = st, 
                                errors = errs } <- get
            let 
                -- 'Case' current data
                compExp  = D.caseExp cs
                csGuards = D.caseGuards cs
                csElse   = D.caseElse cs
                p = D.cpos cs

                guardLabels = map fst csGuards
                guardStmnts = map snd csGuards

                -- new Case data
                newBid      = ST.scopeCnt st
                newCaseExp' = cleanExpr compExp st
                newCaseExp  = fromRight compExp newCaseExp'

                expType     = getType newCaseExp st
                
                newLabels'  = map (`cleanExpr` st) guardLabels 
                newLabels   = rights newLabels'
                    
                -- Error check
                    -- errors in label expressions
                labErrs = concat . lefts $ newLabels'
                    -- errors in condition expression
                condErrs = concat . lefts $ [newCaseExp']
                    --If the guards and the expressions are ok, then check the types.
                    --If types are ok, then check if they are constant expressions
                newErrs 
                    | not (null labErrs) || not (null condErrs) = 
                        [ContextError p s | s <- labErrs ++ condErrs]
                    | any ((/=expType) . flip getType st) newLabels = 
                        [ContextError p UnmatchingCaseTypes ]
                    | not . all D.isConstant $ newLabels =
                        [ContextError p CaseNotConstLabel]
                    | otherwise = []

                
            --now we have to push an empty scope and check all the statements in the 
            --right side of the case labels
            put state{
                symTable = ST.pushEmptyScope st,
                errors = newErrs ++ errs
                }

            io $ print guardStmnts
            newStmnts <- mapM checkStmnt guardStmnts
            newElse   <- checkStmnt csElse
            -- Get the new state and pop the context
            finalState <- get
            put finalState{
                    symTable = ST.popScope . symTable $ finalState
                }

            -- Return the resulting case statement
            return cs{
                D.caseExp    = newCaseExp,
                D.caseGuards = zip newLabels newStmnts,
                D.cbid       = newBid,
                D.caseElse     = newElse    
                    }

        checkStmnt fc@D.ProcCall{D.procName = s, D.procCallArgs = args, D.pcallPos = p} = do
            state@ContextState{symTable = st, errors = errs} <- get
            let 
                newArgs 
                    | null newErrs = rights . map (`cleanExpr` st) $ args
                    | otherwise = args
                --Check errors:
                newErrs = [ ContextError p err | err <- fromLeft [] (checkFun' s args st)]
                            

            unless (null newErrs) $ put state{errors = newErrs ++ errs}

            return fc{D.procCallArgs = newArgs}


        checkStmnt c@D.Continue{D.contPos = p} = do
            state@ContextState{errors = errs, analysisState = anSt} <- get
            put state{
                errors = [ContextError p LoopCntrlOOC | head anSt /= Loop] ++ errs
            }
            return c

        checkStmnt b@D.Break{D.bpos = p} = do
            state@ContextState{errors = errs, analysisState = anSt} <- get
            put state{
                errors = [ContextError p LoopCntrlOOC | head anSt /= Loop] ++ errs
            }
            return b

        checkStmnt x = return x

        

        
    
-------------------------------------------------------------------------------
-- < Aux functions >-----------------------------------------------------------

-- Return either a cleaned expression or a list of errors
cleanExpr :: D.Exp -> ST.SymbolTable -> Either [ErrClass] D.Exp
cleanExpr expr st = 
    let 
        errs    = checkExpr expr st
        cleaned = reduceExpr' expr st
        
        creduced = D.reduceConstant cleaned
        errs'
            | not (null errs) = errs
            | isNothing creduced = [DivideByZero]
            | otherwise = []
        ret 
            | null errs' = Right . fromJust $ creduced
            | otherwise  = Left errs'

    in ret

--Check an expression to find possible errors
checkExpr :: D.Exp -> ST.SymbolTable -> [ErrClass]
--check var reference
checkExpr D.IdExpr{D.idExpr = s} st = 
    let 
        sym  = ST.findSym s st
        sym' = fromJust sym
        notVarError = [UndeFunRef s | ST.isFunc sym' ||  ST.isProc sym']
        errs = case sym of 
                Nothing -> [UndefinedRef s]
                Just _  -> notVarError

    in  errs

--Check function call
checkExpr D.FunExpr{D.funExpId = s, D.funExpArgs = args, D.callPos = p} st = 
    let 
        newErrs = 
            case checkFun' s args st of
                Left errs -> errs
                Right sym -> [UndeProcRef s | ST.isProc sym]
    in newErrs

checkExpr D.BinaryOp{D.opert = o, D.oper1 = e1, D.oper2 = e2} st = 
    let
        lerrors = checkExpr e1 st
        rerrors = checkExpr e2 st
        ltype   =  getType e1 st
        rtype   = getType e2 st
        newErrs 
            | not (null lerrors) || not (null rerrors) = lerrors ++ rerrors
            | ltype /= rtype = [UnmatchTypesOper o ltype rtype]
            | o /= "=" && o /= "<>" && ltype /= opInType o = [UnmatchTypesOper o ltype rtype]
            | otherwise = []
    in newErrs

--check unary operators
checkExpr D.NumExpr{ D.numExpr = D.Op1{D.unOprn = expr, D.unOp = o }} st = 
    let
        expErrs = checkExpr expr st
        etype   = getType expr st
        newErrs 
            | not (null expErrs)  = expErrs
            | etype /= D.RealT = [ UnmatchTypesOper1 o etype ]
            | otherwise = [] 
    in newErrs

checkExpr D.NumExpr{} _ = []

--check Not operation
checkExpr D.BoolExpr{ D.boolExpr = D.Not{D.notExpr = expr}} st = 
    let
        expErrs = checkExpr expr st
        etype   = getType expr st
        newErrs 
            | not (null expErrs)  = expErrs
            | etype /= D.BooleanT = [ UnmatchTypesOper1 "not" etype]
            | otherwise = []

    in newErrs

checkExpr D.BoolExpr{} _ = []

-- aux function: checks if a symbol can be added.
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


-- aux function: checks the args in a function call
checkFun :: ST.Symbol -> [D.Exp] -> ST.SymbolTable -> [ErrClass]
checkFun sym@ST.Symbol{ST.symType = f@ST.Function{}} exps st = 
    let 
        fargs       = ST.funcArgs f
        fname       = ST.symId sym
        nargsMsg    = "Unmatching number of arguments."
        unvArgsMsg  = "Unvalid expressions in arguments."
        unvTypesMsg = "Unmatching arguments types."
        argsErrs    = foldl (\b a -> checkExpr a st ++ b ) [] exps
        argsTypes   = map snd fargs
        currArgsT   = map (`getType` st) exps
        typeMatch   = zipWith (==) argsTypes currArgsT
        errs
            | length exps /= length fargs = [UnvalidArgs fname nargsMsg]
            | not . null $ argsErrs       =  UnvalidArgs fname unvArgsMsg:argsErrs
            | not (and typeMatch)         = [UnvalidArgs fname unvTypesMsg]
            | otherwise                   = []

    in errs

checkFun ST.Symbol{ST.symId = s} _ _ = error $ 
    "error calling checkFun: this symbol is not a function: " ++ s

--aux function: give a function name, its formal parameters, and the symbol table, 
--return either a list of errors in the function call or the symbol refered by the name.
checkFun' :: String -> [D.Exp] -> ST.SymbolTable -> Either [ErrClass] ST.Symbol
checkFun' s args st =
    let 
        sym       
            | elem s (ST.funcStack st) = ST.findFunc s st
            | otherwise= ST.findSym s st 
        sym'      = fromJust sym
        
        isBuiltIn = S.member s D.builtInFuns
        argsErrs  = checkFun sym' args st
        newErrs 
            | isNothing sym && 
                not isBuiltIn        = [UndefinedRef s]
            | ST.isBoolVar sym' || ST.isRealVar sym'  = [UndeVarRef s]
            | not (null argsErrs)    =  argsErrs 
            | otherwise = []
        
        ret 
            | null newErrs = Right sym'
            | otherwise = Left newErrs
    in ret 


-- aux function: Return the return value of a valid exp
getType :: D.Exp -> ST.SymbolTable -> D.DataType
getType D.BoolExpr{} _ = D.BooleanT
getType D.NumExpr{}  _ = D.RealT

getType D.FunExpr{D.funExpId = s} st = 
    let 
        sym 
            | elem s (ST.funcStack st)  = ST.findFunc s st
            | otherwise = ST.findSym s st
            
        sym' = fromJust sym

        dtype
            | isNothing sym || not (ST.isFunc sym')  = 
                error $ "error in getType, this is not a valid function: " ++ s 
            | otherwise = ST.funcType . ST.symType $ sym'
    in dtype

getType D.IdExpr{D.idExpr = s} st = 
    let 
        sym  = ST.findSym s st
        sym' = fromJust sym

        dtype 
            | isNothing sym || ST.isFunc sym' || ST.isProc sym' =
                error $ "error in getType, this is not a valid variable: " ++ s 
            | ST.isBoolVar sym' = D.BooleanT
            | ST.isRealVar sym' = D.RealT
            | otherwise = error "error in getType"
    in dtype
    
getType D.BinaryOp{D.opert = o} _
    | o=="+" || o=="-" || o=="*" || o=="/" || o=="%" = D.RealT
    | otherwise = D.BooleanT


-- Aux function: Given a VALID expression, returns 
-- a simpler version of such expression with the types correctly
-- checked
reduceExpr :: D.Exp -> ST.SymbolTable -> Either D.BoolExp D.NumExp
reduceExpr bo@D.BinaryOp{D.opert = o, D.oper1 = e1, D.oper2 = e2} st
    | o == "=" || o == "<>"  = 
        let
            op1 = case reduceExpr e1 st of
                    Left be  -> D.BoolExpr be
                    Right ne -> D.NumExpr ne

            op2 = case reduceExpr e2 st of
                    Left be  -> D.BoolExpr be
                    Right ne -> D.NumExpr ne

            newExp = Left D.CompOp{
                        D.compOp   = o,
                        D.compArg1 = op1,
                        D.compArg2 = op2
                        }
        in newExp
            
    | opInType o == D.RealT = --input is real, output may be bool or real
        let 
            op1 = case reduceExpr e1 st of 
                    Left be  -> error "Error reducing expression: unmatching args types"
                    Right ne -> D.NumExpr ne
            
            op2 = case reduceExpr e2 st of 
                    Left be  -> error "Error reducing expression: unmatching args types"
                    Right ne -> D.NumExpr ne
            
            newExp 
                | opOutType o == D.RealT = 
                    Right D.Op2{
                        D.binOp = o, 
                        D.binOprn1 = op1,
                        D.binOprn2 = op2
                        } 
                | opOutType o == D.BooleanT =
                    Left D.RelOp{
                        D.relOp = o,
                        D.relOprn1 = op1,
                        D.relOprn2 = op2
                        }
                | otherwise = error "Unknown data type error in reduceExpr" 
        in newExp
    | opInType o == D.BooleanT = --input is bool, then output will be bool
        let 
            op1 = case reduceExpr e1 st of
                    Left be  -> D.BoolExpr be
                    Right ne -> error "Error reducing expression: unmatching args types"
            op2 = case reduceExpr e2 st of
                    Left be  -> D.BoolExpr be
                    Right ne -> error "Error reducing expression: unmatching args types"

            newExp = Left $ D.OpB o op1 op2

        in newExp
    | otherwise = error "Unknown error in reduce Expr"

reduceExpr D.IdExpr{D.idExpr = s} st = 
    let 
        sym  = ST.findSym s st
        sym' = fromJust sym
        newExp 
            | isNothing sym = error $ 
                "Error in reduceExpr, this is not a vallid ID: " ++ s
            | ST.isBoolVar sym' = Left (D.BoolVar s)
            | ST.isRealVar sym' = Right (D.NumVar s)
            | otherwise = error $ "error un reduceExpr: this is not a var: " ++ s 
    in newExp
    
reduceExpr D.FunExpr{D.funExpId = s, D.funExpArgs = args} st = 
    let 
        sym  
            | elem s (ST.funcStack st) = ST.findFunc s st
            | otherwise = ST.findSym s st
        sym' = fromJust sym
        ftype = ST.funcType . ST.symType $ sym'
        newArgs = rights . map (`cleanExpr` st) $ args
        newExp 
            | isNothing sym = error $ 
                "Error in reduceExpr, this is not a vallid ID: " ++ s
            | not (ST.isFunc sym') = error $
                "Error in reduceExpr, this is not a valid function: " ++ s
            | ftype == D.RealT = Right (D.NumFunCall s newArgs)
            | ftype == D.BooleanT = Left (D.BoolFunCall s newArgs)
    in newExp

reduceExpr (D.BoolExpr (D.Not e1)) st = 
    let
        newExp = case reduceExpr e1 st of
                    Left eb  -> Left . D.Not . D.BoolExpr $ eb
                    Right en -> error  
                        "Error in reduceExpr: not a valid bool exp in Not argument"

    in newExp

reduceExpr (D.BoolExpr expr) _ = Left expr

reduceExpr (D.NumExpr unop@(D.Op1 o e1)) st = 
    let
        newExp = case reduceExpr e1 st of
                    Left eb  -> error  
                        "Error in reduceExpr: not a valid real exp in unary op argument"
                    Right en -> Right unop{D.unOprn= D.NumExpr en}

    in newExp

reduceExpr (D.NumExpr expr) _ = Right expr
        

--Aux function to reduceExpr: unwrapps the result in a Exp type
reduceExpr' :: D.Exp -> ST.SymbolTable -> D.Exp
reduceExpr' expr st = case reduceExpr expr st of
                        Left be  -> D.BoolExpr be
                        Right ne -> D.NumExpr ne



--aux function: Returns the input type of an operator given its string representation
opInType :: String  -> D.DataType
opInType "+" = D.RealT
opInType "-" = D.RealT
opInType "*" = D.RealT
opInType "/" = D.RealT
opInType "%" = D.RealT
opInType "and" = D.BooleanT
opInType "or"  = D.BooleanT
opInType "not" = D.BooleanT
opInType "<="  = D.RealT
opInType ">="  = D.RealT
opInType "<"   = D.RealT
opInType ">"   = D.RealT
opInType o = error $ "error in opInType, couldn't check the input of " ++ o

--aux function: Returns the output type of an operator given its string representation
opOutType :: String -> D.DataType
opOutType "+" = D.RealT
opOutType "-" = D.RealT
opOutType "*" = D.RealT
opOutType "/" = D.RealT
opOutType "%" = D.RealT
opOutType "and" = D.BooleanT
opOutType "or"  = D.BooleanT
opOutType "not" = D.BooleanT
opOutType "<="  = D.BooleanT
opOutType ">="  = D.BooleanT
opOutType "<"   = D.BooleanT
opOutType ">"   = D.BooleanT
opOutType "="  = D.BooleanT
opOutType "<>"  = D.BooleanT
opOutType o = error $ "Error in opType, this is not an operator: " ++ o

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

    show (FunWOutReturn s) =
        "Function '" ++ s ++ "' does not have a return statement"

    show (UndefinedRef s) = 
        "Undefined symbol reference: '" ++ s ++ "'"

    show (UndeFunRef s) =
        "Unvalid reference to '" ++ s ++ "'. This is a function, not a variable."
    
    show (UndeProcRef s) =
        "Unvalid reference to '" ++ s ++ "'. This is a Procedure, it does not have return value."
    

    show (UndeVarRef s) =
        "Unvalid reference to '" ++ s ++ "'. This is a variable, not a function."
    
    show (UnvalidArgs s m) = 
        "Unvalid arguments for function '" ++ s ++ "'.\n" ++ m

    show (UnmatchTypesOper op t1 t2) = 
        "Unmatching types for operator '" ++ op ++ "' between " ++ show t1 ++
        " and " ++ show t2

    show (UnmatchTypesOper1 op t1 ) = 
        "Unmatching types for operator '" ++ op ++ "' and type " ++ show t1
    
    show (UnmatchTypesAssign s dt1 dt2) =
        "Unmatching types for assign operation between " ++
        "variable '" ++ s ++ "' of type " ++ show dt1 ++ 
        " and expression of type " ++ show dt2  

    show (CondNotBool p) =
        "Expression in conditional statement must be a boolean expression"

    show IteretNotNum =
        "Expressions in 'for' iterator must be a numeric expression"

    show DivideByZero = 
        "Error dividing by zero in numeric expression"

    show UnmatchingCaseTypes = 
        "Case statement labels must be of the same type of the expression"

    show CaseNotConstLabel =
        "Case statement labels must be constant expressions"
    show LoopCntrlOOC = 
        "'continue' or 'break' statement found out of loop"
    
    show (UnvLoopAssign s) = 
        "Illegal assignment to for-loop variable: " ++ s
-- Needed to use IO within State monad context
io :: IO a -> StateT ContextState IO a
io = liftIO  
