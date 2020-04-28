{-
    This module contains the functions required to evaluate a Pascal Statement
    or declaration. Every function in this module assumes that the program does 
    not have static errors.
-}
module Pascal.Interpret 
(
    interpret
)
where

-- < Useful imports > ---------------------------------------------------------
import Data.Maybe 
import Data.Fixed
import Data.Either
import Control.Monad
import Control.Monad.State
import qualified Data.List  as L
import qualified Data.Map   as M
import qualified Data.Set   as S
-------------------------------------------------------------------------------

-- < Pascal module Imports > --------------------------------------------------
import qualified Pascal.Data        as D
import qualified Pascal.SymbolTable as ST
-------------------------------------------------------------------------------

-- Note: haskell "error String"  statements are reserved for programming errors in 
-- the interpreter, not user errors
data ErrClass = DividingByZero
              | UnvalidFuncArgs{
                  unvArgFun :: String,  --function name
                  expErr :: [ErrClass]    --errors in the args
              } 
              | UnknownError deriving(Show,Eq)


-- Everytime an instruction is executed, this execution returns a
-- status with aditional information
data Status = Ok                    --Execution ended ok
            | Error{                --Execution ended with an error
                errPos  :: (Int,Int),
                errClass:: ErrClass --aditional data about the error
                }
            | Break                 --break the loop
            | Continue              --Execute next instruction
            | FuncReturn{           --Data returned by a function
                retVal :: ST.SymType
                }
            deriving(Show,Eq)

type RunState = ST.SymbolTable  --Just renaming the symbol table as the state object

type RetState a = StateT RunState IO a

interpret :: D.MainProgram -> IO Status
interpret (_,p) = interpret' p

interpret' :: D.Program -> IO Status
interpret' prog = do
    (status,state) <- runStateT (runProgram prog) ST.newTable
    print state
    return status


-- runProgram returns the resulting symbol table after running a program
runProgram :: D.Program -> RetState Status
runProgram D.Program{D.progDecl = pd, D.progInstrs = instrs} = do
    st <- get
    --Declare every symbol in the declarations
    let 
        newSt = foldl (\b a -> fromJust . (`ST.insertSym` b) . ST.createSym  $ a) st pd
    --Now that the symbols are declared, run the statements
    put newSt
    runInst instrs

    where 
        runInsts ::  [D.Statement] -> RetState Status
        runInsts [] = return Ok
        runInsts (inst:insts) = do
            newStat <- runInst inst

            case newStat of
                Continue  -> runInsts insts
                x         -> return x
                                    
        runInst :: D.Statement -> RetState Status
        runInst D.Break{} = return Break
        runInst D.Continue{} = return Ok
        runInst D.Block{D.blockInsts = insts} = runInsts insts

        --Run assign
        runInst D.Assign{D.assVarId = s, D.assVal = expr,D.assPos = p} = do
            val'<- eval expr
            st  <- get
            
            let
                currVal = ST.symType . fromJust . (`ST.findSym` st) $ s        
                ret 
                    | isLeft val' = Error p (fromLeft UnknownError val')
                    | otherwise   = Continue

            unless(isLeft val') $ put $ ST.setVal s (fromRight currVal val') st
            
            return ret

        --Run if 
        runInst D.If{D.ifCond = expr, D.succInst=si, D.failInst = fi, D.ipos=p} = do
            val' <- eval expr
            st   <- get
            
            case val' of
                Left err -> return $ Error p err
                Right (ST.BoolVar b) ->
                    do 
                        let 
                            inst
                                | b = si
                                | otherwise = fi
                        put $ ST.pushEmptyScope st
                        status <- runInst inst
                        resSt  <- get    
                        put $ ST.popScope resSt
                        case status of
                            err@Error{} -> return err
                            Break       -> return Break
                            Ok          -> return Ok
                            _           -> return Continue
                _ -> error "error in runInst: not a valid expression in if condition"

        -- run while             
        runInst w@D.While{D.whCond = expr, D.whBody = inst, D.wpos = p} = do
            val' <- eval expr
            st   <- get
            
            case val' of 
                Left err -> return $ Error p err
                Right (ST.BoolVar False) -> return Continue
                Right (ST.BoolVar True)  ->
                    do
                        put $ ST.pushEmptyScope st
                        status <- runInst inst
                        resSt  <- get
                        put $ ST.popScope resSt

                        case status of
                            err@Error{} -> return err
                            Break       -> return Continue
                            _           -> runInst w
                _ -> error "error in runInst: not a valid expression in while condition"
        -- run
        runInst D.For{D.forIter = s, D.forInitVal = expr1, D.forEndVal = expr2, 
                      D.forBody = fbody, D.forIncr = incr, D.fpos = p} = do
            --Evaluate both expressions
            val1' <- eval expr1
            val2' <- eval expr2
            --If one of them failed, then return an error
            if isLeft val1'
                then return $ Error p (fromLeft UnknownError val1')
            else if isLeft val2'
                then return $ Error p (fromLeft UnknownError val2')
            --Otherwise, run a while statement built from the For conditions
            else do
                let 
                    op = case incr of
                            "to"     -> "+"
                            "downto" -> "-" 
                            b        -> error $ "unvalid operation in for statement: " ++ b
                    rel = case incr of
                            "to"     -> "<="
                            "downto" -> ">=" 
                            b        -> error $ "unvalid operation in for statement: " ++ b
                    --We build an expression like "i + 1"
                    operation = D.NumExpr $ D.Op2 op (D.NumExpr . D.NumVar $ s) (D.NumExpr . D.NumConst $ 1) 
                    -- i := init value
                    initAssign = D.Assign{
                                        D.assVarId = s, 
                                        D.assVal = D.NumExpr . D.NumConst . ST.rval . fromRight' $ val1',
                                        D.assPos = p
                                        }
                    -- i := i + 1 
                    incrAssing = D.Assign{
                                        D.assVarId = s, 
                                        D.assVal = operation,
                                        D.assPos = p
                                        }
                    -- Condition to check at every iteration
                    condition = D.BoolExpr $ 
                                    D.RelOp rel (D.NumExpr . D.NumVar $ s) 
                                    (D.NumExpr . D.NumConst . ST.rval . fromRight' $ val2')
                    -- We are going to simulate a while statement
                    loop = do
                        check <- eval condition
                        case check of
                            Left err -> return $ Error p err
                            Right (ST.BoolVar True) -> 
                                do 
                                    status <- runInst fbody
                                    case status of
                                        err@Error{} -> return err
                                        Break       -> return Continue
                                        _           -> do 
                                                        runInst incrAssing
                                                        loop
                            Right (ST.BoolVar False) -> return Continue
                --Init iterator & run the loop
                status <- runInst initAssign
                case status of 
                    err@Error{} -> return err
                    _           -> do
                        st <- get
                        put $ ST.pushEmptyScope st
                        newStatus <- loop
                        resSt <- get
                        put $ ST.popScope resSt
                        return newStatus 
                 
        --Run case 
        runInst D.Case{D.caseExp = expr, D.caseGuards = guards, D.caseElse = celse, D.cpos = p} = do
            val' <- eval expr 
            case val' of 
                Left err -> return $ Error p err
                Right symt -> do
                    guards' <- mapM (eval . fst)  guards
                    let
                        match = [ t | (s,t) <- zip (rights guards') (map snd guards), s==symt ]
                        inst
                            | null match = celse
                            | otherwise = head match
                        
                    st <- get
                    put $ ST.pushEmptyScope st
                    status <- runInst inst
                    resSt <- get
                    put $ ST.popScope resSt
                    return status
        
        runInst D.ProcCall{D.procName = s, D.procCallArgs = args, D.pcallPos = p} = do
            status <- runFunc' s args
            case status of 
                err@Error{} -> return err{errPos = p}
                x           -> return Continue
                        

        ----
        runInst _ = return Continue
--Utility: run a function with the given  name
runFunc' :: String -> [D.Exp] -> RetState Status
runFunc' s args = do
    st <- get
    let 
        sym' 
            | s `elem`  (ST.funcStack st) =  ST.findFunc s st
            | otherwise = ST.findSym s st

    case sym' of
        Nothing  -> error $ "Error in runFunc': this is not a valid function: " ++ s
        Just sym -> runFunc sym args

runFunc :: ST.Symbol -> [D.Exp] -> RetState Status
runFunc sym@ST.Symbol{ST.symType=f@ST.Function{}} exprs = do
    st <- get
    symVals'' <- mapM eval exprs           --Possible sym values
    let 
        -- Symbol Data:
        fbody = ST.funcBody f -- function program
        ftype = ST.funcType f -- function output type
        fargs = ST.funcArgs f -- [(string, type)]: name and type
        fname = ST.symId sym  -- function name
        fpos  = ST.symPos sym -- Function declaration position

        -- Create the aditional variables for the function 
        declars' = [D.Variable s t fpos | (s,t) <- fargs]
        declars 
            | ftype /= D.NoneT = D.Variable fname ftype fpos : declars'
            | otherwise = declars'
        symVals' = rights symVals''
        symVals 
            | ftype == D.RealT = ST.RealVar 0:symVals'
            | ftype == D.BooleanT = ST.BoolVar False:symVals'
            | otherwise = symVals'

        argErrs = lefts symVals''

        newSyms' = map ST.createSym declars
        newSyms  = zipWith (\s t -> s{ST.symType=t}) newSyms' symVals -- Set the initial values of the args
        
        -- New st: we have to push a new scope and declare 
        -- the function args, and maybe an extra variable with 
        -- the name of the function in case this function requires a 
        -- return type
        -- push an empty scope where all the function variables 
        -- will be declared. We have to add this function to the funcStack 
        st' = ST.pushFunc fname . ST.pushEmptyScope $ st
        newSt = foldl (\b a -> fromJust $ ST.insertSym a b) st' newSyms

    
    if null argErrs 
        then do 
            put newSt
            funStatus <- runProgram fbody
            resSt     <- get
            put $ ST.popFunc . ST.popScope $ resSt

            case funStatus of
                err@(Error _ _) -> return  err
                Ok              ->  if ftype == D.NoneT
                                        then return  Ok
                                        else return $ FuncReturn (ST.symType . fromJust . (`ST.findSym` resSt) $ fname)
                x -> error $ "undefined program return status in funEval: " ++ show x
                    

        else return $ Error (0,0) (UnvalidFuncArgs fname argErrs)
        
            

runFunc s _ = error $ "Not a valid function symbol type " ++ show s

--Eval returns a SymType with the values, or error if some error is found
eval :: D.Exp -> RetState (Either ErrClass ST.SymType)
eval D.NumExpr{D.numExpr = expr} = do

    ev <- evalNumExp expr
    case ev  of
        Left x    -> return $ Left x
        Right f   -> return $ Right ST.RealVar{ST.rval=f}

eval D.BoolExpr{D.boolExpr = expr} = do
    ev <- evalBoolExp expr 
    case ev of
        Left x     -> return $ Left x
        Right f    -> return $ Right ST.BoolVar{ST.bval=f}


--Evaluate returns either an ErrClass describing some kind of error, or a constant value:
evalNumExp :: D.NumExp -> RetState (Either ErrClass Float)
evalNumExp D.NumConst{D.numVal = v} = return $ Right v
evalNumExp D.NumVar{D.numVarId = s} = do
    st <- get
    let 
        sym' = ST.findSym s st 
        sym  = fromJust sym'

        ret  
            | isNothing sym' = error $ "error in evalNumExp: This symbol does not exists: " ++ s
            |      ST.isFunc sym 
                || ST.isProc sym
                || ST.isBoolVar sym = error $ "error in evalNumExp: unmatching type of symbol in: " ++ s
            | otherwise = Right . ST.rval . ST.symType $ sym

    return ret

evalNumExp D.Op2{D.binOp = o, D.binOprn1 = opr1, D.binOprn2 = opr2} = do
    st <- get
    val1' <- evalNumExp . D.numExpr $ opr1
    val2' <- evalNumExp . D.numExpr $ opr2
    let 
        val1  = fromRight 0 val1'
        val2  = fromRight 0 val2'
        op = case o of
                "+" -> (+)
                "-" -> (-)
                "*" -> (*)
                "/" -> (/)
                "%" -> mod'
        ret
            | isLeft val1' =  val1'
            | isLeft val2' =  val2'
            | (o=="/" || o=="%") && val2==0 
                           = Left DividingByZero
            | otherwise    = Right $ op val1 val2

    return  ret

evalNumExp D.Op1{D.unOp = o, D.unOprn = opr} = do
    st   <- get
    val' <- evalNumExp . D.numExpr $ opr
    let
        val  = fromRight 0 val'

        op = D.strToUnOp o

        ret 
            | isLeft val' = val'
            | otherwise   = Right $ op val
            
    return  ret

evalNumExp D.NumFunCall{D.numFuncId = s, D.numFunArgs = exprs} = do
    st  <- get 
    ret <- runFunc' s exprs
    case ret of 
        Ok  -> error $ "error in evalNumExp: this is a procedure, not a function: " ++ s
        FuncReturn (ST.RealVar v) -> return $  Right v
        Error{errClass = err } -> return $ Left err  
        _   -> error "error in evalNumExp: unknown function return"
        
evalBoolExp :: D.BoolExp -> RetState(Either ErrClass Bool)
evalBoolExp D.TrueC  = return $ Right True
evalBoolExp D.FalseC = return $ Right False

evalBoolExp D.BoolVar{D.boolVarId = s} = do
    st <- get

    let 
        sym' = ST.findSym s st
        sym  = fromJust sym'
        ret 
            | isNothing sym' = error $ "Error in evalBoolExp: this is not a valid variable: " ++ s
            | not (ST.isBoolVar sym) = error $ "Error in evalBoolExp: this is not a Boolean variable: " ++ s
            | otherwise = Right . ST.bval . ST.symType $ sym

    return ret

evalBoolExp D.BoolFunCall{D.boolFuncId = s, D.boolFunArgs = exprs} = do
    st <- get
    res <- runFunc' s exprs
    case res of
        Ok                  -> error $ "Error in evalBoolExp: this is not a boolean function: " ++ s
        Error{errClass=err} -> return $ Left err
        FuncReturn (ST.BoolVar b) -> return $ Right b
        _                         -> error "Error in evalBoolExp: not a valid function in boolean expression"

evalBoolExp D.Not{D.notExpr = expr} = do
    ret <- eval expr
    case ret of
        Left err             -> return $ Left err
        Right (ST.BoolVar b) -> return $ Right (not b)
        Right _              -> error "error in evalBoolExp: this is not a valid expression"

evalBoolExp D.RelOp{D.relOp = o, D.relOprn1 = opr1, D.relOprn2 = opr2 } = do
    val1' <- eval opr1
    val2' <- eval opr2
    let
        val1 = case val1' of
                Right (ST.RealVar f1) -> f1
                Right _               -> error "Error in evalBoolExp: this is not a valid expression"
                _                     -> 0
        val2 = case val2' of
                Right (ST.RealVar f2) -> f2
                Right _               -> error "Error in evalBoolExp: this is not a valid expression"
                _                     -> 0
        op = D.strToRelOp o
        ret
            | isLeft val1' = Left $ fromLeft UnknownError val1'
            | isLeft val2' = Left $ fromLeft UnknownError val2'
            | otherwise    = Right $ op val1 val2
            
    return ret

evalBoolExp D.CompOp{D.compOp = o, D.compArg1 = opr1, D.compArg2 = opr2 } = do
    val1' <- eval opr1
    val2' <- eval opr2
    let
        val1 = case val1' of
                Right f1 -> f1
                _        -> error "Error in evalBoolExp: Incorrect use of Left value of 'eval'"
        val2 = case val2' of
                Right f2 -> f2
                _        -> error "Error in evalBoolExp: Incorrect use of Left value of 'eval'"
        op = D.strToCompOp o
        ret
            | isLeft val1' = Left $ fromLeft UnknownError val1'
            | isLeft val2' = Left $ fromLeft UnknownError val2'
            | otherwise    = Right $ op val1 val2
            
    return ret

evalBoolExp D.OpB{D.boolOp = o, D.boolOprn1 = opr1, D.boolOprn2 = opr2} = do
    --We will check the first operand value. We will try to not check the second one.
    val1' <- eval opr1
    
    let 
        
        op = D.strToBoolOp o

    case val1' of 
        Left err -> return $ Left err
        Right (ST.BoolVar b) -> 
            if (b && o=="or") || (not b && o=="and") 
                then return $ Right b 
                else do
                    val2' <- eval opr2
                    case val2' of
                        Left err -> return $ Left err
                        Right (ST.BoolVar b') -> return . Right $ op b b'
                        Right _ -> error "Error in evalBoolExp: this is not a valid booleam expression"
        Right _ -> error "Error in evalBoolExp: this is not a valid booleam expression"
    

-- Aux function: get the right value from an Either a b,
-- but it may cause an error if the arg is left
fromLeft' :: Either a b -> a
fromLeft' (Left x) = x
fromLeft' _ = error "Error in fromLeft': Cannot get Left value from a Right value"

fromRight' :: Either a b -> b
fromRight' (Right x) = x
fromRight' _ = error "Error in fromRight': Cannot get Right value from a Left value"

io :: IO a -> StateT RunState IO a
io = liftIO 