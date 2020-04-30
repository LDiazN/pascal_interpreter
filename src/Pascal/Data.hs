-- This file contains the data-structures for the AST
-- The role of the parser is to build the AST (Abstract Syntax Tree) 

module Pascal.Data
    (
        NumExp(..),
        BoolExp(..),
        Exp(..),
        Statement(..),
        Declaration(..),
        Program(..),
        DataType(..),
        MainProgram,
        printMainProg,
        builtInFuns,
        ioFuns,
        numFuns,
        reduceConstant,
        isConstant,
        strToBoolOp,
        strToCompOp,
        strToRelOp,
        strToUnOp,
        inputFun,
        outputFun
    ) where

---------------------------------------------------------------------------------
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.Fixed
---------------------------------------------------------------------------------


-- Data-structure for generic expressions
data Exp = BoolExpr { boolExpr :: BoolExp } --Boolean expression
         | NumExpr  { numExpr :: NumExp }   --Numeric expresion
         | FunExpr  { funExpId :: String, funExpArgs :: [Exp], callPos :: (Int,Int) } --Function call
         | IdExpr   { idExpr :: String, refPos :: (Int,Int) } --Simple id
         | BinaryOp { opert :: String, oper1 :: Exp, oper2 :: Exp }
           deriving(Eq)

data NumExp = Op1{                --Unary numeric operation
                unOp  :: String,  --Unary Operator
                unOprn :: Exp     --argument expression
                }
            | Op2{                  --Binary numeric operation
                binOp :: String,    --Binary Operator
                binOprn1 :: Exp,    --left argument 
                binOprn2 :: Exp     --right argument
                }
            | NumFunCall{            --Numeric function call
                numFuncId :: String, --Function name
                numFunArgs :: [Exp]  --Functions args  
                }
            | NumConst{              -- A constant value
                numVal :: Float      -- Actual value (e.g. 10.0)
                }
            | NumVar{               -- A variable number
                numVarId :: String  -- variable name (e.g. Var x)
                }
            deriving(Eq)
                

-- Data-structure for boolean expressions
data BoolExp = 
              OpB{                   --Binary Boolean operation
                boolOp :: String,    --Binary Operator
                boolOprn1 :: Exp,    --left argument 
                boolOprn2 :: Exp     --right argument
                }
            | Not{
                notExpr :: Exp --Negated expresion
                }
            | BoolFunCall{            --Boolean function call
                boolFuncId :: String, --Function name
                boolFunArgs :: [Exp]  --Functions args  
                }
            | TrueC                  -- True constant
            | FalseC                 -- False constant
            | RelOp{  --Relational operation
                relOp :: String,
                relOprn1 :: Exp,
                relOprn2 :: Exp
                }
            | CompOp{
                compOp   :: String, -- = or <>
                compArg1 :: Exp,  
                compArg2 :: Exp
                }
            | BoolVar{               -- A variable number
                boolVarId :: String  -- variable name (e.g. Var x)
                }
            deriving(Eq)

-- Enum for the supported built in types
data DataType   = RealT 
                | BooleanT 
                | NoneT 
                deriving(Show,Eq,Enum)

--Symbol declaration
data Declaration =  Variable{                 --Variable declaration
                        varId :: String,      --Variable name
                        varType :: DataType,  --Variable type
                        vDeclpos:: (Int, Int) --Declaration position
                        }  
                 |  Function{
                        funcId :: String,               --function name
                        funcArgs :: [(String,DataType)],--function args
                        funcType :: DataType,    -- function return type    
                        funcBody :: Program,     -- function code
                        fDeclPos :: (Int, Int)   --Declaration position
                        }
                deriving(Eq)
                

-- Data-structure for statements
data Statement = Assign{                -- Variable assignment
                    assVarId :: String, -- Variable name
                    assVal   :: Exp,    -- Variable Value
                    assPos   :: (Int,Int) -- statement position
                    } 
                -- If statement
                | If {
                    ifCond :: Exp,        -- condition that must be true
                    succInst :: Statement,-- Instruction executed when cond=true
                    failInst :: Statement,-- Executed when cond = false 
                    ibid     :: Int,      -- if bid
                    ipos     :: (Int,Int)
                    }
                -- Case statement
                | Case{
                    caseExp    :: Exp,       --Expresion to compare 
                    -- Each of the expresion-statement guards to be
                    -- evaluated:
                    caseGuards :: [(Exp, Statement)],
                    caseElse   :: Statement, --Default case
                    cbid       :: Int,
                    cpos       :: (Int,Int)  --Case position
                    }
                -- For loop
                | For{
                    forIter     :: String,     --For iterator name
                    forInitVal  :: Exp,        --Iterator init valye
                    forEndVal   :: Exp,        --Iterator final value
                    forBody     :: Statement,  --For instructions
                    forIncr     :: String,     --Increase iterator 
                    fbid        :: Int,        --For block id
                    fpos        :: (Int,Int)   -- Position in the file
                    }
                -- While loop
                | While{
                    whCond :: Exp,          --While condition 
                    whBody :: Statement,    --while instructions
                    wbid   :: Int,          --While block id 
                    wpos   :: (Int,Int)     --While calling pos. in the file
                    }
                -- Function & procedure calls
                | ProcCall{
                    procName :: String,
                    procCallArgs :: [Exp],
                    pcallPos :: (Int, Int)
                    }
                -- Block
                | Block {   --Instruction block
                    blockInsts :: [Statement]
                    } 
                | Break{
                    bpos :: (Int,Int) --break position
                    } 
                | Continue{
                    contPos :: (Int,Int) --contienue position
                    }
                | Skip
                deriving (Eq)

-- Data-structure for whole program
-- Hint: make a tuple containing the other ingredients
data Program = Program{
                                            --It must be a Block Statement
                progInstrs :: Statement,    --Set of statements to execute. 
                progDecl   :: [Declaration] -- Variables declared for this 
                                            -- program. Note that since
                                            -- Pascal doesn't allow declaring
                                            -- variables in the program body,
                                            -- declariations are independent on
                                            -- the program statements
            }
            deriving(Eq)

type MainProgram = (String,Program)

--I/O with stdin/stdout functions:
inputFun :: String
inputFun = "readln"
outputFun :: String
outputFun = "writeln"

--Set of built-in functions
builtInFuns :: S.Set String
builtInFuns = S.fromList [
                            inputFun,
                            outputFun,
                            "sin",
                            "cos",
                            "sqrt",
                            "exp",
                            "ln"
                        ]
-- Set of io functions
ioFuns :: S.Set String
ioFuns = S.fromList [
                    inputFun,
                    outputFun
                    ]

-- set of numeric functions:
numFuns :: M.Map String (Float -> Float)
numFuns = M.fromList [
            ("sin", sin),
            ("cos", cos),
            ("sqrt", sqrt),
            ("exp", exp),
            ("ln", log)
            ]

------------------------------------------------------------
-- < Utility functions for the AST > -----------------------

strToRelOp :: String -> Float -> Float -> Bool
strToRelOp o = 
    case o of 
        "<"  -> (<)
        "<=" -> (<=)
        ">"  -> (>)
        ">=" -> (>=)
        _    -> error $ "Error in strToRelOp: This is not a relational operator: " ++ o
        
strToUnOp :: String -> Float -> Float
strToUnOp o = 
    case o of 
        "+" -> (0+)
        "-" -> (0-)
        _   -> error $ "Error in strToUnOp: This is not an unary operator: " ++ o

strToBoolOp :: String -> Bool -> Bool -> Bool
strToBoolOp o = 
    case o of 
        "and" -> (&&)
        "or"  -> (||)
        _     -> error $ "Error in strToBoolOp: This is not a boolean operator: " ++ o

strToCompOp :: Eq a => String -> a -> a -> Bool
strToCompOp o =
    case o of 
        "="  -> (==)
        "<>" -> (/=)
        _    -> error $ "Error in strToCompOp: This is not a boolean operator: " ++ o

strToBinOp :: String -> Float -> Float -> Float
strToBinOp o = 
    case o of 
        "+" -> (+)
        "-" -> (-)
        "*" -> (*)
        "/" -> (/)
        "mod" -> mod'
        _   -> error $ "Error in strToBinOp: This is not an binary operator: " ++ o


-- Aux function: Checks if an Exp is a constant, bool or real
isConstant :: Exp -> Bool
isConstant e = isNumConstant' e  || isBoolConstant' e

-- Aux function: checks if a NumExp is a constant
isNumConstant :: NumExp -> Bool
isNumConstant NumConst{} = True
isNumConstant _ = False

--Like isNumConstant, but with NumExpr wrapped in a generic exp
isNumConstant' :: Exp -> Bool
isNumConstant' (NumExpr expr) = isNumConstant expr
isNumConstant' _ = False

-- Aux function: checks if a BoolExp is a constant
isBoolConstant :: BoolExp -> Bool
isBoolConstant TrueC  = True
isBoolConstant FalseC = True
isBoolConstant _ = False

--Like isBoolConstant, but with NumExpr wrapped in a generic exp
isBoolConstant' :: Exp -> Bool
isBoolConstant' (BoolExpr expr) = isBoolConstant expr
isBoolConstant' _ = False


--Return a bool from a bool constant
etoBool :: BoolExp -> Bool
etoBool TrueC = True
etoBool FalseC = False
etoBool _ = error "This is not a bool constant"

--Return an exp from a bool val
btoExp :: Bool -> BoolExp
btoExp True = TrueC
btoExp False = FalseC

-- Aux function: returns a reduced expression
reduceConstant :: Exp -> Maybe Exp
reduceConstant (NumExpr ne) = (Just . NumExpr) =<< reduceConstantNum ne 

reduceConstant (BoolExpr be) = (Just . BoolExpr) =<< reduceConstantBool be 

reduceConstant _ = error "Incorrect use of reduceConstant"

-- Aux function: Given a boolean expressionm reduce as much
-- as possible.  If some error is found, return Nothing
reduceConstantBool :: BoolExp -> Maybe BoolExp
reduceConstantBool (Not (BoolExpr expr)) = 
    
    let 

        redExp = reduceConstantBool expr

        newExp 
            | isNothing redExp = Nothing
            | otherwise = case expr of 
                            TrueC  -> Just FalseC
                            FalseC -> Just TrueC
                            _      -> Just . fromJust $ redExp

    in newExp
    
reduceConstantBool Not{} = error "Not a valid Bool Exp in reduceConstantBool"

reduceConstantBool be@(OpB o op1 op2) = 
    let
        newOp1 = case op1 of
                    BoolExpr be'-> be'
                    _           -> error "Error in reduceConstantBool, this not a bool expr"
        newOp2 = case op2 of
                    BoolExpr be'-> be'
                    _           -> error "Error in reduceConstantBool, this not a bool expr"
        
        newOp1' = reduceConstantBool newOp1
        newOp2' = reduceConstantBool newOp2

        newOp1'' = fromJust newOp1'
        newOp2'' = fromJust newOp2'

        op = strToBoolOp o

        newExp 
            | isNothing newOp1' || isNothing newOp2' = Nothing
            | isBoolConstant newOp1'' && isBoolConstant newOp2'' =
                Just . btoExp $ op  (etoBool newOp1'' ) (etoBool newOp2'')
            | otherwise = Just be{boolOprn1 = BoolExpr newOp1'', boolOprn2 = BoolExpr newOp2''}
    in newExp

reduceConstantBool be@(CompOp o (NumExpr ne1) (NumExpr ne2)) =
    let 
        op = strToCompOp o

        newOp1 = reduceConstantNum ne1
        newOp2 = reduceConstantNum ne2

        newOp1' = fromJust newOp1
        newOp2' = fromJust newOp2

        newExp
            | isNothing newOp1 || isNothing newOp2 = Nothing
            | isNumConstant newOp1' && isNumConstant newOp2' = 
                Just . btoExp $ op (numVal newOp1')  (numVal newOp2')
            | otherwise = Just be{compArg1 = NumExpr newOp1', compArg2 = NumExpr newOp2'}
    in newExp

reduceConstantBool be@(CompOp o (BoolExpr be1) (BoolExpr be2)) =
    let 
        op = strToCompOp o

        newOp1 = reduceConstantBool be1
        newOp2 = reduceConstantBool be2

        newOp1' = fromJust newOp1
        newOp2' = fromJust newOp2

        newExp
            | isNothing newOp1 || isNothing newOp2 = Nothing
            | isBoolConstant newOp1' && isBoolConstant newOp2' = 
                Just . btoExp $ op (etoBool newOp1')  (etoBool newOp2')
            | otherwise = Just be{compArg1 = BoolExpr newOp1', compArg2 =BoolExpr newOp2'}
    in newExp

reduceConstantBool CompOp{} = error 
    "error in reduceConstantBool: Unvalid comparision between different types"

reduceConstantBool be@(RelOp o (NumExpr ne1) (NumExpr ne2)) = 
    let 
        op = strToRelOp o


        newOp1 = reduceConstantNum ne1
        newOp2 = reduceConstantNum ne2

        newOp1' = fromJust newOp1
        newOp2' = fromJust newOp2

        newExp
            | isNothing newOp1 || isNothing newOp2 = Nothing
            | isNumConstant newOp1' && isNumConstant newOp2' = 
                Just . btoExp $ op (numVal newOp1')  (numVal newOp2')
            | otherwise = Just be{relOprn1 = NumExpr newOp1', relOprn2 = NumExpr newOp2'}
    in newExp

reduceConstantBool RelOp{} = error
    "error in reduceConstantBool: unvalid relational expression between non-Real expressions"

--Else
reduceConstantBool x = Just x

-- Aux function: given a numeric expression, reduce as much as
-- possible all the constant sub expressions. If some error is 
-- found (dividing by 0, for example) return Nothing 
reduceConstantNum :: NumExp -> Maybe NumExp
reduceConstantNum op@Op1{unOp = o, unOprn = expr'} 
    | o /= "+" && o /= "-" = error $ "Unvalid unary operator: " ++ o
    | otherwise = newExp 
    where 
        expr = case expr' of 
                NumExpr a -> a
                _         -> error "error in reduceConstantNum, not a num exp"
        redExp   = reduceConstantNum expr
        redExp'  = fromJust redExp
        constant = numVal redExp'
        oper = strToUnOp o
        newExp 
            | isNothing redExp = Nothing
            | isNumConstant redExp' = Just . NumConst . oper $ constant
            | otherwise = Just op{unOprn = NumExpr redExp'}


reduceConstantNum op@Op2{binOp = o,binOprn1 = opr1' , binOprn2 = opr2'} =
    let 
        --General data
        opr1     = case opr1' of
                    NumExpr a -> a
                    _         -> error "Error in reduceConstantNum, not a num exp"
        opr2     = case opr2' of
                    NumExpr a -> a
                    _         -> error "Error in reduceConstantNum, not a num exp"                            
        newOp1   = reduceConstantNum opr1 
        newOp2   = reduceConstantNum opr2
        newOp1'  = fromJust newOp1
        newOp2'  = fromJust newOp2
        oper2Val = numVal newOp2' 
        oper1Val = numVal newOp1'

        --In case of addition or multiplication:
        oper  =  strToBinOp o
        
        --In case of division:          
        newExp   
            | isNumConstant newOp2' && oper2Val == 0 = Nothing
            | isNumConstant newOp2' && isNumConstant newOp1' =
                Just . NumConst $ oper oper1Val oper2Val
            | otherwise = Just op{binOprn1 = NumExpr newOp1', binOprn2 = NumExpr newOp2'}

        --Any other operation: 
        newExp' 
            | isNothing newOp1 || isNothing newOp2 = Nothing
            | o == "/" || o=="%" = newExp
            | isNumConstant newOp1' && isNumConstant newOp2' =
                Just . NumConst $ oper oper1Val oper2Val
            | otherwise = Just op{binOprn1 = NumExpr newOp1', binOprn2 = NumExpr newOp2'}

    in newExp'



reduceConstantNum ne = Just ne


------------------------------------------------------------
-- < Helper functions to print program > -------------------

instance Show Declaration where
    show (Variable s d _) = "[Var] " ++ s ++ " : " ++ show d

    show f@Function{}   = funKind ++ fname ++ 
                          "(" ++ fargs ++ ")" ++ ftype ++
                          fbody
        where 
            fname  = funcId f
            fargs' = map (\(a , b) -> a ++ " : " ++ show b ++ "; ") . funcArgs $ f
            fargs  = concat fargs'
            ftype' = funcType f
            ftype  = case ftype' of 
                        NoneT -> ""
                        _     -> " : " ++ show ftype' 
            fbody' = funcBody f
            fbody  = unlines . map ("  "++) . lines . show $ fbody'
            funKind= case ftype' of 
                        NoneT -> "[Procedure] "
                        _     -> "[Function] "
    
    

instance Show Statement where
    show b@Block{} = "BEGIN\n" ++ stmts ++ "END\n"
        where 
            stmts' = blockInsts b
            stmts  =  unlines . map ("  "++) . lines . concatMap show $ stmts'
    show (Assign vid expr _) = vid ++ " := " ++  show expr ++ "\n"

    show mif@If{}  = "IF (" ++ expr ++ ") " ++ "[ BID: " ++ bid ++ " ]\n" ++
                     sccbody ++ "ELSE: " ++ fbody   
        where 
            bid = show . ibid $ mif 
            expr = show . ifCond $ mif
            sccbody = printBody . succInst $ mif
            fbody = case failInst mif of
                        Skip -> "  [NO ELSE STATEMENT]\n"
                        stm  -> printBody stm

            printBody :: Statement -> String
            printBody = unlines . map ("  "++) . lines . show  

    show f@For{}   = "FOR " ++ fvar ++ ":= " ++ expr1 ++ " " ++ how ++ " " ++ expr2 ++ " DO " ++
                     "[ BID: " ++ bid ++ "]\n" ++ fbody ++ "[END FOR]\n"
        where 
            fvar  = forIter f
            expr1 = show . forInitVal $ f
            expr2 = show . forEndVal $ f
            how   = forIncr f
            bid   = show . fbid $ f
            fbody = unlines . map ("  "++) . lines . show . forBody $ f
    
    show w@While{} = "WHILE " ++ expr ++ " DO [ BID: " ++ bid ++ " ] \n" ++
                     wbody
        where 
            expr = show . whCond $ w
            bid  = show . wbid $ w
            wbody = unlines . map ("  "++) . lines . show . whBody $ w
        
    show p@ProcCall{} = "[FUN/PROC CALL] " ++ pname ++ "( " ++ pargs ++ ")\n"
        where 
            pname = procName p
            pargs = concatMap ((++ "; ") . show) . procCallArgs $ p
    
    show c@Case{}    = "CASE " ++ cexpr ++ " : [ BID: " ++ bid ++ " ]\n" ++ 
                       cases ++ "END\n"
        where 
            cexpr = show . caseExp $ c
            bid  = show . cbid $ c
            cases' = caseGuards c
            cases  = unlines . map (("  "++) . (\(a,b) -> show a ++ " : " ++ show b)) $ cases'
    
    show Break{} = "BREAK\n"
    show Continue{} = "CONTINUE\n"
    show Skip = ""

instance Show Program where
    show (Program pins pdec) = strdec ++ "\n" ++ show pins
        where 
            strdec = unlines . map show $ pdec

instance Show Exp where
    show BoolExpr{ boolExpr = b} = show b
    show NumExpr{numExpr = n} = show n
    show FunExpr{funExpId=feid, funExpArgs = args} = 
        feid ++ "(" ++ args' ++ ")"
        where 
            args' = concatMap ((++ "; ") . show ) args
    show IdExpr{idExpr = expid} = "[var]" ++ expid
    show BinaryOp{opert = o, oper1 = op1, oper2 = op2} = "( " ++ o ++ " (" ++
                                show op1 ++") ("++ show op2 ++ ") )"

instance Show NumExp where
    show Op1{ unOp = o, unOprn = e } = "( " ++ o ++ " " ++ show e ++ " )"
    show Op2{ binOp = o, binOprn1 = op1, binOprn2 = op2 } = "( " ++ o ++ 
                    "(" ++show op1++") (" ++ show op2 ++ "))"
    show (NumConst n) = show n
    show (NumVar s)  = s
    show (NumFunCall s a) = s ++ "( " ++ args ++ ")"
        where 
            args = concatMap ((++ "; ") . show) a

instance Show BoolExp where
    show OpB{boolOp = o, boolOprn1 = op1, boolOprn2 = op2} = "( " ++ o ++ 
                            " (" ++ show op1 ++ ") (" ++ show op2 ++ ")"
    show Not{notExpr = ne} = "NOT (" ++ show ne ++ ")"
    show BoolFunCall{boolFuncId = bfid, boolFunArgs = args} = bfid ++ "(" ++ args' ++ ")"
        where args' = concatMap ((++ "; ") . show) args

    show TrueC = "true"
    show FalseC = "false"
    show BoolVar{boolVarId = bvid} = bvid
    show RelOp{relOp = o, relOprn1 = op1, relOprn2 = op2} = "( " ++ o ++ 
                            " (" ++ show op1 ++ ") (" ++ show op2 ++ "))"
    show CompOp{compOp = o, compArg1 = op1, compArg2 = op2} = "( " ++ o ++ 
                            " (" ++ show op1 ++ ") (" ++ show op2 ++ ")"

printMainProg :: MainProgram -> String
printMainProg (s, p) = "[PROGRAM: " ++ s ++ "]\n" ++ show p