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
        printMainProg
    ) where

-- Data-structure for  numeric expressions
data Exp = BoolExpr { boolExpr :: BoolExp } --Boolean expression
         | NumExpr  { numExpr :: NumExp }   --Numeric expresion
         | FunExpr  { funExpId :: String, funExpArgs :: [Exp] } --Function call
         | IdExpr   { idExpr :: String } --Simple id
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
                boolOprn1 :: Exp,--left argument 
                boolOprn2 :: Exp --right argument
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

data Declaration = --Symbol declaration
                    Variable{                 --Variable declaration
                        varId :: String,      --Variable name
                        varType :: DataType   --Variable type
                        }  
                 |  Function{
                        funcId :: String,               --function name
                        funcArgs :: [(String,DataType)],--function args
                        funcType :: DataType,    -- function return type    
                        funcBody :: Program      -- function code
                        }
                 |  Procedure{
                        procId :: String,               --
                        procArgs :: [(String,DataType)],
                        procBody :: Program
                        }
                deriving(Eq)
                

-- Data-structure for statements
data Statement = Assign{                -- Variable assignment
                    assVarId :: String, -- Variable name
                    assVal   :: Exp     -- Variable Value
                    } 
                -- If statement
                | If {
                    ifCond :: Exp,        -- condition that must be true
                    succInst :: Statement,-- Instruction executed when cond=true
                    failInst :: Statement,-- Executed when cond = false 
                    ibid     :: Int       -- if bid
                    }
                -- Case statement
                | Case{
                    caseExp    :: Exp,       --Expresion to compare 
                    -- Each of the expresion-statement guards to be
                    -- evaluated:
                    caseGuards :: [(Exp, Statement)],
                    caseElse   :: Statement, --Default case
                    cbid       :: Int
                    }
                -- For loop
                | For{
                    forIter     :: String,     --For iterator name
                    forInitVal  :: Exp,        --Iterator init valye
                    forEndVal   :: Exp,        --Iterator final value
                    forBody     :: Statement,  --For instructions
                    forIncr     :: String,     --Increase iterator 
                    fbid        :: Int         --For block id
                    }
                -- While loop
                | While{
                    whCond :: Exp,        --While condition 
                    whBody :: Statement,--while instructions
                    wbid   :: Int         --While block id 
                    }
                -- Function & procedure calls
                | ProcCall{
                    procName :: String,
                    procCallArgs :: [Exp]
                }
                -- Block
                | Block {   --Instruction block
                    blockInsts :: [Statement]
                    } 
                | Break
                | Continue
                | Skip
                deriving ( Eq)


-- Data-structure for variables


-- Data-structure for hole program
-- TODO: add declarations and other useful stuff
-- Hint: make a tuple containing the other ingredients
data Program = Program{
                progInstrs ::Statement,     --Set of statements to execute. 
                                            --It must be a Block Statement
                progDecl :: [Declaration]   -- Variables declared for this 
                                            -- program. Note that since
                                            -- Pascal doesn't allow declaring
                                            -- variables in the program body,
                                            -- declariations are independent on
                                            -- the program statements
            }
            deriving(Eq)

type MainProgram = (String,Program)

------------------------------------------------------------
-- < Helper function to print program > --------------------

instance Show Declaration where
    show (Variable s d) = "[Var] " ++ s ++ " : " ++ show d

    show f@Function{}   = "[Function] " ++ fname ++ 
                          "(" ++ fargs ++ ")" ++ ftype ++
                          fbody
        where 
            fname  = funcId f
            fargs' = map (\(a , b) -> a ++ " : " ++ show b ++ "; ") . funcArgs $ f
            fargs  = concat fargs'
            ftype  = " : " ++ (show . funcType $ f) 
            fbody' = funcBody f
            fbody  = unlines . map ("  "++) . lines . show $ fbody'

    show p@Procedure{}   = "[Procedure] " ++ pname ++ 
                           "(" ++ pargs ++ ")" ++ pbody
        where 
            pname  = procId p
            pargs' = map (\(a , b) -> a ++ " : " ++ show b ++ "; ") . procArgs $ p
            pargs  = concat pargs'
            pbody' = procBody p
            pbody  = unlines . map ("  "++) . lines . show $ pbody'

instance Show Statement where
    show b@Block{} = "BEGIN\n" ++ stmts ++ "END\n"
        where 
            stmts' = blockInsts b
            stmts  =  unlines . map ("  "++) . lines . concatMap show $ stmts'
    show (Assign vid exp) = vid ++ " := " ++  show exp ++ "\n"

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
    
    show Break = "BREAK"
    show Continue = "CONTINUE"

instance Show Program where
    show (Program pins pdec) = strdec ++ "\n" ++ show pins
        where 
            strdec = unlines . map show $ pdec

instance Show Exp where
    show BoolExpr{ boolExpr = b} = show b
    show NumExpr{numExpr = n} = show n
    show FunExpr{funExpId=id, funExpArgs = args} = id ++ "(" ++ args' ++ ")"
        where 
            args' = concatMap ((++ "; ") . show ) args
    show IdExpr{idExpr = id} = id
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
    show BoolFunCall{boolFuncId = id, boolFunArgs = args} = id ++ "(" ++ args' ++ ")"
        where args' = concatMap ((++ "; ") . show) args

    show TrueC = "true"
    show FalseC = "false"
    show BoolVar{boolVarId = id} = id
    show RelOp{relOp = o, relOprn1 = op1, relOprn2 = op2} = "( " ++ o ++ 
                            " (" ++ show op1 ++ ") (" ++ show op2 ++ ")"
    show CompOp{compOp = o, compArg1 = op1, compArg2 = op2} = "( " ++ o ++ 
                            " (" ++ show op1 ++ ") (" ++ show op2 ++ ")"

printMainProg :: MainProgram -> String
printMainProg (s, p) = "[PROGRAM: " ++ s ++ "]\n" ++ show p