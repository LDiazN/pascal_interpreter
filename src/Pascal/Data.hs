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
        MainProgram
    ) where

-- Data-structure for  numeric expressions
data Exp = BoolExpr { boolExpr :: BoolExp } --Boolean expression
         | NumExpr  { numExpr :: NumExp }   --Numeric expresion
         | FunExpr  { funExpId :: String, funExpArgs :: [Exp] } --Function call
         | IdExpr   { idExpr :: String } --Simple id
         | BinaryOp { opert :: String, oper1 :: Exp, oper2 :: Exp }
           deriving(Show, Eq)

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
            deriving(Show, Eq)
                

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
            deriving(Show, Eq)

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
                deriving(Show, Eq)
                

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
                deriving (Show, Eq)


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
            deriving(Show, Eq)

type MainProgram = (String,Program)