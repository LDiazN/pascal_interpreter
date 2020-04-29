module Pascal.SymbolTable where

import qualified Pascal.Data as D
import qualified Data.Map as M
import qualified Data.List as L

import Data.Maybe

--This file contains data declaration & functions
--for the symbol table

-- This Data contains aditional info depending
-- on the symbol type.
data SymType =  Program 
             |  Function{         -- Functions with Non return type are procedures
                    fbid  :: Int, -- bid (Scope) for this function
                    funcArgs :: [(String,D.DataType)],  --function args
                    funcType :: D.DataType,             -- function return type    
                    funcBody :: D.Program   
                }
             |  BoolVar{
                    bval :: Bool     --boolean value
                }
            
             |  RealVar{
                    rval :: Float    --real value
                }
             deriving(Show, Eq)
            
-- Data structure to represent a symbol
data Symbol = Symbol{
                symId :: String,     --Symbol name, like the name of a var
                symScope :: Int,     --Declaration scope of this symbol
                symType :: SymType,  --Aditional data for this symbol
                symPos  :: (Int,Int) --declaration position (ln,col)
                } deriving (Show, Eq)
        
-- symbol table:
-- this symbol maps from a symbol name to a list of 
-- symbols with the same name but different scopes
data SymbolTable = SymbolTable{
                    --map from names to symbol.
                    symMap   :: M.Map String [Symbol], 
                    --Stack of currently available scopes
                    scopeStk :: [Int], 
                    -- Next scope available to push
                    scopeCnt :: Int,
                    -- Stack of currently available functions. Since 
                    -- a name could refer both to a function or a variable in the 
                    -- same context in recursive function calls, we have to 
                    -- infer which one we are looking for. If the name is
                    -- stacked, it means  that such name could refer to the return value or 
                    -- the recursive function call
                    funcStack :: [String]
                    }
                    deriving(Show, Eq)

-- Contructor for SymbolTables
newTable :: SymbolTable
newTable = foldl (\b a -> fromJust $ insertSym a b) initTable numFuns
    where 
        initTable = SymbolTable{
                        symMap = M.empty,
                        scopeStk = [0],
                        scopeCnt = 1,
                        funcStack = []
                        }
        numFuns = [ Symbol{
                        symId = s,
                        symScope = -1,
                        symPos = (0,0),
                        symType = Function{
                                fbid = 0,
                                funcArgs = [("x", D.RealT)],
                                funcType = D.RealT,
                                funcBody = D.Program (D.Block []) []
                                }
                        } | s <-  M.keys D.numFuns]
    
        

-- Creates a new empty scope
pushEmptyScope :: SymbolTable -> SymbolTable
pushEmptyScope st@SymbolTable{ scopeStk = s, scopeCnt = c } =
                    st{scopeStk = c:s, scopeCnt = c+1} 

-- Pop a scope
popScope :: SymbolTable -> SymbolTable
popScope st@SymbolTable{ scopeStk = [] }   = 
    error "error in popScope: there is no scope to pop from the stack" 
popScope st@SymbolTable{ scopeStk = x:xs } = st{scopeStk = xs}

{- 
 Find the symbol by name in the current scope.
 If multiple symbols have the same name, then we return
 the symbol with the higher scope (the scope 
 nearer to the top of the scope stack)
-}
findSym ::  String      -> --Symbol name
            SymbolTable -> --Table to search in
            Maybe Symbol
findSym s  = findSym' s (const True) 

--Find the frist symbol that holds a property
findSym' :: String           -> --Symbol name
            (Symbol -> Bool) -> --Some predicate
            SymbolTable      -> --Table to search in
            Maybe Symbol
findSym' s f st = 
    let 
        smap = symMap st
        stk  = scopeStk st
        mtch = M.lookup s smap

        sym = (findSym'' stk =<< mtch)
        --Aux function: finds the first symbol available in the 
        --current context
        findSym'':: [Int] -> [Symbol] -> Maybe Symbol
        findSym'' [] _ = Nothing
        findSym'' _ [] = Nothing
        findSym'' (x:xs) l = case L.find (\s -> ((==x) . symScope $ s) && f s ) l of
                                Just sym -> Just sym
                                Nothing  -> findSym'' xs l
    in sym

--Find a function whose name could be a variable or a function
findFunc :: String      -> --Symbol name
            SymbolTable -> --Table to search in
            Maybe Symbol
findFunc s st 
    | notElem s . funcStack $ st = Nothing
    | otherwise = findSym' s isFunc st

{-
    In order to keep the table consistent, 
    we override the symbol scope when adding it
    to the table. If the symbol cannot be added, 
    we return nothing, if the symbol can be added,
    we return the new table (wrapped in a just)
-}
insertSym ::Symbol      -> -- Symbol to insert 
            SymbolTable -> -- Table to be modified
            Maybe SymbolTable --Nothing if the symbol cannot be added
insertSym sym st = 
    let 
        symName = symId sym
        smap = symMap st
        currScope = head . scopeStk $ st
        checkSym = findSym symName st
        newSt = case checkSym of 
                    Nothing -> Just newSt'
                    Just s  -> if symScope s == currScope
                                    then Nothing
                                    else Just newSt'

        --In case we have to add the symbol to the map
        symList = M.lookup symName smap
        newSym  = sym{symScope = currScope}
        newSmap = case symList of 
                    Nothing -> M.insert symName [newSym] smap
                    Just l  -> M.insert symName (newSym:l) smap
        newSt' = st{symMap = newSmap}

    in newSt

--Removes a symbol from the table
removeSym ::Symbol      -> --symbol to remove
            SymbolTable -> --Table to be modified
            SymbolTable    --Same table if the symbol doesn't exists
removeSym s st = 
    let 
        symName = symId s
        smap    = symMap st
        newSt = case M.lookup symName smap of
                    Nothing -> st
                    Just l  -> st{symMap = 
                                M.insert symName (filter (/= s) l) smap } 
    in newSt

--Remove sym by name: remove the first symbol that matches a given name
removeSym' :: String -> SymbolTable -> SymbolTable
removeSym' s st =
    case findSym s st of
        Nothing -> st
        Just sym-> removeSym sym st

------------------------------------------------------------------
-- < Symbol Table utilities > ------------------------------------

-- Push a function name into the function stack
pushFunc :: String -> SymbolTable -> SymbolTable
pushFunc s st@SymbolTable{funcStack = funcs} = st{funcStack = s:funcs}

-- Pop a function from the function stack
popFunc :: SymbolTable -> SymbolTable
popFunc st@SymbolTable{funcStack = funcs}
    | null funcs = st
    | otherwise  = st{funcStack = tail funcs}

-- get the top of the function stack. If no function is stacked, return ""
topFunc :: SymbolTable -> String
topFunc SymbolTable{funcStack = []} = ""
topFunc SymbolTable{funcStack = x:xs} = x

-- Replace the value of the first value that checks the predicate
replace :: Eq a => [a] -> (a->Bool) -> a -> [a]
replace [] _ _ = []
replace (x:xs) f y 
    | f x = y:xs
    | otherwise = x : replace xs f y

-- change the value of this variable 
setVal :: String -> SymType -> SymbolTable -> SymbolTable
setVal s x st = 
    let 
        smap = symMap st
        checkSym = findSym s st
        sym = fromJust checkSym
        newSym = sym{symType = x}
        slist = fromMaybe [] (M.lookup s smap)
        newList = replace slist (==sym) newSym

        newSt = case checkSym of 
                    Nothing   -> st
                    Just _    -> st{ symMap = M.insert s newList smap }
    in newSt

-- Creates the symbol from a declaration, assuming that the declaration is correct
createSym :: D.Declaration -> Symbol
createSym D.Variable{ D.varId = s, D.varType = t, D.vDeclpos = p} = 
    let 
        newType = case t of 
                    D.BooleanT -> BoolVar{bval = False}
                    D.RealT    -> RealVar{rval = 0}
                    _          -> error "error in createSym: this is not a valid variable"
    in 
        Symbol{
            symId = s,
            symScope = 0, --The symbol table can set this value to any value
            symPos = p,
            symType = newType
        }

createSym D.Function{   D.funcId = s, 
                        D.funcArgs = args,
                        D.funcType = t, 
                        D.funcBody = fbody,
                        D.fDeclPos = p} = 
    let 
        newType = Function{
                    fbid = 0,      
                    funcArgs = args,
                    funcType = t,
                    funcBody = fbody
                }
    in 
        Symbol{
            symId = s,
            symScope = 0, --The symbol table can set this value to any value
            symPos = p,
            symType = newType
        }        

-- Check if the given id corresponds to a symbol with a specific condition.
-- This is useful to check the type of the symbol. If the symbol does not 
-- exist, return false
checkSym :: String -> (Symbol -> Bool)  -> SymbolTable -> Bool
checkSym s f st = maybe False f (findSym s st)

isBoolVar :: Symbol -> Bool
isBoolVar Symbol{symType = BoolVar{}} = True
isBoolVar _ = False

isRealVar :: Symbol -> Bool
isRealVar Symbol{symType = RealVar{}} = True
isRealVar _ = False

isFunc :: Symbol -> Bool
isFunc Symbol{symType = Function{ funcType = D.NoneT }} = False
isFunc Symbol{symType = Function{}} = True
isFunc _ = False

isProc :: Symbol -> Bool
isProc Symbol{symType = Function{ funcType = D.NoneT }} = True
isProc _ = False