module SymbolTable where

import qualified Pascal.Data as D
import qualified Data.Map as M
import qualified Data.List as L

import Data.Maybe

--This file contains data declaration & functions
--for the symbol table

-- This Data contains aditional info depending
-- on the symbol type.
data SymType =  Program 
             |  Function{
                    fbid  :: Int, -- bid (Scope) for this function
                    fdecl :: D.Declaration -- Declaration data 
                }
             |  Procedure{
                    pbid  :: Int, -- scope for his procedure
                    pdecl :: D.Declaration --Declaration data
                }
             |  BoolVar{
                    bval    :: Bool,     --boolean value
                    bdecPos :: (Int,Int) --Declaration pos 
                }
            
             |  RealVar{
                    rval    :: Float,    --real value
                    rdecPos :: (Int,Int) --Declaration pos 
                }
             deriving(Show, Eq)
            
-- Data structure to represent a symbol
data Symbol = Symbol{
                symId :: String,   --Symbol name, like the name of a var
                symScope :: Int,   --Declaration scope of this symbol
                symType :: SymType --Aditional data for this symbol
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
                    scopeCnt :: Int
                    }
                    deriving(Show, Eq)
-- Contructor for SymbolTables
newTable :: SymbolTable
newTable = SymbolTable{
            symMap = M.empty,
            scopeStk = [0],
            scopeCnt = 0
            }

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
findSym s st = 
    let 
        smap = symMap st
        stk  = scopeStk st
        mtch = M.lookup s smap

        sym = (findSym' stk =<< mtch)
        --Aux function: finds the first symbol available in the 
        --current context
        findSym' :: [Int] -> [Symbol] -> Maybe Symbol
        findSym' [] _ = Nothing
        findSym' _ [] = Nothing
        findSym' (x:xs) l = case L.find ((==x) . symScope) l of
                                Just sym -> Just sym
                                Nothing  -> findSym' xs l
    in sym

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