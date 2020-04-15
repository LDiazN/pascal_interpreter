{

{-# LANGUAGE OverloadedStrings                  #-}
{-# LANGUAGE NoMonomorphismRestriction          #-}
{-# LANGUAGE CPP                                #-}
{-# OPTIONS_GHC -fno-warn-unused-binds          #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures    #-}
{-# OPTIONS_GHC -fno-warn-unused-matches        #-}
{-# OPTIONS_GHC -fno-warn-unused-imports        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing        #-}
{-# OPTIONS_GHC -fno-warn-tabs                  #-}
{-# OPTIONS_GHC -funbox-strict-fields           #-}

module Pascal.Lexer
  ( Alex(..)
  , AlexPosn(..)
  , AlexState(..)
  , Token(..)
  , TokenClass(..)
  , alexError
  , alexMonadScan
  , runAlex
  , tokenToPosN
  , tokenizer
  )
where

import System.Exit
import qualified Data.ByteString.Lazy.Char8 as B
}

%wrapper "monadUserState-bytestring"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters


-- TODO: Map symbols into token types (with or without parameters)
tokens :-

  -- < Keywords > ----------------------------------------------
  begin                                 { readTok   TkBegin }
  end                                   { readTok   TkBegin }
  program                               { readTok   TkProgram }  
  var                                   { readTok   TkVar }
  if                                    { readTok   TkIf }
  else                                  { readTok   TkElse }
  then                                  { readTok   TkThen }
  writeln|readln|sqrt|sin|cos|ln|exp    { readFunc }
  while                                 { readTok   TkWhile }
  do                                    { readTok   TkDo }
  for                                   { readTok   TkFor }
  to                                    { readTok   TkTo }
  break                                 { readTok   TkBreak }
  continue                              { readTok   TkContinue }
  function                              { readTok   TkFunction }
  procedure                             { readTok   TkProcedure }

  -- < Operators & separators > --------------------------------
  [\+]|[\-]|[\*]|[\/]|[\%]              { readAritOper }
  [\=]|\>\=|\<\=|\<|\>|\<\>             { readRelOper }
  and|or|not                            { readBoolOper }
  \:\=                                  { readTok   TkInitVar }
  \=                                    { readTok   TkAssign }
  \;                                    { readTok   TkSemiColon }
  \:                                    { readTok   TkColon }
  \,                                    { readTok   TkComma }

  -- < Constants & names > -------------------------------------
  true                                  { readTok  TkTrue }
  false                                 { readTok  TkFalse }
  $digit+                               { readInt }
  $alpha [$alpha $digit \_ \']*         { readId }

  -- < Unvalid Tokens > ----------------------------------------
  $digit+ $alpha [$alpha $digit \_ \']* { addErrUndef }

  -- < To ignore > ---------------------------------------------
  "//".*                                ; -- skip one line comments
  $white+                               ; -- remove multiple white-spaces
  --[\(]|[\)]|begin|end                   { readTok     TokenK }
{

-- Some action helpers:
--tok' f (p, _, input, _) len = return $ Token p (f (B.take (fromIntegral len) input))
--tok x = tok' (\s -> x)
--tok_string x = tok' (\s -> x (B.unpack s))
--tok_read x = tok' (\s -> x . read . B.unpack $ s)

----- << Token Reader functions >> -----
-- Given a token without associated value, return an action 
readTok :: TokenClass -> AlexInput -> Int64 -> Alex Token
readTok t (alxpsn, _, _, _) _ = return $ Token alxpsn t 

-- Action to read an int token
readInt :: AlexInput -> Int64 -> Alex Token
readInt (pn@(AlexPn _ r c), _, s, _) l = 
        return (Token pn . TkInt . read . take (fromIntegral l) . B.unpack $ s)

-- Action to read an id 
readId :: AlexInput -> Int64 -> Alex Token
readId (pn@(AlexPn _ r c), _, s, _) l = 
        return (Token pn . TkId . take (fromIntegral l) . B.unpack $ s)

-- Action to read a numeric operation
readAritOper :: AlexInput -> Int64 -> Alex Token
readAritOper (pn@(AlexPn _ r c), _, s, _) l = 
        return (Token pn . TkAritOper . take (fromIntegral l) . B.unpack $ s)

-- Action to read a relational operation
readRelOper :: AlexInput -> Int64 -> Alex Token
readRelOper (pn@(AlexPn _ r c), _, s, _) l = 
        return (Token pn . TkRelOper . take (fromIntegral l) . B.unpack $ s)

-- Action to read a bool operation
readBoolOper :: AlexInput -> Int64 -> Alex Token
readBoolOper (pn@(AlexPn _ r c), _, s, _) l = 
        return (Token pn . TkBoolOper . take (fromIntegral l) . B.unpack $ s)

-- Action to read a built-in function
readFunc :: AlexInput -> Int64 -> Alex Token
readFunc (pn@(AlexPn _ r c), _, s, _) l = 
        return (Token pn . TkBuiltfunc . take (fromIntegral l) . B.unpack $ s)


-- Action to add an error if found
addErrUndef  :: AlexInput -> Int64 -> Alex Token
addErrUndef (AlexPn _ ln c, _, s, _) l = do
      let id = take (fromIntegral l) . B.unpack $ s
          e = UndefToken id ln c
      errors <- getErrors
      setErrors (e:errors)
      alexMonadScan


-- Main function: Returns a token list from a String
-- This function returns a list of tokens given string
tokenizer :: String -> Either String [Token]
tokenizer s = do
    let loop = do
          tk <- alexMonadScan
          case tk of
            Token _ TkEOF ->do  
                              -- Now that we reached the end of file, 
                              -- we have to check if there was some error
                              errs <- getErrors
                              case errs of 
                                [] -> return []
                                _  -> alexError . unlines . map show $ errs
                              return []
            Token _ _     -> do
                              tks <- loop
                              return (tk:tks)
             
    runAlex (B.pack s) loop
        




-- user state data type
data AlexUserState = AlexUserState{
                      errors :: [LexError]
                    }

alexInitUserState :: AlexUserState                    
alexInitUserState = AlexUserState []

getErrors :: Alex [LexError]
getErrors = Alex $ \s@AlexState{ alex_ust = ust } -> Right (s, errors ust)

setErrors :: [LexError] -> Alex ()
setErrors e = Alex $ \s@AlexState{ alex_ust = ust } -> Right (s{alex_ust = ust{errors = e} },())




------ < Token Data Types > ------
data Token = Token AlexPosn TokenClass
  deriving (Show)

tokenToPosN :: Token -> AlexPosn
tokenToPosN (Token p _) = p

alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return $ Token p TkEOF


data TokenClass
 = TkAritOper    { aritOper :: String }    
 | TkRelOper     { relOper  :: String }    
 | TkBoolOper    { boolOper :: String }
 | TkTrue      
 | TkFalse      
 | TkInt         { intVal :: Int}
 | TkId          { idVal :: String }
 | TkEOF
 | TkBegin
 | TkEnd
 | TkProgram  
 | TkVar  
 | TkIf
 | TkElse
 | TkThen
 | TkBuiltfunc   { funcName :: String }
 | TkWhile  
 | TkFor
 | TkTo          
 | TkDo
 | TkBreak
 | TkContinue
 | TkFunction 
 | TkProcedure
 | TkAssign
 | TkInitVar
 | TkColon
 | TkSemiColon
 | TkComma
 

 deriving (Eq, Show)

data LexError = UndefToken{ undefTok :: String, 
                            undfline :: Int, 
                            undfpos :: Int }
              
instance Show LexError where
    show (UndefToken s l c) = "Lexer Error: Undefined symbol  '" ++ s ++ 
                              "'\n             at line: " ++ show l ++
                              ", column: " ++ show c ++ "."

}
