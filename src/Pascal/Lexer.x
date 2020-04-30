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
  , lexerUtil
  , getErrors
  , LexError(..)
  , getId
  , getPos
  )
where

import System.Exit
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
}

%wrapper "monadUserState-bytestring"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters


tokens :-

  <0> $white+                               ; -- remove multiple white-spaces
  -- < Keywords > ----------------------------------------------
  <0> begin                                 { readTkGen }
  <0> end                                   { readTkGen }
  <0> program                               { readTkGen }  
  <0> var                                   { readTkGen }
  <0> if                                    { readTkGen }
  <0> else                                  { readTkGen }
  <0> then                                  { readTkGen }
  <0> case                                  { readTkGen }
  <0> of                                    { readTkGen }
  <0> while                                 { readTkGen }
  <0> do                                    { readTkGen }
  <0> for                                   { readTkGen }
  <0> to                                    { readTkGen }
  <0> downto                                { readTkGen }
  <0> break                                 { readTkGen }
  <0> continue                              { readTkGen }
  <0> function                              { readTkGen }
  <0> procedure                             { readTkGen }

  -- < Supported Data Types > ----------------------------------
  <0> real                                  { readTkGen }
  <0> boolean                               { readTkGen }

  -- < Operators & separators > --------------------------------
  <0> [\+]|[\-]|[\*]|[\/]| mod              { readTkGen }
  <0> [\=]|\>\=|\<\=|\<|\>|\<\>             { readTkGen }
  <0> and|or|not                            { readTkGen }
  <0> \:\=                                  { readTkGen }
  <0> \;                                    { readTkGen }
  <0> \:                                    { readTkGen }
  <0> \,                                    { readTkGen }
  <0> \.                                    { readTkGen }
  <0> \(                                    { readTkGen }
  <0> \)                                    { readTkGen }

  -- < Constants & names > -------------------------------------
  <0> true                                  { readTkGen }
  <0> false                                 { readTkGen }
  <0> $digit+[\.]$digit+                    { readReal }
  <0> $digit+                               { readReal }
  <0> $alpha [$alpha $digit \_ \']*         { readId }

  -- < Unvalid Tokens > ----------------------------------------
  <0> $digit+ $alpha [$alpha $digit \_ \']* { addErrUndef }
  <0> \*\)                                  { addErrUnMtchComm }
  <0> .                                     { addErrUndef }
  
  -- < To ignore > ---------------------------------------------
  <0> "//".*                                ; -- skip one line comments
  <0> \(\*                                  { begin comment }
  <comment> \*\)                            { begin program }
  <comment> $white+                         ; -- ignore anything 
                                              -- inside a comment.
  <comment> .                               ;
  -- Note: Since we have a comment and a program state, it's easier to 
  -- add some restrictions if needed. If we don't want nested comments, 
  -- we can use the comment state to check such errors.
  -- If we want to add string data types, then it's easier with state handling.
{

-- Some action helpers:
--tok' f (p, _, input, _) len = return $ Token p (f (B.take (fromIntegral len) input))
--tok x = tok' (\s -> x)
--tok_string x = tok' (\s -> x (B.unpack s))
--tok_read x = tok' (\s -> x . read . B.unpack $ s)

----- << Possible reader states >> -----
program :: Int 
program = 0


----- << Token Reader functions >> -----

readTkGen :: AlexInput ->Int64 -> Alex Token
readTkGen (pn@(AlexPn _ r c), _, s, _) l = 
        return (Token pn . TkGen . take (fromIntegral l) . B.unpack $ s)

-- Action to read an int token
readReal :: AlexInput -> Int64 -> Alex Token
readReal (pn@(AlexPn _ r c), _, s, _) l = 
        return (Token pn . TkReal . read . take (fromIntegral l) . B.unpack $ s)

-- Action to read an id 
readId :: AlexInput -> Int64 -> Alex Token
readId (pn@(AlexPn _ r c), _, s, _) l = 
        return (Token pn . TkId . take (fromIntegral l) . B.unpack $ s)


-- Helper function to add an error to the error stack
addError :: LexError -> Alex ()
addError e = do
  errors    <- getErrors 
  setErrors (e:errors)

-- Action to add an error if found
addErrUndef  :: AlexInput -> Int64 -> Alex Token
addErrUndef (AlexPn _ ln c, _, s, _) l = do
      let id = take (fromIntegral l) . B.unpack $ s
          e = UndefToken id ln c
      addError e
      alexMonadScan

-- Action to add an error if found
addErrUnMtchComm  :: AlexInput -> Int64 -> Alex Token
addErrUnMtchComm (AlexPn _ ln c, _, s, _) l = do
      let e = UnmatchedCommClose ln c
      addError e
      alexMonadScan

----- << Alex analyzer functions >> -----

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

                              -- check if all the comments are closed
                              stcode <- alexGetStartCode
                              when (stcode==comment) $ addError UnexpectedEOF
                              errs   <- getErrors
                              case errs of 
                                [] -> return []
                                _  -> alexError . unlines . map show $ errs
                              return []
            Token _ _     -> do
                              tks <- loop
                              return (tk:tks)
             
    runAlex (B.pack s) loop
        

-- In case of monadic threaded parser:
lexerUtil :: Alex (Either String Token)
lexerUtil = do
  tk <- alexMonadScan
  case tk of
    Token _ TkEOF ->do  
          -- Now that we reached the end of file, 
          -- we have to check if there was some error

          -- check if all the comments are closed
          stcode <- alexGetStartCode
          when (stcode==comment) $ addError UnexpectedEOF
          errs   <- getErrors
          case errs of 
            [] -> return $ Right tk
            _  -> return $ Left . ("lexical error at line "++) . unlines . map show $ errs
          return $ Right tk
    _     -> return $ Right tk

-- user state data type
data AlexUserState = AlexUserState{
                      errors :: [LexError] --A stack of errors 
                    }
-- Initialization function required by alex
alexInitUserState :: AlexUserState                    
alexInitUserState = AlexUserState []

getErrors :: Alex [LexError]
getErrors = Alex $ \s@AlexState{ alex_ust = ust } -> Right (s, errors ust)

setErrors :: [LexError] -> Alex ()
setErrors e = Alex $ \s@AlexState{ alex_ust = ust } -> Right (s{alex_ust = ust{errors = e} },())



------ << Token Data Types >> --------
data Token = Token AlexPosn TokenClass
  deriving (Show)

tokenToPosN :: Token -> AlexPosn
tokenToPosN (Token p _) = p

alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return $ Token p TkEOF


data TokenClass
 = TkGen         { tkVal    :: String } --Generic token     
 | TkReal        { realVal :: Float }
 | TkId          { idVal :: String }
 | TkEOF

-- get Position of a token 
getPos :: Token -> (Int, Int)
getPos (Token (AlexPn _ l c) _) = (l,c)

getId  :: Token -> String
getId (Token _ (TkId s)) = s
getId t = error "error calling getId : this is not a TkId: "++ show t 

instance Show TokenClass where
  show (TkGen s) = s
  show (TkReal f) = show f
  show (TkId s) = s
  show TkEOF = "[EOF]"

data LexError = UndefToken{ undefTok :: String, 
                            undfline :: Int, 
                            undfpos  :: Int }

              | UnexpectedEOF
              | UnmatchedCommClose{
                            unmtCommLn  :: Int,
                            unmtCommPos :: Int
                            }
              
instance Show LexError where
    show (UndefToken s l c) = "Lexer Error: Undefined symbol  '" ++ s ++ 
                              "'\n             at line: " ++ show l ++
                              ", column: " ++ show c ++ "."
    show UnexpectedEOF      = "Lexer Error: Unexpected EOF. Perhaps you" ++
                              " miss a closing comment symbol '*)'"

    show (UnmatchedCommClose l c) = "Lexer Error: Unmatched closing comment symbol '*)'"++ 
                                  "\n             at line: " ++ show l ++
                                  ", column: " ++ show c ++ "."

}
