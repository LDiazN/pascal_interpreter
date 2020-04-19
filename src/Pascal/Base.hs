module Pascal.Base where

import Pascal.Lexer
----------------------------------------------------------------------------

-- For readablity - these are the interfaces Happy expects:

data Parser a = Ok a | Failed String


thenP :: Parser a -> (a -> Parser b) -> Parser b
thenP m k = case m of 
              Ok a     -> k a
              Failed e -> Failed e

returnP :: a -> Parser a
returnP  = Ok 

--alexShowError :: (Show t, Show t1) => (t, t1, Maybe String) -> Alex a
--alexShowError (line, column, e) = alexError $ "show-error: " ++ show (line, column, e)
--
--alexGetPosition :: Alex AlexPosn
--alexGetPosition = Alex $ \s@AlexState{alex_pos=pos} -> Right (s, pos)

failP :: String -> Parser a
failP  = Failed 

catchP :: Parser a -> (String -> Parser a) -> Parser a
catchP p f = case p of
            Ok a -> Ok a
            Failed s -> f s

happyError :: [Token] -> Parser a 
happyError ((Token (AlexPn _ ln col) t):tks) = failP $ 
          "Parse Error: Unexpected symbol '" ++ show t ++ "'\n" ++ 
          "At line: " ++ show ln ++  ", column: " ++ show col 


--happyError :: Parser a
--happyError = do
--  (AlexPn _ line col) <- alexGetPosition
--  alexShowError (line, col, Nothing)

-- Link the lexer and the parser:
--lexer :: (Token -> Parser a) -> Parser a
--lexer f = alexMonadScan >>= f
