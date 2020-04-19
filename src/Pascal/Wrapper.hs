module Pascal.Wrapper
  ( parse'
  --, parseString
  , Error(..)
  , ErrClass(..)
  )
  where

----------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import           Data.List            (isPrefixOf)
----
import           Pascal.Lexer         (tokenizer)
import           Pascal.Data          (MainProgram)
import           Pascal.Parser        (happyParser)
import           Pascal.Base
----------------------------------------------------------------------------

data ErrClass
    = Syntactical (Maybe String)
    | Lexical
    | Message String
    deriving (Show, Eq)

data Error = Error
    { errLine  :: Int
    , errPos   :: Int
    , errClass :: ErrClass
    } deriving (Eq)

instance Show Error where
    show (Error _ _ (Message s)) = s
    show (Error l c Lexical) =  "Lexical error at line: " ++ show l ++ 
                                ", column: " ++ show c
    show (Error l c (Syntactical s) ) = 
        case s of 
            Nothing ->  "Unkown syntactical error at line " ++ show l ++ 
                        ", column: " ++ show c
            Just s' ->  "Syntactical error at line: " ++ show l ++ ", column: " ++
                        show c ++ ".\n" ++ s'


--parse :: BL.ByteString -> Either Error MainProgram
--parse s =
--    -- Alex's error type is a String, that we have to parse here,
--    -- otherwise we cannot get type-safe information out of 'parse'.
--    let showErrPrefix = "show-error: " :: String
--        lexicalErrorPrefix = "lexical error at line " :: String
--     in case runAlex s happyParser of
--            Right x -> Right x
--            Left str | showErrPrefix `isPrefixOf` str ->
--                          let (line, column, m) =
--                                  (read (drop (length showErrPrefix) str) :: (Int, Int, Maybe String))
--                           in Left (Error line column (Syntactical m))
--                     | lexicalErrorPrefix `isPrefixOf` str ->
--                          let info = drop (length lexicalErrorPrefix) str
--                              lineStr = takeWhile (/= ',') info
--                              columnStr = drop (9 + length lineStr) info
--                           in Left (Error (read lineStr) (read columnStr) Lexical)
--                     | otherwise  -> Left (Error 0 0 (Message str))


parse' :: String -> Either Error MainProgram
parse' s = 
    case tokenizer s of
        Left s    -> Left $ Error 0 0 (Message s)
        Right tks -> case happyParser tks of
                        Ok mp -> Right mp
                        Failed e -> Left $ Error 0 0 (Message e)




-- string version of above function for testing and running
--parseString :: String -> Either Error MainProgram
--parseString s = parse $ BLC.pack s 