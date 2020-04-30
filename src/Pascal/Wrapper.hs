module Pascal.Wrapper
  ( parse'
  , Error(..)
  , ErrClass(..)
  )
  where

----------------------------------------------------------------------------

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



parse' :: String -> Either Error MainProgram
parse' s = 
    case tokenizer s of
        Left m    -> Left $ Error 0 0 (Message m)
        Right tks -> case happyParser tks of
                        Ok mp -> Right mp
                        Failed e -> Left $ Error 0 0 (Message e)


