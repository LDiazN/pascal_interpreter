import Pascal.Lexer
import Pascal.Data
import Pascal.Parser
import Pascal.Wrapper
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as BLC

main = do
    (filename:[]) <- getArgs
    fileContent   <- readFile filename
    putStrLn "Tokenizing..."
    case parseString fileContent of
        Left s    -> print s
        Right tks -> print tks

