import Pascal.Lexer
import Pascal.Data
import Pascal.Parser
import Pascal.Wrapper
import Pascal.Analyzer
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as BLC

main = do
    (filename:[]) <- getArgs
    fileContent   <- readFile filename
    putStrLn "Tokenizing..."
    case parse' fileContent of
        Left s    -> print s
        Right tks -> do 
                        putStr . printMainProg $ tks

                        prog <- analyzeAST tks
                        case prog of
                            Left  s -> putStrLn s
                            Right p -> putStr . printMainProg $ p

