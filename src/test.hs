import Pascal.Lexer
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as BLC

main = do
    (filename:[]) <- getArgs
    fileContent   <- readFile filename
    case tokenizer fileContent of
        Left s    -> putStrLn s
        Right tks -> putStr . unlines . map show $ tks 

