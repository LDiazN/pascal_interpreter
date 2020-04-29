module Main where

import Pascal
import System.Environment
import System.Directory
import Control.Monad

import qualified Data.ByteString.Lazy.Char8 as BLC
main :: IO ()
main = do
    args <- getArgs
    let 
      filename = head args
      flags    = tail args
      astMsg   = "-a --ast: Print a raw version of the AST, without static checking.\n" ++
                 "If the program has lexical or syntactical errors, print those errors instead of the AST"

      cleanAStMsg = "-ca --clean-ast: Print a clean version of the AST if it does not have any errors.\n" ++ 
                    "If there is static errors, print those errors instead of the AST"
      symTableMsg = "-st --symbol-table: [TEST FEATURE ONLY] Print the resulting symbol table with the native 'show'"++
                    " haskell method if there's no runtime, static, or lexical/syntactical errors"
      helpMsg     = "-h --help: Show this help"
      noFlagsMsg  = "( no flags currently available )"
      availableFlags = [cleanAStMsg, astMsg, symTableMsg, helpMsg]
      help = "Use: Pascal [ filename ] { flags }\n  Available Flags: \n" ++ (unlines . map ("    "++) . concatMap lines) availableFlags

    if null args 
      then putStrLn help
      else do 
        when (elem "-h" flags || elem "--help" flags) $ putStr help
        file <- doesFileExist filename
        if not file 
          then putStrLn $ "Error: this is not a valid file '" ++ filename ++ "'"

          else do 
            fileContent <- readFile filename
            case parse' fileContent of
              Left s    -> print s
              Right tks -> do 
                              when (elem "-a" flags || elem "--ast" flags) $ putStr . printMainProg $ tks
                              prog <- analyzeAST tks
                              case prog of
                                  Left  s -> putStrLn s
                                  Right p -> do
                                              when (elem "-ca" flags || elem "--clean-ast" flags) $ putStr . printMainProg $ p
                                              status <-  interpret p
                                              print status