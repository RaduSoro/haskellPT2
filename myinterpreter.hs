module Main where

import Lib
import Tokens
import Parser
import System.Environment
import Control.Exception
import System.IO

main :: IO (Env)
main = catch main' noParse

main' = do (fileName : _ ) <- getArgs 
           
           -- input <- getContents
           -- let split_input = lines input

           sourceText <- readFile fileName
           let tokens = scanTokens sourceText
           let parsed = parseCalc tokens
           -- putStrLn( "Contents : " ++ (show tokens) )
           -- putStrLn ("Parsed as " ++ (show parsed))
           
           eval parsed emptyDataStore

noParse :: ErrorCall -> IO (Env)
noParse e = do let err =  show e
               hPutStr stderr err
               return ([])