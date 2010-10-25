
module Main where

import System.Environment
import System
import Builder
import Project
import qualified Control.Exception as E
import qualified Data.List as List

buildProgram :: String -> String -> IO ()
buildProgram fileIn fileOut =
	readFile fileIn >>= return . createProgram . projectFromXml >>= writeFile fileOut

parseArgs :: [String] -> IO ()
parseArgs [fin, fout] =
	E.handle catchFn $ buildProgram fin fout
	where
		catchFn (E.SomeException e) = do { putStr (addNl (show e)); System.exitWith (ExitFailure 1) }
		addNl s
			| List.isSuffixOf "\n" s = s
			| otherwise = s ++ "\n"
parseArgs _ = putStr "Usage: ptp <project_file> <output_file>\n"

main = do
	getArgs >>= parseArgs
