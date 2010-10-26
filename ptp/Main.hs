
module Main where

import System.Environment
import System
import Builder
import Project
import Parser
import Codegen
import CodegenTools
import CodegenTypes
import Declarations
import qualified Control.Exception as E
import qualified Data.List as List

buildProgram :: String -> String -> IO ()
buildProgram fileIn fileOut =
	readFile fileIn >>= return . createProgram . projectFromXml >>= writeFile fileOut

loadProject :: String -> IO Project
loadProject fileIn = readFile fileIn >>= return . projectFromXml

oneLineAnswer :: String -> (Project -> String) -> IO ()
oneLineAnswer fileIn fn = do
	project <- loadProject fileIn;
	printHandle $ putStr $ fn project ++ "\n"

parseArgs :: [String] -> IO ()
parseArgs (param:args) | "--" `List.isPrefixOf` param =
	case param:args of
		[ "--type", fin, str ] -> oneLineAnswer fin (typePrint str)
		[ "--place", fin, str ] -> oneLineAnswer fin (placePrint str)
		_ -> putStr $ "Invalid parameter " ++ param
	where
		typePrint str project = (typeString . fromNelType) (parseType (typeTable project) "<cmd>" str)
		placePrint str project = (typeString . TPlace . fromNelType) (parseType (typeTable project) "<cmd>" str)

parseArgs [fin, fout] = printHandle $ buildProgram fin fout
parseArgs _ = putStr "Usage: ptp <project_file> <output_file>\n"

printHandle =
	E.handle catchFn
	where
		catchFn (E.SomeException e) = do { putStr (addNl (show e)); System.exitWith (ExitFailure 1) }
		addNl s
			| List.isSuffixOf "\n" s = s
			| otherwise = s ++ "\n"

main = do
	getArgs >>= parseArgs
