
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
import Utils
import qualified Control.Exception as E
import qualified Data.List as List

buildProgram :: String -> String -> IO ()
buildProgram fileIn fileOut =
	readFile fileIn >>= return . (createProgram fileOut) . projectFromXml >>= writeFile fileOut

loadProject :: String -> IO Project
loadProject fileIn = readFile fileIn >>= return . projectFromXml

quickAnswer :: String -> (Project -> String) -> IO ()
quickAnswer fileIn fn = do
	project <- loadProject fileIn;
	printHandle $ putStr $ fn project ++ "\n"

parseArgs :: [String] -> IO ()
parseArgs (fin:param:args) | "--" `List.isPrefixOf` param =
	case param:args of
		[ "--type", str ] -> quickAnswer fin (typePrint str)
		[ "--place-type", str ] -> quickAnswer fin (placeTypePrint str)
		[ "--transition-vars", str ] -> quickAnswer fin (transitionVars str)
		_ -> putStr $ "Invalid parameter " ++ param ++ "\n"
	where
		typePrint str project = (typeString . fromNelType) (parseType (typeTable project) "<cmd>" str)
		placeTypePrint str project = (typeString . TPlace . fromNelType) (placeTypeById' project (read str))
		transitionVars str project = case transitionVarType project (transitionById project (read str)) of
			TStruct _ types -> addDelimiter "\n" (map (\(n,t) -> typeString t ++ " " ++ n) $! types)
			_  -> error "Invalid type"

parseArgs [fin, fout] = printHandle $ buildProgram fin fout
parseArgs _ = putStr "Usage: ptp <project_file> [options]\n" >> System.exitWith (ExitFailure 1)

printHandle =
	E.handle catchFn
	where
		catchFn (E.SomeException e) = do { putStr (addNl (show e)); System.exitWith (ExitFailure 1) }
		addNl s
			| List.isSuffixOf "\n" s = s
			| otherwise = s ++ "\n"

main = do
	getArgs >>= parseArgs
