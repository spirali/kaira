{-
    Copyright (C) 2010, 2011 Stanislav Bohm

    This file is part of Kaira.

    Kaira is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, version 3 of the License, or
    (at your option) any later version.

    Kaira is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Kaira.  If not, see <http://www.gnu.org/licenses/>.
-}

module Main where

import System.Environment
import System
import Builder
import Project
import Parser
import Codegen
import CodegenTypes
import Declarations
import BuilderTools
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
		placeTypePrint str project = (typeString . caPlace . fromNelType) (placeTypeById project (read str))
		transitionVars str project = case varStruct project (transitionById project (read str)) of
			TStruct _ types -> addDelimiter "\n" (map (\(n,t) -> typeString t ++ " " ++ n) $! publicTypes types)
			_  -> error "Invalid type"
		isPublic ('_':'_':_) = False
		isPublic _ = True
		publicTypes types = filter (\(n,t) -> isPublic n) types

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
