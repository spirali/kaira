
module Main where

import System.Environment
import Builder
import Project

buildProgram :: String -> String -> IO ()
buildProgram fileIn fileOut =
	readFile fileIn >>= return . createProgram . projectFromXml >>= writeFile fileOut

parseArgs :: [String] -> IO ()
parseArgs [fin, fout] = buildProgram fin fout
parseArgs _ = putStr "Usage: ptp <project_file> <output_file>\n"

main = do 
	getArgs >>= parseArgs
