
module Utils where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Declarations

addDelimiter :: String -> [String] -> String
addDelimiter d xs = List.concat $ List.intersperse d xs

countedMap :: (Int -> a -> b) -> [a] -> [b]
countedMap f list  = [ f x y | (x, y) <- zip [0..] list ]

indexOf :: (a -> Bool) -> [a] -> Maybe Int
indexOf fn list = 
	indexOf' fn list 0
	where 
		indexOf' _ [] _ = Nothing
		indexOf' fn (a:as) n 
			| fn a = Just n 
			| otherwise = indexOf' fn as (n+1)

divide :: (Ord b) => (a -> b) -> [a] -> [[a]]
divide fn list = 
	map snd (Map.toList (Map.unionsWith (++) [ Map.singleton (fn x) [x] | x <- list ]))

triangleDependancy :: (a -> a -> Bool) -> [a] -> [(a, [a])]
triangleDependancy fn list =
	makeTriangle list []
	where 
		makeTriangle [] _ = []
		makeTriangle (a:as) b = (a, filter (fn a) b) : (makeTriangle as (a:b))

hasKey :: (Eq a) => a -> [(a, b)] -> Bool
hasKey key list = Maybe.isJust $ List.lookup key list
