{-
    Copyright (C) 2010 Stanislav Bohm

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

module Utils where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

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

{-
strType (TData name rawType mode _) = "TData " ++ name
strType TInt = "Int"
strType (TTuple types) = "Tuple:[" ++ addDelimiter "," (map strType types) ++ "]"
strType (TArray t) = "Array " ++ strType t
strType (TPointer t) = "Pointer:" ++ strType t
strType t = "SomeType"-}


dependacyOrder :: (Ord a) => ([a] -> a -> Bool) -> (Set.Set a) -> [a]
dependacyOrder fn items =
	process items []
	where
		process items ordered
			| items == Set.empty = ordered
			| otherwise = let (newOrdered, notOrdered) = Set.partition (fn ordered) items in
					if newOrdered == Set.empty then
						error "Items cannot be ordered"
					else
						process notOrdered (ordered ++ Set.toList newOrdered)

-- |[[1,2,3],[2,4],[5]] ==> [[1,2,3,4],[5]]
joinOverlapped :: (Eq a) => [[a]] -> [[a]]
joinOverlapped [] = []
joinOverlapped (x:rest) =
	List.foldr List.union x contains : notContains
	where
		hasIntersection x y = (not . null) (x `List.intersect` y)
		(contains, notContains) = List.partition (hasIntersection x) (joinOverlapped rest)
