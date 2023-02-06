-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module ClassTest1Retake (checkPeriodic, divisibleByIndex, findCubes, edit, edits, solvable) where

import Data.List
import Data.Char

import Types

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}

checkPeriodic :: String -> Int -> Bool
checkPeriodic str n = all (\ k ->  (str !! k) == (str !! (k + n))) [0..(length str-1-n)]

{- Question 2 -}

divisibleByIndex :: [Int] -> [Bool]  
divisibleByIndex n = map (\(n, k) -> n `mod` k == 0) (zip n [1..])

{- Question 3 -}

findCubes :: Int -> [(Int,Int,Int)]
findCubes n = take 1 [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], (x^3 + y^3 + z^3) == n]

{- Question 4 -}


edit :: EditCommand -> Text -> Text
edit  (MoveRight) (l,r) = (((take 1 r) ++ l), drop 1 r)
edit  (MoveLeft) (l,r) = ((drop 1 l),  (take 1 l) ++ r)
edit (Insert ch) (l,r) = ((([ch]) ++ l), r) 
edit (BackSpace) (l,r) = (drop 1 l, r)

edits :: [EditCommand] -> Text -> Text
edits [] (l,r) = (l,r)
edits (x:xs) (l,r) = edits xs (edit x (l,r))

        
{- Question 5 -}

solvable :: ([Bool] -> Bool) -> Int -> Bool
solvable = undefined 

