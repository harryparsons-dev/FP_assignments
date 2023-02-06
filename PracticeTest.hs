-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module PracticeTest (checksum , golfScorer, highlyDivisible, largestOddFactor, equals, babylonianPalindromes) where

import Types

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}
checksum :: Integral a => [a] -> Bool
checksum a| length a == 8 && sum a `mod` 11 == 0 = True
          | otherwise = False

{- Question 2 -}
golfScorer :: Integer -> Integer -> Integer
golfScorer par sto   | sto == 1 = 5
                     | sto <= (par-2) = 4
                     | sto == (par-1) = 3
                     | sto == par = 2
                     | sto == (par+1) = 1
                     | sto > (par+1) = 0
                     | otherwise = -1

{- Question 3 -}
highlyDivisible :: Int -> [Int]
highlyDivisible n = take n [x | x <- [1..], allDivides x]
    where allDivides :: Int -> Bool
          allDivides n = all (\ k -> n `mod` k == 0) [2..12]





largestodd :: Int -> Int
largestodd n = head(reverse [x | x <- factors(n), odd(x) == True])

largestOddFactor :: Int -> [Int]
largestOddFactor n = map largestodd [x | x <-[1..n]]
  

  

{- Question 4 -}
equals :: (Enum a, Bounded a, Eq b) => (a -> b) -> (a -> b) -> Bool
equals f g = and [f x == g x | x<-[minBound..maxBound]]

{- Question 5 -}


dec_to_bab :: Integer -> [Integer]
dec_to_bab a | a < 60 = [a]
             | otherwise = (a `div` 60) : (dec_to_bab(a `mod` 60))
             
toBase60 :: Integer -> [Integer]
toBase60 n = go n []
  where go n soFar | n < 60 = n:soFar
        go n soFar | otherwise = let r = n `mod` 60
                                     m = n - r
                                  in go (m `div` 60) (r:soFar)

babylonianPalindromes :: [Integer]
babylonianPalindromes = [x | x <- [1..], let a = toBase60 x, length a > 1, reverse a == a] 



