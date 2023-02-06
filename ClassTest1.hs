-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module ClassTest1 (checkParity, substitution, largestPrimeBetween, strongPrimes, executeCommands, atmChange) where

import Types
import Data.Char

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------


{- Question 1 -}

countOnes :: String -> Int
countOnes n = length(filter(=='1') n)

checkParity :: String -> Bool
checkParity a | length a < 8 && length a `mod` 8 == 0 && (countOnes a `mod` 2 == 0) = True
checkParity a | let x = take 8 a in length x `mod` 8 == 0 && (countOnes x `mod` 2 == 0) = checkParity (drop 8 a)
              | otherwise = False
              
              

{- Question 2 -}

-- convert :: Char -> String -> Char
-- convert n key = key !! charLabel(head(n))
convertChar :: Char -> String -> Char
convertChar c key   | isLetter c == True && isUpper c == True= key !! charLabel((c))
                    | isLetter c == True && isUpper c == False = toLower(key !! (charLabel((c))))
                    | charLabel(c) == -33 = ' ' 
                    | otherwise = c

substitution :: String -> String -> String
substitution plaintext key = [convertChar x key | x<-plaintext]

{- Question 3 -}


largestPrimeBetween :: Int -> Int
largestPrimeBetween n =   head (reverse [ x | x <-[n..2*n], isPrime x])



listPrimes :: Int -> Int
listPrimes n =  head(reverse(take n [x | x<-[1..], isPrime x]))




listPrimeshigh :: Int -> Int
listPrimeshigh n  = head [x | x <-[(n+1)..], isPrime x]

listPrimesLow :: Int -> Int
listPrimesLow n  | n == 2 = undefined
                 | isPrime(n-1) = n-1
                 | otherwise = listPrimesLow(n-1)

-- nthPrime :: [Int] -> Int
-- nthPrime n = listPrimes n

-- strongcheck :: Int -> Bool
-- strongcheck x | ((listPrimeshigh x)  + (listPrimesLow x)) `div` 2 < (listPrimes x) = True
--               | otherwise = False

strongPrimes :: Int -> [Int]
strongPrimes n = take n [x | x <- [3..], isPrime x && (x > ((listPrimeshigh x + listPrimesLow x) `div` 2))]
            

-- strongPrimes :: Int -> [Int]
-- strongPrimes = undefined


{- Question 4 -}



executeCommands :: [Command] -> (Int, Int) -> (Int, Int)
executeCommands [] (x,y) = (x,y)
executeCommands [(MoveLeft,z)] (x,y) = ((x-z),y)
executeCommands ((MoveLeft,z):zs) (x,y) = executeCommands zs ((x-z),y) 

executeCommands [(MoveRight,z)] (x,y) = ((x+z),y)
executeCommands ((MoveRight,z):zs) (x,y) = executeCommands zs ((x+z),y) 

executeCommands [(MoveUp,z)] (x,y) = (x,(y+z))
executeCommands ((MoveUp,z):zs) (x,y) = executeCommands zs (x,(y+z)) 

executeCommands [(MoveDown,z)] (x,y) = (x,(y-z))
executeCommands ((MoveDown,z):zs) (x,y) = executeCommands zs (x,(y-z)) 
{- Question 5 -}

convertToNotes :: Int -> Int -> Int
convertToNotes n note | n > note = n - note
                      |  otherwise = 0

-- atmChange :: Int -> [Int] -> [(Int, Int)]
-- atmChange n notes | n > head(reverse(notes) = 

atmChange :: Int -> [Int] -> [(Int, Int)]
atmChange 0 [] = []
atmChange n [] = undefined
atmChange n note | n >= denom = (denom, n `div` denom) : atmChange (n `mod` denom)(reverse(tail(reverse(note))))
                 | n < denom = (denom, 0) : atmChange n (reverse(tail(reverse(note))))
                 where 
                 denom = head(reverse(note))