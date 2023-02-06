-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module ClassTest2 (stateTrib, runStateTrib, writeLeaves, collapse, mapLeavesWithAddress, toQuadTree, fromQuadTree) where

import Data.List
import Data.Char

import Control.Monad.State
import Control.Monad.Writer

import Types

-- Question 1

stateTrib :: Integer -> State (Integer,Integer,Integer) ()
stateTrib 1 = pure() 
stateTrib n = do
                modify(\(x,y,z) -> (x+y+z,x,y))
                stateTrib (n-1)
            
             



runStateTrib :: Integer -> Integer
runStateTrib n =
  let ((),(a,b,c)) = runState (stateTrib n) (1,0,0)
  in a

-- Question 2

writeLeaves :: Bin a b -> Writer [Either a b] ()
writeLeaves (Lf n) = tell [Left n]
writeLeaves (Nd n ln rn) = do 
                                writeLeaves (ln)
                                tell [Right n]
                                writeLeaves (rn)
                                
                           

-- Question 3

collapse :: Bin (Bin a b) b -> Bin a b
collapse (Lf(Nd x ln rn)) = Nd x ln rn
collapse (Nd y (ln) (rn)) =  (Nd y (collapse ln) (collapse rn))
collapse (Lf (Lf n)) = Lf n


-- Question 4

mapLeavesWithAddress :: (a -> Address -> c) -> Bin a b -> Bin c b
-- mapLeavesWithAddress (adr) (Nd x l r) = (Nd x (mapLeavesWithAddress adr (Nd l)) (mapLeavesWithAddress adr (Nd r)))
-- mapLeavesWithAddress adr  (Lf x) = (Lf [adr])
mapLeavesWithAddress = undefined


-- Question 5

toQuadTree :: Image -> QuadTree
toQuadTree  =  undefined 

fromQuadTree :: QuadTree -> Image
fromQuadTree = undefined
