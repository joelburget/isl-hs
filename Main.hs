module Main where

import ISL
import InlineBindings

example1 :: String
example1 = unlines
  [ "[n] -> { [i,j] -> [i2,j2] : i2 = i + 1 and j2 = j + 1 and "
  , "1 <= i and i < n and 1 <= j and j < n or "
  , "i2 = i + 1 and j2 = j - 1 and "
  , "1 <= i and i < n and 2 <= j and j <= n }"
  ]

main :: IO ()
main = do
  putStrLn "inline 1:"
  test1
  putStrLn "inline 2:"
  test2

  ctx <- ctxAlloc


  -- test 1
  m <- mapReadFromStr ctx example1
  (m, exact) <- mapPower m
  print exact
  mapFree m


  -- test 2
  m <- mapReadFromStr ctx example1
  (m, exact) <- mapTransitiveClosure m
  print exact
  mapFree m


  -- test 3
  space <- spaceSetAlloc ctx 0 2
  bset <- basicSetUniverse =<< spaceCopy space
  ls <- localSpaceFromSpace space

  c <- constraintAllocEquality =<< localSpaceCopy ls
  c <- constraintSetCoefficientSi c IslDimSet 0 (-1)
  c <- constraintSetCoefficientSi c IslDimSet 1 2
  bset <- basicSetAddConstraint bset c

  c <- constraintAllocEquality =<< localSpaceCopy ls
  c <- constraintSetConstantSi c (-10)
  c <- constraintSetCoefficientSi c IslDimSet 0 1
  bset <- basicSetAddConstraint bset c

  c <- constraintAllocEquality ls
  c <- constraintSetConstantSi c 42
  c <- constraintSetCoefficientSi c IslDimSet 0 (-1)
  bset <- basicSetAddConstraint bset c

  bset <- basicSetProjectOut bset IslDimSet 1 1
  print =<< basicSetToStr bset


  -- test 4
  bset <- basicSetReadFromStr ctx "{[i] : exists (a : i = 2a and i >= 10 and i <= 42)}"
  print =<< basicSetToStr bset


  ctxFree ctx

  gen
