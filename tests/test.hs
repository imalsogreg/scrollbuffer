module Main where

-- These import are coppied from simormar's test-async.hs and
-- twittner's ZMQ3 Properties.hs.  I have to find out which
-- ones I actually need.
import Test.QuickCheck
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2
import Control.Monad
import Prelude as P
import Data.Monoid

import Data.ScrollBuffer as S
import Data.Sequence as Seq
import Data.Foldable as Fold

import Prelude hiding (catch)

instance (Arbitrary a) => Arbitrary (ScrollBuffer a) where
  arbitrary = do
    elems  <- listOf arbitrary
    wInd   <- arbitrary
    if P.null elems 
      then return $ ScrollBuffer (Seq.fromList elems) 0
      else
      return $ ScrollBuffer (Seq.fromList elems) (wInd `mod` P.length elems)
        
main :: IO ()
main = defaultMain tests

tests = [
  testGroup "Monoid laws" [
     testProperty "mempty `mappend` ScrollBuffer" prop_mempty_x
  ,  testProperty "ScrollBuffer `mappend` mempty" prop_x_mempty
  ,  testProperty "Buffer 1 `mappend` Buffer 2"   prop_monoid_x_y
  ]
  , testGroup "Cursor movement" [
      testProperty "Non-zero cursor moves back for 1 element" prop_nz_add_one 
     ,testProperty "Zero-cursor to end for 1 element advance" prop_z_add_one
     ,testProperty "Advance by self-copy" prop_self_copy
     ]
  , testGroup "Splitting" [
      testProperty "Split parts sum to whole" prop_split_sum_correct
   ,  testProperty "Get last n while n <= length" prop_last_n
   ,  testProperty "Get last n with n too big"    prop_last_n_big
     ]
  ]


prop_mempty_x :: ScrollBuffer Double -> Bool
prop_mempty_x sb = mempty `mappend` sb == sb
  where _ = sb :: ScrollBuffer Double

prop_x_mempty :: ScrollBuffer Int -> Bool
prop_x_mempty sb = sb `mappend` mempty == sb
  where _ = sb :: ScrollBuffer Int
        
prop_monoid_x_y :: ScrollBuffer Int -> 
                   ScrollBuffer Int -> 
                   ScrollBuffer Int -> Bool
prop_monoid_x_y sb1 sb2 sb3 = 
  (sb1 `mappend` sb2) `mappend` sb3 ==
  sb1 `mappend` (sb2 `mappend` sb3)
  where _ = sb1 :: ScrollBuffer Int

prop_nz_add_one :: ScrollBuffer Char -> Char -> Property
prop_nz_add_one sb@(ScrollBuffer _ inInd) e =
  inInd /= 0 ==> outInd == inInd - 1 
    where
      (ScrollBuffer _ outInd) = sb S.|> e
      _ = sb :: ScrollBuffer Char
      
prop_z_add_one :: ScrollBuffer Double -> Double -> Property
prop_z_add_one sb@(ScrollBuffer s inInd) e =
  inInd == 0 && (not $ S.null sb) ==> 
  outInd == Seq.length s - 1
    where
      (ScrollBuffer _ outInd) = sb S.|> e
      _ = sb :: ScrollBuffer Double
      
prop_self_copy :: ScrollBuffer Double -> Bool
prop_self_copy sb@(ScrollBuffer s _) =
  sb == sb `advanceList` Fold.toList s
    where _ = sb :: ScrollBuffer Double
          
prop_split_sum_correct :: ScrollBuffer String -> Bool
prop_split_sum_correct sb@(ScrollBuffer s _) = 
  P.length a + P.length b == Seq.length s
    where
      (a,b) = toScrollParts sb
      _ = sb :: ScrollBuffer String
      
prop_last_n :: ScrollBuffer (ScrollBuffer Double) -> Int -> Property
prop_last_n sb@(ScrollBuffer s _) n =
  n <= Seq.length s ==> 
  n ~~| sb == Seq.drop (Seq.length s - n) s
    where
      _ = sb :: ScrollBuffer (ScrollBuffer Double)
      
prop_last_n_big :: ScrollBuffer Double -> Int -> Property
prop_last_n_big sb@(ScrollBuffer s _) n =
  n > Seq.length s ==>
  n ~~| sb == s
  where _ = sb :: ScrollBuffer Double