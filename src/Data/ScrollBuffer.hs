module Data.ScrollBuffer where

import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold
import qualified Data.Monoid   as M
import Data.List

data ScrollBuffer a = ScrollBuffer (Seq.Seq a) Int

--'mappend' the way I'm using it advances the scrollbuffer
--Does this obey the monoid rules?
instance M.Monoid (ScrollBuffer a) where
  mempty = fromList []
  mappend sb1 (ScrollBuffer s2 _) = advanceSeq sb1 s2
            
instance (Show a) => Show (ScrollBuffer a) where
  show (ScrollBuffer s cursor) = withBraces showSBString
    where f c i          = if i == cursor then "*" else show c
          showSBList     = zipWith f (Fold.toList s) [0..]
          showSBString   = intercalate ", " showSBList
          withBraces str = "[" ++ str ++ "] ind: " ++ show cursor
                                    
instance Fold.Foldable ScrollBuffer where
  fold        (ScrollBuffer s _) = Fold.fold s
  foldMap f   (ScrollBuffer s _) = Fold.foldMap f s
  foldr   f b (ScrollBuffer s _) = Fold.foldr f b s
  foldl   f a (ScrollBuffer s _) = Fold.foldl f a s
  foldr1  f   (ScrollBuffer s _) = Fold.foldr1 f s
  foldl1  f   (ScrollBuffer s _) = Fold.foldl1 f s

fromList :: [a] -> ScrollBuffer a
fromList l = ScrollBuffer (Seq.fromList l) 0

advanceOne :: ScrollBuffer a -> a -> ScrollBuffer a
advanceOne (ScrollBuffer s i) a = 
  ScrollBuffer ((Seq.drop 1 s) Seq.|> a) newI
    where newI = (i - 1) `mod` Seq.length s

advanceList :: ScrollBuffer a -> [a] -> ScrollBuffer a
advanceList sb as = 
  foldl advanceOne sb as

advanceSeq :: ScrollBuffer a -> Seq.Seq a -> ScrollBuffer a
advanceSeq (ScrollBuffer s1 i1) s2 =
  ScrollBuffer (Seq.drop l2 (M.mappend s1 s2)) newI
    where l2   = Seq.length s2
          newI = (i1 - l2) `mod` Seq.length s1
          
take :: Int -> ScrollBuffer a -> [a]
take n (ScrollBuffer s _) = Prelude.take n (Fold.toList s)

drop :: Int -> ScrollBuffer a -> [a]
drop n (ScrollBuffer s _) = Prelude.drop n (Fold.toList s)

length :: ScrollBuffer a -> Int
length (ScrollBuffer s _ ) = Seq.length s

toScrollParts :: ScrollBuffer a -> ([a],[a])
toScrollParts (ScrollBuffer s i) = (newPart,oldPart)
  where newPart = (Prelude.drop i . Fold.toList) s
        oldPart = (Prelude.take i . Fold.toList) s