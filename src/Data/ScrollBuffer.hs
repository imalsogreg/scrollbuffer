module Data.ScrollBuffer ( 
  
  -- * ScrollBuffer
  ScrollBuffer,
  
  -- * Construction
  fromList, fromSeq, 
  
  -- * Advancement
  advanceOne, advanceList, advanceSeq, (|>),
  
  -- * Accessors
  -- ** Extracting subsequences
  (~~|), toScrollParts,
  
  take, drop,
  
  -- ** Length Information
  length, null,
  

  ) where

import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold
import qualified Data.Monoid   as M
import qualified Data.List     as L (intercalate)
import           Prelude hiding ( length, null, take, drop, (++) )
import qualified Prelude as P

-- |Scrolling buffers based on Data.Sequence, for adding new data while forgetting old
data ScrollBuffer a = ScrollBuffer (Seq.Seq a) Int

-- This follows the monoid laws.  Is it useful
instance M.Monoid (ScrollBuffer a) where
  mempty = fromList []
  sb1 `mappend` sb2@(ScrollBuffer s2 _)
    | null sb1  = sb2
    | null sb2  = sb1
    | otherwise = advanceSeq sb1 s2

instance (Show a) => Show (ScrollBuffer a) where
  show (ScrollBuffer s cursor) = "[" P.++ showSBString P.++ "] ind: " P.++ show cursor
    where printElem e ind = if ind == cursor then "*" else show e
          showSBList      = zipWith printElem (Fold.toList s) [0..]
          showSBString    = L.intercalate ", " showSBList
                            
                            
instance Fold.Foldable ScrollBuffer where
  fold        (ScrollBuffer s _) = Fold.fold s
  foldMap f   (ScrollBuffer s _) = Fold.foldMap f s
  foldr   f b (ScrollBuffer s _) = Fold.foldr f b s
  foldl   f a (ScrollBuffer s _) = Fold.foldl f a s
  foldr1  f   (ScrollBuffer s _) = Fold.foldr1 f s
  foldl1  f   (ScrollBuffer s _) = Fold.foldl1 f s

-- | Build a ScrollBuffer from a list of elements
fromList :: [a] -> ScrollBuffer a
fromList l = ScrollBuffer (Seq.fromList l) 0

-- | Build a ScrollBuffer from a sequence of elements
fromSeq :: Seq.Seq a -> ScrollBuffer a
fromSeq s = ScrollBuffer s 0

-- | Add one element to the right of the buffer and bump one off the left
advanceOne :: ScrollBuffer a -> a -> ScrollBuffer a
advanceOne (ScrollBuffer s i) a = 
  ScrollBuffer ((Seq.drop 1 s) Seq.|> a) newI
    where newI = (i - 1) `mod` Seq.length s

infixr 5 |>
-- | Add one element to the right of the buffer and bump one off the left (advanceOne)
(|>) :: ScrollBuffer a -> a -> ScrollBuffer a
sb |> a = advanceOne sb a

-- | Add a list of elements to the right of the buffer one at a time.
advanceList :: ScrollBuffer a -> [a] -> ScrollBuffer a
advanceList sb as = 
  foldl advanceOne sb as

-- | Add a sequence of elements to the right of the buffer one at a time
advanceSeq :: ScrollBuffer a -> Seq.Seq a -> ScrollBuffer a
advanceSeq (ScrollBuffer s1 i1) s2 =
  ScrollBuffer (Seq.drop l2 (M.mappend s1 s2)) newI
    where l2   = Seq.length s2
          newI = (i1 - l2) `mod` Seq.length s1
          
take :: Int -> ScrollBuffer a -> [a]
take n (ScrollBuffer s _) = P.take n (Fold.toList s)

drop :: Int -> ScrollBuffer a -> Seq.Seq a
drop n (ScrollBuffer s _) = Seq.drop n s


length :: ScrollBuffer a -> Int
length (ScrollBuffer s _ ) = Seq.length s

null :: ScrollBuffer a -> Bool
null (ScrollBuffer s _ ) = Seq.null s

infixr 5 ~~|
-- | Take the n most recent samples.  Mnemonic: ~ ... ñ.  Keep ñ before the right edge.
(~~|) :: Int -> ScrollBuffer a -> Seq.Seq a
n ~~| sb = drop (length sb - n) sb

-- | Break the buffer at the index.  First element in the pair is all 
-- samples more recent than the index, second element is the older 
-- elements.  This is useful for 'EKG' style plots that hold old data 
-- stationary and scroll the new data over top.
toScrollParts :: ScrollBuffer a -> ([a],[a])
toScrollParts (ScrollBuffer s i) = (newPart,oldPart)
  where newPart = (P.drop i . Fold.toList) s
        oldPart = (P.take i . Fold.toList) s
        
