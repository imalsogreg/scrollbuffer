{- | 
A purely functional fixed-length forgetful buffer.

This is meant to be used where a circular list for
streaming data would normally be used in an imperative
language.

Example use-case: A film crew trying to take high-speed 
video of sharks hunting for seals.  The camera operator 
doesn't know in advance when a shark attack will happen, 
in order to trigger recording; and the camera can't store 
more than 10 seconds (10 thousand frames) at a time. So 
we store the frames in a buffer with limited memory, and 
rely on the camera operator to press a trigger button as 
soon as possible after the shark attack.  The trigger 
moves the frames in the camera's memory onto the hard disk.

          
>                 Shark attach           
>                 |          | <- Operator's reaction time
>                 V__________Trigger 
>                 |          | 
>Data:   NNNNNNNNNSSSSSSSSSSNNNNN
>Buffer: .....NNNNSSSSSSSSSSNN
>             |______________| <- Buffer's memory
>        |   | 
>        |___|  <- Past (forgotten) data

ScrollBuffer forgetfully buffers data using Data.Sequence.  
Additionally, it keeps track of a cursor position that
corresponds to one point in time.

>Data     0 1 2 3 4 5 6 7     |
>Buffer       2 3 4 5 6 7     |  EKG view: 3 4 5 6 7 2
>Cursor         |             |
>                             |
>Add one element: 8           |
>                             |
>Data     0 1 2 3 4 5 6 7 8   |
>Buffer         3 4 5 6 7 8   |  EKG view: 3 4 5 6 7 8
>Cursor         |             |
>                             |
>Add one element: 9           |
>                             |
>Data     0 1 2 3 4 5 6 7 8 9 |
>Buffer           4 5 6 7 8 9 |  EKG view: 9 4 5 6 7 8
>Cursor                     | |

Press the trigger and capture the N most recent samples
using the ~~| operator.

~~| Doesn't involve the cursor.  The cursor is used as
a break point for 'EKG-style' scrolling display with
new data appearing to overwrite old (although this
is not the pattern in which data is written in the
underlying datastructure).

-}

module Data.ScrollBuffer ( 
  
  -- * ScrollBuffer
  ScrollBuffer(..),
  
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

-- This follows the monoid laws.  Is it useful?
instance M.Monoid (ScrollBuffer a) where
  mempty = fromList []
  sb1@(ScrollBuffer s1 i1) `mappend` sb2@(ScrollBuffer s2 i2)
    | null sb1  = sb2
    | null sb2  = sb1
    | otherwise = ScrollBuffer (M.mappend s1 s2) (i1 + i2)

instance (Eq a) => Eq (ScrollBuffer a) where
  (ScrollBuffer s1 i1) == (ScrollBuffer s2 i2) = s1 == s2 && i1 == i2

instance (Show a) => Show (ScrollBuffer a) where
  show (ScrollBuffer s cursor) = "[" P.++ showSBString P.++ "] ind: " P.++ show cursor
    where printElem e ind = if ind == cursor 
                            then (":" P.++ show e P.++ ":" )
                            else show e
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
        
