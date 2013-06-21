--  Copyright (C) 2013 Michael Zuser mikezuser@gmail.com
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | Miscellaneous list functions
module KitchenSink.List
  ( -- * Combinatorial functions
    hydra
  , permutations
  , choices
  , nonEmptySubseqs
  , sublists
  , cartesianProduct
  , partitions
    -- * Miscellaneous utility
  , nonempty
  , filterByIndex
  , filterWithIndex
  , pinch
    -- * Recursion schemes
    -- ** Cascade
    -- | A cascade is like a fold, but each step in the fold can produce more
    -- elements that need to folded.
  , cascadel, cascadel', cascadelM
  , cascader, cascader', cascaderM
  )where
import KitchenSink.Combinators
import Control.Arrow
import Control.Applicative
import Data.List hiding (permutations)
import Data.Foldable (Foldable)
import qualified Data.Foldable as F

----------------------------
-- Combinatorial functions
----------------------------

-- | A many 'head'ed beast. Produces lists with each element of the given
-- list moved to the front.
hydra :: [a] -> [[a]]
hydra []     = []
hydra (x:xs) = (x:xs) : [y:x:ys | (y:ys) <- hydra xs]

-- | Like Data.List.'Data.List.permutations', but produces the results in a
-- more canonical order.
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = [y:zs | (y:ys) <- hydra xs, zs <- permutations ys]

-- | Produces a list of all possible choices of /n/ elements from a given list.
choices :: Int -> [a] -> [[a]]
choices n xs = n > length xs ? [] ?? go n xs
  where
    go 0 _      = [[]]
    go _ []     = []
    go n (x:xs) = map (x:) (go (n-1) xs) ++ go n xs

-- | Produces all non-empty subsequences of a given list.
-- Code from "Data.List", where it is mysteriously unexported.
nonEmptySubseqs :: [a] -> [[a]]
nonEmptySubseqs []     = []
nonEmptySubseqs (x:xs) = [x] : foldr f [] (nonEmptySubseqs xs)
  where f ys r = ys : (x:ys) : r

-- | Produce all (non-empty) sublists of a given list.
sublists :: [a] -> [[a]]
sublists = concatMap (tail . inits) . tails

-- | Takes the Cartesian product of a list of lists.
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct []     = [[]]
cartesianProduct (l:ls) = [x:xs | x <- l, xs <- cartesianProduct ls]

-- | Produce all partitions of a given list.
partitions :: [a] -> [([a],[a])]
partitions []     = [([], [])]
partitions (x:xs) = uncurry (++) . (consF &&& consS) $ partitions xs
  where
    consF = map $ first  (x:)
    consS = map $ second (x:)

--------------------------
-- Miscellaneous utility
--------------------------

-- | Just a nonempty list or Nothing.
nonempty :: [a] -> Maybe [a]
nonempty xs = null xs ? Just xs ?? Nothing

-- | Filter elements of a list based on their index.
filterByIndex :: (Int -> Bool) -> [a] -> [a]
filterByIndex = filterWithIndex <$> flip . const

-- | Filter elements of a list based on their index and value.
filterWithIndex :: (Int -> a -> Bool) -> [a] -> [a]
filterWithIndex f = map snd . filter (uncurry f) . zip [0..]

-- | 'pinch' is like 'zip' but hold onto the end of the longer list.
pinch :: [a] -> [b] -> ([(a,b)], Either [a] [b])
pinch xs     []     = ([], Left  xs)
pinch []     ys     = ([], Right ys)
pinch (x:xs) (y:ys) = ((x,y):zs, rs)
  where (zs, rs) = pinch xs ys

----------------------
-- Recursion schemes
----------------------

-- | cascade left
cascadel :: Foldable f => (a -> b -> (a, f b)) -> a -> f b -> a
cascadel f = F.foldl $ uncurry (cascadel f) <$$> f

-- | cascade left strictly
cascadel' :: Foldable f => (a -> b -> (a, f b)) -> a -> f b -> a
cascadel' f = F.foldl' $ uncurry (cascadel f) <$$> f

-- | cascade left monadicly
cascadelM :: (Foldable f, Monad m) => (a -> b -> m (a, f b)) -> a -> f b -> m a
cascadelM f = F.foldlM $ (>>= uncurry (cascadelM f)) <$$> f

-- | cascade right
cascader :: Foldable f => (b -> a -> (a, f b)) -> a -> f b -> a
cascader f = F.foldr $ uncurry (cascader f) <$$> f

-- | cascade right strictly
cascader' :: Foldable f => (b -> a -> (a, f b)) -> a -> f b -> a
cascader' f = F.foldr' $ uncurry (cascader f) <$$> f

-- | cascade right monadicly
cascaderM :: (Foldable f, Monad m) => (b -> a -> m (a, f b)) -> a -> f b -> m a
cascaderM f = F.foldrM $ (>>= uncurry (cascaderM f)) <$$> f

