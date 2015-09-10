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

-- | Control combinators.
module KitchenSink.Control
  ( pass
  , ifM, whenM, unlessM
  , (<&&>), (<||>)
  , findM, anyM, allM
  , onA
  , parIOWith, parIO, parIO_
  ) where
import KitchenSink.Combinators
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.ParallelIO.Local
import Data.Foldable (Foldable, foldlM)
import Data.Maybe

-- | Do nothing.
pass :: Monad m => m ()
pass = return ()

-- | 'if'' with a monadic condition
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b x y = b >>= \b -> if b then x else y

-- | 'Control.Monad.when' with a monadic condition
whenM :: Monad m => m Bool -> m () -> m ()
whenM b x = ifM b x pass

-- | 'Control.Monad.unless' with a monadic condition
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b x = ifM b pass x

-- | Monadic ('&&')
infixr 3 <&&>
(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
a <&&> b = ifM a b (return False)

-- | Monadic ('||')
infixr 2 <||>
(<||>) :: Monad m => m Bool -> m Bool -> m Bool
a <||> b = ifM a (return True) b

-- | 'Data.Foldable.find' with a monadic predicate
findM :: (Foldable f, Functor m, Monad m) => (a -> m Bool) -> f a -> m (Maybe a)
findM p = foldlM (flip f) Nothing
  where f a = maybe ((? a) <$> p a) (return . Just)

-- | 'any' with a monadic predicate
anyM :: (Foldable f, Functor m, Monad m) => (a -> m Bool) -> f a -> m Bool
anyM = fmap isJust <$$> findM

-- | 'all' with a monadic predicate
allM :: (Foldable f, Functor m, Monad m) => (a -> m Bool) -> f a -> m Bool
allM p = not <$$> anyM (not <$$> p)

-- | 'on' with an applicative projection
infixl 0 `onA`
onA :: Applicative f => (b -> b -> c) -> (a -> f b) -> a -> a -> f c
onA op proj a b = op <$> proj a <*> proj b

-- | Run a list of IO actions given an evaluator.
parIOWith :: (Pool -> [IO a] -> IO b) -> [IO a] -> IO b
parIOWith p as = do
    n <- getNumCapabilities
    withPool n $ \pool ->
        p pool $ map (extraWorkerWhileBlocked pool) as

-- | Run a list of IO actions in parallel
parIO :: [IO a] -> IO [a]
parIO = parIOWith parallel

-- | Run a list of IO actions in parallel, discarding the results
parIO_ :: [IO a] -> IO ()
parIO_ = parIOWith parallel_

