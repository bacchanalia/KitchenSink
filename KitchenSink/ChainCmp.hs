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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}

-- | Chain comparisons together
--
-- example:
--
-- @
-- inRange :: Ord a => (a, a) -> a -> Bool
-- inRange (a, b) x = doCmp (a <=. x <=. b)
-- @
module KitchenSink.ChainCmp
    ( CmpChain, doCmp
    , (==.), (/=.), (<.) , (>.), (<=.), (>=.)
    ) where
import KitchenSink.Combinators
import Data.Maybe

infixl 4 ==., /=., <., <=., >., >=.

-- | A chain of comparisions.
newtype CmpChain a = Chain {unChain :: Maybe a}

-- | Run a chain of comparisions to get a boolean result.
doCmp :: CmpChain a -> Bool
doCmp = isJust . unChain

class ChainCmp a b where
    -- | Chaining ('==')
    (==.) :: a -> b -> CmpChain b
    -- | Chaining ('/=')
    (/=.) :: a -> b -> CmpChain b
    -- | Chaining ('<')
    (<.)  :: a -> b -> CmpChain b
    -- | Chaining ('<=')
    (<=.) :: a -> b -> CmpChain b
    -- | Chaining ('>')
    (>.)  :: a -> b -> CmpChain b
    -- | Chaining ('>=')
    (>=.) :: a -> b -> CmpChain b

liftS :: (a -> a -> Bool) -> (CmpChain a -> a -> CmpChain a)
liftS op x y = Chain $ (? y) . (`op` y) =<< unChain x

liftZ :: (a -> a -> Bool) -> (a -> a -> CmpChain a)
liftZ op x y = Chain $ (? y) . (`op` y) $ x

instance (a ~ b, Ord a) => ChainCmp (CmpChain a) b where
    (==.) = liftS (==)
    (/=.) = liftS (/=)
    (<.)  = liftS (<)
    (<=.) = liftS (<=)
    (>.)  = liftS (>)
    (>=.) = liftS (>=)

instance (a ~ b, Ord a) => ChainCmp a b where
    (==.) = liftZ (==)
    (/=.) = liftZ (/=)
    (<.)  = liftZ (<)
    (<=.) = liftZ (<=)
    (>.)  = liftZ (>)
    (>=.) = liftZ (>=)

