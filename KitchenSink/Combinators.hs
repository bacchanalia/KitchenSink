--  Copyright (C) 2012 Michael Zuser mikezuser@gmail.com
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

-- | Miscellaneous combinators.
module KitchenSink.Combinators
  ( (|$|)
  , (<$$>)
  , (<|>)
  , if'
  , (?)
  , (??)
  , dup
  , swap
  , bot
  ) where
import Prelude hiding (foldr)
import Control.Applicative hiding ((<|>))
import Control.Monad
import qualified Control.Applicative as A
import Data.Foldable

infixl 0 |$|
-- | Left fix ($) usable as an argument seperator.
(|$|) :: (a -> b) -> a -> b
(|$|) = ($)

infixl 4 <$$>
-- | Map inside two levels of functor.
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap fmap fmap

infixr 1 <|>
-- | Rexport of (Control.Applicative.'Control.Applicative.<|>') with infixr 1.
(<|>) :: Alternative f => f a -> f a -> f a
(<|>) = (A.<|>)

-- | Church encode a Bool.
if' :: Bool -> a -> a -> a
if' b t e = if b then t else e

infixr 2 ?
-- | Just a value given a condition or Nothing
(?) :: Bool -> a -> Maybe a
b ? x = if b then Just x else Nothing

infixl 0 ??
-- | ('??') is a generalization of fromMaybe.
-- Get the \"leftmost\" element out of a Foldable or a default if there is none.
(??) :: Foldable f => f a -> a -> a
(??) = flip $ foldr const

-- | Duplicate a value across a pair.
dup :: a -> (a,a)
dup = \a -> (a,a)

-- | Swap the order of values in a pair.
swap :: (a, b) -> (b, a)
swap = \(a, b) -> (b, a)

-- | A shorter 'undefined'.
-- The sound your program makes when falling off a cliff.
bot :: a
bot = error "Bottom CH!"

