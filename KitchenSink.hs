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

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_HADDOCK prune #-}

-- | Everything in the KitchenSink
module KitchenSink
  ( module Prelude
  , module KitchenSink.Combinators
  , module KitchenSink.Control
  , module KitchenSink.List
  , module KitchenSink.ChainCmp
  , module KitchenSink.Math
  , module KitchenSink.Qualified
  , module KitchenSink.Search
  , module KitchenSink.String
  , module KitchenSink.System
  , module Codec.Binary.UTF8.String
  , module Control.Applicative
  , module Control.Applicative.Backwards
  , module Control.Applicative.Lift
  , module Control.Arrow
  , module Control.Category
  , module Control.Concurrent
  , module Control.Concurrent.Async
  , module Control.Concurrent.Chan
  , module Control.Concurrent.MVar
  , module Control.Concurrent.STM
  , module Control.Concurrent.ParallelIO.Local
  , module Control.DeepSeq
  , module Control.Error
  , leftT, rightT
  , module Control.Exception
  , tryJustIO
  , module Control.Monad
  , module Control.Monad.Fix
  , module Control.Monad.Cont
  , module Control.Monad.Error
  , module Control.Monad.Identity
  , module Control.Monad.List
  , module Control.Monad.Maybe
  , module Control.Monad.RWS
  , module Control.Monad.Reader
  , module Control.Monad.State
  , module Control.Monad.Trans
  , module Control.Monad.Writer
  , wpass
  , module Control.Parallel
  , module Data.Bits
  , module Data.Bits.Extras
  , module Data.Char
  , module Data.Complex
  , module Data.Dynamic
  , module Data.Either
  , module Data.Fixed
  , FixNum
  , module Data.Foldable
  , module Data.Function
  , module Data.Functor
  , module Data.Functor.Compose
  , module Data.Functor.Constant
  , module Data.Functor.Identity
  , module Data.Functor.Product
  , module Data.Functor.Reverse
  , module Data.Hashable
  , module Data.IORef
  , module Data.Int
  , module Data.Ix
  , module Data.List
  , module Data.List.Split
  , sEndBy, sOneOf
  , module Data.Maybe
  , module Data.Monoid
  , MProduct
  , module Data.Ord
  , module Data.Primitive
  , PArray
  , module Data.Ratio
  , module Data.STRef
  , module Data.String
  , module Data.Time
  , module Data.Typeable
  , module Data.Word
  , module Data.Traversable
  , module Debug.Trace
  , module GHC.Generics
  , module System.CPUTime
  , module System.Directory
  , module System.Environment
  , module System.Exit
  , module System.FilePath
  , module System.IO
  , module System.IO.Error
  , module System.Info
  , module System.Mem
  , module System.Mem.StableName
  , module System.Mem.Weak
  , module System.Locale
  , module System.Posix
  , dup1, PosixHandler
  , module System.Process
  , module System.Random
  , splitGen
  , module System.Timeout
  , module Test.QuickCheck
  , (.&?.)
  , module Text.Parsec
  , tryp, ParsecState, labelp, labelsp
  , module Text.Parsec.Error
  , module Text.Parsec.Pos
  , module Text.Printf
  ) where

import KitchenSink.Combinators
import KitchenSink.Control
import KitchenSink.List
import KitchenSink.ChainCmp
import KitchenSink.Math
import KitchenSink.Qualified
import KitchenSink.Search
import KitchenSink.String
import KitchenSink.System

import Prelude hiding
    -- generalized by Control.Category
  ( (.), id
    -- generalized by Data.Foldable
  , all, and, any, concat, concatMap, elem, foldl, foldl1, foldr, foldr1, mapM_
  , maximum, minimum, notElem, or, product, sequence_, sum
    -- generalized by Data.Traversable
  , mapM, sequence
  )
import Codec.Binary.UTF8.String
import Control.Applicative hiding
    -- replaced by KitchenSink.Combinators
    ((<|>))
import Control.Applicative.Backwards
import Control.Applicative.Lift
import Control.Arrow
import Control.Category
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.ParallelIO.Local
import Control.DeepSeq
import Control.Error hiding
    -- conflicts with Control.Arrow
    ( left  -- disamb: leftT
    , right -- disamb: rightT
    -- replaced by Control.Monad.Maybe
    , MaybeT, runMaybeT
    )
import Control.Exception hiding
    -- conflicts with Control.Error
    (tryJust) -- disamb: tryJustIO
import Control.Monad hiding
    -- generalized by Data.Foldable
  ( forM_, mapM_, msum, sequence_
    -- generalized by Data.Traversable
  , forM, mapM, sequence
  )
import Control.Monad.Instances ()
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Cont hiding
    -- generalized by Data.Foldable
  ( forM_, mapM_, msum, sequence_
    -- generalized by Data.Traversable
  , forM, mapM, sequence
  )
import Control.Monad.Error hiding
    -- generalized by Data.Foldable
  ( forM_, mapM_, msum, sequence_
    -- generalized by Data.Traversable
  , forM, mapM, sequence
  )
import Control.Monad.Identity hiding
    -- generalized by Data.Foldable
  ( forM_, mapM_, msum, sequence_
    -- generalized by Data.Traversable
  , forM, mapM, sequence
  )
import Control.Monad.List hiding
    -- generalized by Data.Foldable
  ( forM_, mapM_, msum, sequence_
    -- generalized by Data.Traversable
  , forM, mapM, sequence
  )
import Control.Monad.Maybe
import Control.Monad.RWS hiding
  ( forM_, mapM_, msum, sequence_
    -- generalized by Data.Traversable
  , forM, mapM, sequence
    -- conflicts with Data.Functor.Product
  , Product -- disamb: MProduct
    -- conflicts with KitchenSink.Control
  , pass    -- disamb: wpass
  )
import Control.Monad.Reader hiding
    -- generalized by Data.Foldable
  ( forM_, mapM_, msum, sequence_
    -- generalized by Data.Traversable
  , forM, mapM, sequence
  )
import Control.Monad.State hiding
    -- generalized by Data.Foldable
  ( forM_, mapM_, msum, sequence_
    -- generalized by Data.Traversable
  , forM, mapM, sequence
  )
import Control.Monad.Writer hiding
    -- generalized by Data.Foldable
  ( forM_, mapM_, msum, sequence_
    -- generalized by Data.Traversable
  , forM, mapM, sequence
    -- conflicts with Data.Functor.Product
  , Product -- disamb: MProduct
    -- conflicts with KitchenSink.Control
  , pass    -- disamb: wpass
  )
import Control.Parallel
import Data.Bits
import Data.Bits.Extras
import Data.Char
import Data.Complex
import Data.Dynamic
import Data.Either
import Data.Fixed hiding
  -- conflicts with Text.QuickCheck
  (Fixed)  -- disamb: FixNum
import Data.Foldable
import Data.Function hiding
    -- generalized by Control.Category
  ((.), id)
import Data.Functor
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Reverse
import Data.Hashable
import Data.IORef
import Data.Int
import Data.Ix
import Data.List hiding
    -- generalized by Data.Foldable
  ( all, and, any, concat, concatMap, elem, find, foldl, foldl', foldl1, foldr
  , foldr1, maximum, maximumBy, minimum, minimumBy, notElem, or, product, sum
    -- generalized by Data.Traversable
  , mapAccumL, mapAccumR
    -- replaced by KitchenSink.List
  , permutations
  )
import Data.List.Split hiding
    -- conflicts with Text.Parsec
    ( endBy -- disamb: sEndBy
    , oneOf -- disamb: sOneOf
    , sepBy -- disamb: splitOn
    )
import Data.Maybe
import Data.Monoid hiding
    -- conflicts with Data.Functor.Product
    (Product)   -- disamb: MProduct
import Data.Ord
import Data.Primitive hiding
    -- conflicts with KitchenSink.Qualified
    (Array) -- disamb: PArray
import Data.Ratio
import Data.STRef
import Data.String
import Data.Time
import Data.Typeable
import Data.Word
import Debug.Trace
import Data.Traversable
import GHC.Generics
import System.CPUTime
import System.Directory
import System.Environment
import System.Exit
import System.FilePath hiding
    -- conflicts with Data.Hashable
    (combine)   -- disamb: (</>)
import System.IO
import System.IO.Error
import System.Info
import System.Locale
import System.Mem
import System.Mem.StableName
import System.Mem.Weak
import System.Posix hiding
    -- conflicts with KitchenSink.Combinators
    ( dup       -- disamb: dup1
    -- conflicts with Control.Exception
    , Handler   -- disamb: PosixHandler
    -- replaced by System.Directory
    , createDirectory, removeDirectory
    -- replaced by System.Environment
    , getEnv, getEnvironment
    )
import System.Process
import System.Random hiding
    -- conflicts with Data.List.Split
    (split)     -- disamb: splitGen
import System.Timeout
import Test.QuickCheck hiding
    -- conflicts with Data.Bits
    ((.&.))     -- disamb (.&?.)
import Text.Parsec hiding
    -- generalized by Control.Applicative
    ( many, optional, (<|>)
    -- conflicts with Control.Exception
    , try       -- disamb: tryp
    -- conflicts with Control.Monad.State
    , State     -- disamb: ParsecState
    -- conflicts with Text.QuickCheck
    , label     -- disamb: labelp
    , labels    -- disamb: labelsp
    )
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Printf

import qualified Control.Error
import qualified Control.Exception
import qualified Control.Monad.Writer
import qualified Data.Fixed
import qualified Data.List.Split
import qualified Data.Monoid
import qualified Data.Primitive
import qualified System.Posix
import qualified System.Random
import qualified Test.QuickCheck
import qualified Text.Parsec

-- | disambiguation: Control.Error.'Control.Error.left' /
-- Control.Arrow.'Control.Arrow.left'
--
-- Control.Error.left => leftT
leftT = Control.Error.left

-- | disambiguation: Control.Error.'Control.Error.right' /
-- Control.Arrow.'Control.Arrow.right'
--
-- Control.Error.right => rightT
rightT = Control.Error.right

-- | disambiguation: Control.Exception.'Control.Exception.tryJust' /
-- Control.Error.'Control.Error.tryJust'
--
-- Control.Exception.tryJust => tryJustIO
tryJustIO = Control.Exception.tryJust

-- | disambiguation: Control.Monad.Writer.'Control.Monad.Writer.pass' /
-- KitchenSink.Control.'KitchenSink.Control.pass'
--
-- Control.Monad.Writer.pass => wpass
wpass = Control.Monad.Writer.pass

-- | disambiguation: Data.Fixed.'Data.Fixed.Fixed' /
-- Test.QuickCheck.'Test.QuickCheck.Fixed'
--
-- Data.Fixed.Fixed => FixNum
type FixNum = Data.Fixed.Fixed

-- | disambiguation: Data.List.Split.'Data.List.Split.endBy' /
-- Text.Parsec.'Text.Parsec.endBy'
--
-- Data.List.Split.endBy => sEndBy
sEndBy = Data.List.Split.endBy
-- | disambiguation: Data.List.Split.'Data.List.Split.oneOf' /
-- Text.Parsec.'Text.Parsec.oneOf'
--
-- Data.List.Split.oneOf => sOneOf
sOneOf = Data.List.Split.oneOf

-- | disambiguation: Data.Monoid.'Data.Monoid.Product' /
-- Data.Functor.'Data.Functor.Product'
--
-- Data.Monoid.Product => MProduct
type MProduct = Data.Monoid.Product

-- | disambiguation: Data.Primitive.'Data.Primitive.Array' /
-- KitchenSink.Qualified.'KitchenSink.Qualified.Array'
--
-- Data.Primitive.Array => PArray
type PArray = Data.Primitive.Array

-- | disambiguation: System.Posix.'System.Posix.dup' /
-- KitchenSink.Combinators.'KitchenSink.Combinators.dup'
--
-- System.Posix.dup => dup1
dup1 = System.Posix.dup
-- | disambiguation: System.Posix.'System.Posix.Handler' /
-- Control.Exception.'Control.Exception.Handler'
--
-- System.Posix.Handler => PosixHandler
type PosixHandler = System.Posix.Handler

-- | disambiguation: System.Random.'System.Random.split' /
-- Data.List.Split.'Data.List.Split.split'
--
-- System.Random.split => splitGen
splitGen = System.Random.split

-- | disambiguation: (Test.QuickCheck.'Test.QuickCheck..&.') /
-- (Data.Bits.'Data.Bits..&.')
--
-- (Test.QuickCheck..&.) => (.&?.)
(.&?.) = (Test.QuickCheck..&.)

-- | disambiguation: Text.Parsec.'Text.Parsec.try' /
-- Control.Exception.'Control.Exception.try'
--
-- Text.Parsec.try => tryp
tryp = Text.Parsec.try
-- | disambiguation: Text.Parsec.'Text.Parsec.State' /
-- Control.Monad.State.'Control.Monad.State.State'
--
-- Text.Parsec.State => ParsecState
type ParsecState = Text.Parsec.State
-- | disambiguation: Text.Parsec.'Text.Parsec.label' /
-- Test.QuickCheck.'Test.QuickCheck.label'
--
-- Text.Parsec.label => labelp
labelp = Text.Parsec.label
-- | disambiguation: Text.Parsec.'Text.Parsec.labels' /
-- Test.QuickCheck.'Test.QuickCheck.labels'
--
-- Text.Parsec.labels => labelsp
labelsp = Text.Parsec.labels

-- Possible future additions:
--  Colour
--  Criterion
--  data-reify
--  derive
--  Diagrams
--  Gtk
--  HTML/XML something or other
--  HUnit
--  Lenses
--  Network
--  Regex
--  TH
--  more stuff from QuickCheck
--  utf8
--  LogicT
