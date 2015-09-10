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

-- | Miscellaneous string functions.
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, FlexibleInstances #-}
module KitchenSink.String
  ( chomp
  , multiline, Lines
  ) where

import Control.Applicative
import Control.Monad.Writer
import Data.List
import Data.String

-- | Remove trainling newlines from a string
chomp :: String -> String
chomp = dropWhileEnd (== '\n')

-- | 'multiline' is a hack for using do-notation and OverloadedStrings to write
-- multiline strings
--
-- @
--    {-# LANGUAGE OverloadedStrings #-}
--
--    twolines = multiline $ do
--      "line1"
--      "line2"
--    -- twolines == "line1\nline\n"
-- @
multiline :: Lines -> String
multiline = execWriter . lineWriter

type Lines = LineWriter ()
newtype LineWriter a = LineWriter { lineWriter :: Writer String a }
  deriving (Functor, Applicative, Monad)

instance IsString (LineWriter a) where
  fromString s = LineWriter $ tell (s ++ "\n") >> return undefined
