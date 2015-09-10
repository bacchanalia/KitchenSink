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

{-# LANGUAGE ScopedTypeVariables #-}

-- | Miscellaneous IO actions.
module KitchenSink.System
  ( clipboard
  , inHomeDir
  , makeAbsolute
  , makeAbsolute'
  , readLnRetry
  , rr
  ) where
import Control.Monad
import Data.List
import Data.Typeable
import System.Directory
import System.FilePath
import System.Process
import Text.Read

-- | Put String into clipboard using xsel
clipboard :: String -> IO ()
clipboard = void . readProcess "xsel" ["--input","--clipboard"]

-- | Create a path within the home directory
inHomeDir :: FilePath -> IO FilePath
inHomeDir path = canonicalizePath . (</> path) =<< getHomeDirectory

-- | Try to read a line untill one contains valid input.
readLnRetry :: forall a. (Typeable a, Read a) => IO a
readLnRetry = maybe failed return . readMaybe =<< getLine
  where
    typestr = show $ typeOf (undefined :: a)
    failed = do
        putStrLn $ "Please try again: input was not a valid "  ++ typestr
        readLnRetry

-- | Make a path absolute by prepending the current directory (if it isn't
-- already absolute) and applying @'normalise'@ to the result.
--
-- The operation may fail with the same exceptions as @'getCurrentDirectory'@.
--
-- From: directory-1.2.2.0
makeAbsolute :: FilePath -> IO FilePath
makeAbsolute = fmap normalise . absolutize
  where absolutize path -- avoid the call to `getCurrentDirectory` if we can
          | isRelative path = fmap (</> path) getCurrentDirectory
          | otherwise       = return path

-- | Make a path absolute by prepending the current directory (if it isn't
-- already absolute) and applying @'normalise'@ to the result.
-- Also remove trailing /'s
--
-- The operation may fail with the same exceptions as @'getCurrentDirectory'@.
makeAbsolute' :: FilePath -> IO FilePath
makeAbsolute' = fmap (dropWhileEnd (== '/')) . makeAbsolute

-- | Use ":cmd rr args" to reload and rerun in ghci
rr :: String ->  IO String
rr args = return $ ":reload\n:main " ++ args ++ "\n"
