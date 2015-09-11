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
  , exitEither
  , freshFileName
  , inHomeDir
  , makeAbsolute
  , makeAbsolute'
  , readLnRetry
  , readProcessE
  , rr
  ) where
import Control.Applicative
import Control.Error
import Control.Monad
import Control.Monad.Error
import Data.List
import Data.Maybe
import Data.Typeable
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Text.Read

import KitchenSink.Control

-- | Put String into clipboard using xsel
clipboard :: String -> IO ()
clipboard = void . readProcess "xsel" ["--input","--clipboard"]

-- | Either write to stderr and exitFailure or write to stdout and exitSuccess.
exitEither :: Either String String -> IO ()
exitEither (Left  msg) = hPutStr stderr msg >> exitFailure
exitEither (Right msg) = hPutStr stdout msg >> exitSuccess

-- | Find an unused filename similar to a given name.
--
-- freshFileName prefix "dir/name" will find a name in the form
-- dir/prefixname-n where n is from [1..]
freshFileName :: String -> FilePath -> IO FilePath
freshFileName prefix path
  = fromJust <$> findM doesNotExist names
  where
    (dir, name) = splitFileName path
    doesNotExist f = not <$> (doesFileExist f <||> doesDirectoryExist f)
    names = path : map (\n -> dir </> concat [prefix,name,"-",show n]) [(1::Int)..]

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

-- | 'System.Process.readProcessWithExitCode' with command failure and
-- exceptions captured inside EitherT.
--
-- Returns the contents of stdout on success.
-- Returns the exception message or the contents of stderr on failure.
readProcessE :: String -> [String] -> String -> EitherT String IO String
readProcessE cmd args input = do
  (ec, out, err) <- scriptIO $ readProcessWithExitCode cmd args input
  if ec == ExitSuccess then return out else throwError err

-- | Use ":cmd rr args" to reload and rerun in ghci
rr :: String ->  IO String
rr args = return $ ":reload\n:main " ++ args ++ "\n"
