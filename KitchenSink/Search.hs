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

-- | Tree search.
module KitchenSink.Search where
import KitchenSink.Combinators
import Control.Parallel

-- | A simple depth first search
depthFirstSearch :: (a -> [a])  -- ^ successor function
                 -> (a -> Bool) -- ^ goal test
                 -> [a]         -- ^ inital states
                 -> Maybe a     -- ^ first found goal state if any
depthFirstSearch succs test = go
  where
    go [] = Nothing
    go (curr:rest)
        | test curr = Just curr
        | otherwise = recCurr <|> recRest
      where
        recCurr = go $ succs curr
        recRest = go rest

-- | A simple depth first search with added parallelism
parDepthFirstSearch :: Int          -- ^ maximum parallel depth
                    -> (a -> [a])   -- ^ successor function
                    -> (a -> Bool)  -- ^ goal test
                    -> [a]          -- ^ inital states
                    -> Maybe a      -- ^ first found goal state if any
parDepthFirstSearch depth succs test = go depth
  where
    go 0 fringe = depthFirstSearch succs test fringe
    go _ [] = Nothing
    go depth (curr:rest)
        | test curr = Just curr
        | otherwise = recCurr `par` recRest `pseq` (recCurr <|> recRest)
      where
        recCurr = go (depth - 1) $ succs curr
        recRest = go depth rest

