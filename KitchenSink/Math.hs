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

-- | Miscellaneous mathematics
module KitchenSink.Math
  ( tau, fibs, fac, primes, factors, divisors
  , showRadix, readRadix
  ) where
import KitchenSink.Combinators
import KitchenSink.ChainCmp
import KitchenSink.List
import Control.Arrow
import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.Char
import Data.List
import Data.Foldable (foldlM)
import Data.Function

-- | The circle constant
tau :: Floating a => a
tau = 2*pi

-- | The Fibonacci sequence
fibs :: [Integer]
fibs = fix $ scanl (+) 0 . (1:)

-- | factorial
fac :: Integer -> Integer
fac = product . enumFromTo 1

-- | The primes
primes :: [Integer]
primes = 2 : filter isPrime [3,5..]
  where isPrime n = all ((/=0) . mod n) . takeWhile ((<=n) . (^2)) $ primes

-- | A (slow) primality test
isPrime :: Integer -> Bool
isPrime n | n < 2     = False
          | otherwise = all ((/=0) . mod n) . takeWhile ((<=n) . (^2)) $ primes

-- | Find the factors of an integer
factors :: Integer -> [Integer]
factors n = unfoldr sepFctr (n, primes)
  where
    sepFctr (1, _ ) = Nothing
    sepFctr (n, ps) = let ps'@(fctr:_) = dropWhile ((/=0) . mod n) ps
                      in  Just (fctr, (div n fctr, ps'))

-- | Find the divisors of an integer
divisors :: Integer -> [Integer]
divisors = (1:) . map product . mix . powers
  where
    powers = map (scanl1 (*)) . group . factors
    mix = concatMap cartesianProduct . nonEmptySubseqs

-- | Show integers in a base between 2 and 35.
showRadix :: Integer -> Integer -> String
showRadix radix n
  | n < 0     = '-' : go (-n)
  | n == 0    = "0"
  | otherwise = go n
  where
    go n = doCmp (2 <=. radix <=. 35) ? reverse (unfoldr sepChars n) ?? error msg
    sepChars d = (d /= 0) ? (first toChar . swap) (d `divMod` radix)
    toChar i = doCmp (0 <=. i <. 10) ? (chr $ (fromInteger i) + ord '0')
                                    ?? (chr $ (fromInteger i) + ord 'A' - 10)
    msg = "showRadix: illeagal radix"

-- | Read integers from a base between 2 and 35.
readRadix :: Integer -> String -> Integer
readRadix radix cs = case cs of
    ('-':cs) -> (0-) $ go cs
    cs       -> go cs
  where
    go cs = join (doCmp (2 <=. radix <=. 35) ? foldlM joinChars 0 cs) ?? error msg
    joinChars i c = (radix*i +) <$> fromChar c
    fromChar c =  doCmp ('0' <=. c <. digit) ? (toInteger $ ord c - ord '0')
              <|> doCmp ('a' <=. c <. lower) ? (toInteger $ ord c - ord 'a' + 10)
              <|> doCmp ('A' <=. c <. upper) ? (toInteger $ ord c - ord 'A' + 10)
    digit = min (chr $ fromInteger radix + ord '0') (chr $ ord '9' + 1)
    lower = chr $ fromInteger radix + ord 'a' - 10
    upper = chr $ fromInteger radix + ord 'A' - 10
    msg = "readRadix: illeagal radix or no parse"

