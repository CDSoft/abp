{-
    This file is part of ABP.

    ABP is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    ABP is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with ABP.  If not, see <https://www.gnu.org/licenses/>.

    For further information about ABP you can visit
    http://cdelord.fr/abp
-}

module Main where

import AbstractProcessor
import Help

import qualified Data.Text.IO as T
import System.Environment
import Text.Pandoc.JSON

main :: IO ()
main = getArgs >>= doArgs

doArgs :: [String] -> IO ()
doArgs ("-v":_) = putStrLn Help.copyright
doArgs ("-h":_) = putStrLn Help.help
doArgs ("emojis":_) = T.putStrLn =<< Help.emojisMarkdown
doArgs _ = toJSONFilter abp
