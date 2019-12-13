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
    http://cdsoft.fr/abp
-}

module AbstractProcessor
    ( abp
    )
where

import CSV
import Comment
import Diagram
import Environment
import Expand
import Include
import Script
import UTF8

import Data.Data
import System.IO
import Text.Pandoc.Generic
import Text.Pandoc.JSON

abp :: Maybe Format -> Pandoc -> IO Pandoc
abp maybeFormat document = do
    setUTF8Encoding stdin
    setUTF8Encoding stdout
    e <- newEnv maybeFormat
    diagramEnv e
    let abpFilter doc = do
            doc' <- expandDoc e abpFilter doc   -- variable expansion, must be done before other filters
            e' <- readEnv e
            inject doc'
                >>= pf commentBlock
                >>= pf (includeBlock abpFilter)
                >>= pf (scriptInline e')
                >>= pf (scriptBlock e')
                >>= pf (diagramBlock e')
                >>= pf csvBlock
    abpFilter document

inject :: Pandoc -> IO Pandoc
inject = return

-- make a Pandoc filter from a Block -> IO [Block] function
pf :: Data a => (a -> IO [a]) -> (Pandoc -> IO Pandoc)
pf = bottomUpM . cat

-- turns a filter from Block -> IO [Block] to [Block] -> IO [Block]
cat :: Monad m => (a -> m [a]) -> [a] -> m [a]
cat f xs = concat <$> mapM f xs
