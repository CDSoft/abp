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

import System.IO
import Text.Pandoc.Generic
import Text.Pandoc.JSON

abp :: Maybe Format -> Pandoc -> IO Pandoc
abp maybeFormat doc = do
    setUTF8Encoding stdin
    setUTF8Encoding stdout
    e <- newEnv maybeFormat
    diagramEnv e
    doc' <- expandDoc e doc                     -- variable expansion, must be done before other filters
    e' <- readEnv e
    bottomUpM commentBlock doc'
        >>= bottomUpM includeBlock
        >>= bottomUpM (scriptInline e')
        >>= bottomUpM (scriptBlock e')
        >>= bottomUpM (diagramBlock e')
        >>= bottomUpM csvBlock
