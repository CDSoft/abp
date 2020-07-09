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

module Tools
    ( inlineToPlainText
    , ljust
    , atoi
    , expandPath
    , cleanAttr
    , parseDoc
    )
where

import Data.List
import qualified Data.Text as T
import System.Directory
import System.FilePath.Posix
import Text.Pandoc

inlineToPlainText :: Inline -> IO T.Text
inlineToPlainText inline = T.strip <$> runIOorExplode (writer doc)
    where
        doc = Pandoc nullMeta [Plain [inline]]
        writer = writePlain def

ljust :: Int -> String -> String
ljust w s = s ++ replicate (w - length s) ' '

atoi :: T.Text -> Int
atoi = read . T.unpack

expandPath :: FilePath -> IO FilePath
expandPath ('~':'/':path) = (</> path) <$> getHomeDirectory
expandPath path = return path

cleanAttr :: [T.Text] -> [T.Text] -> Attr -> Attr
cleanAttr classesToClean namesToClean (blockId, classes, namevals) =
    ( blockId
    , filter (`notElem` classesToClean) classes
    , filter ((`notElem` namesToClean) . fst) namevals
    )

parseDoc :: Maybe FilePath -> T.Text -> IO Pandoc
parseDoc maybeName = runIOorExplode . reader options
    where
        options = def
            { readerExtensions = pandocExtensions
            }
        reader = findReader maybeName
        defaultReader = readMarkdown
        knownReaders = [ (".md", readMarkdown)
                       , (".rst", readRST)
                       , (".latex", readLaTeX)
                       , (".html", readHtml)
                       ]
        findReader (Just name) = head ( [ r
                                        | (e, r) <- knownReaders
                                        , e `isSuffixOf` name
                                        ] ++ [defaultReader]
                                      )
        findReader Nothing = defaultReader
