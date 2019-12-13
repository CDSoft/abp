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

module Tools
    ( inlineToString
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

inlineToString :: Inline -> String
inlineToString (Str s) = s
inlineToString (Emph xs) = concatMap inlineToString xs
inlineToString (Strong xs) = concatMap inlineToString xs
inlineToString (Strikeout xs) = concatMap inlineToString xs
inlineToString (Superscript xs) = concatMap inlineToString xs
inlineToString (Subscript xs) = concatMap inlineToString xs
inlineToString (SmallCaps xs) = concatMap inlineToString xs
inlineToString (Quoted _ xs) = concatMap inlineToString xs
inlineToString (Cite _ xs) = concatMap inlineToString xs
inlineToString (Code _ x) = x
inlineToString Space = " "
inlineToString SoftBreak = " "
inlineToString LineBreak = "\n"
inlineToString (Math _ x) = x
inlineToString (RawInline _ x) = x
inlineToString (Link _ _ (_, x)) = x
inlineToString (Image _ _ (_, x)) = x
inlineToString (Note _) = ""
inlineToString (Span _ xs) = concatMap inlineToString xs

ljust :: Int -> String -> String
ljust w s = s ++ replicate (w - length s) ' '

atoi :: String -> Int
atoi s = case reads s of
            [(i, "")] -> i
            _ -> error $ "Integer expected: " ++ s

expandPath :: FilePath -> IO FilePath
expandPath ('~':'/':path) = (</> path) <$> getHomeDirectory
expandPath path = return path

cleanAttr :: [String] -> [String] -> Attr -> Attr
cleanAttr classesToClean namesToClean (blockId, classes, namevals) =
    ( blockId
    , filter (`notElem` classesToClean) classes
    , filter ((`notElem` namesToClean) . fst) namevals
    )

parseDoc :: Maybe FilePath -> String -> IO Pandoc
parseDoc maybeName = runIOorExplode . reader options . T.pack
    where
        options = def
            { readerExtensions = pandocExtensions
            }
        reader = case maybeName of
            Just name | ".md" `isSuffixOf` name -> readMarkdown
            Just name |  ".rst" `isSuffixOf` name -> readRST
            Just name |  ".latex" `isSuffixOf` name -> readLaTeX
            Just name |  ".html" `isSuffixOf` name -> readHtml
            Just name -> error $ "Unknown file format: " ++ name
            Nothing -> readMarkdown
