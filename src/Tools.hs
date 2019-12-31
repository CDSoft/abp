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
    ( inlineToPlainText
    , inlineToMarkdown
    , markdownToInline
    , noFilter
    , ljust
    , atoi
    , expandPath
    , cleanAttr
    , parseDoc
    , metaToInline
    )
where

import Data.Bool
import Data.List
import Data.List.Extra
import Data.Maybe
import qualified Data.Text as T
import System.Directory
import System.FilePath.Posix
import Text.Pandoc

inlineToPlainText :: Inline -> IO String
inlineToPlainText inline = trim . T.unpack <$> runIOorExplode (writer doc)
    where
        doc = Pandoc nullMeta [Plain [inline]]
        writer = writePlain def

inlineToMarkdown :: Inline -> IO String
inlineToMarkdown inline = trim . T.unpack <$> runIOorExplode (writer doc)
    where
        doc = Pandoc nullMeta [Plain [inline]]
        writer = writeMarkdown def

ljust :: Int -> String -> String
ljust w s = s ++ replicate (w - length s) ' '

atoi :: String -> Int
atoi = read

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

markdownToInline :: (Pandoc -> IO Pandoc) -> String -> IO Inline
markdownToInline abp s = do
    Pandoc _ blocks <- parseDoc Nothing s >>= abp
    return $ blocksToInline s blocks

noFilter :: Pandoc -> IO Pandoc
noFilter = return

blocksToInline :: String -> [Block] -> Inline
blocksToInline _ [] = Span nullAttr []
blocksToInline _ [Plain [x]] = x
blocksToInline _ [Plain xs] = Span nullAttr xs
blocksToInline _ [Para [x]] = x
blocksToInline _ [Para xs] = Span nullAttr xs
blocksToInline _ [LineBlock [[x]]] = x
blocksToInline s [Div _ blocks] = blocksToInline s blocks
blocksToInline s _ = error $ "invalid inline text: " ++ show s

metaToInline :: MetaValue -> Maybe Inline
metaToInline (MetaMap _) = Nothing
metaToInline (MetaList []) = Just $ Span nullAttr []
metaToInline (MetaList [x]) = metaToInline x
metaToInline (MetaList xs) = Just $ Span nullAttr $ intersperse (Span nullAttr [Str ",", Space]) (mapMaybe metaToInline xs)
metaToInline (MetaBool b) = Just $ Str $ bool "false" "true" b
metaToInline (MetaString s) = Just $ Str s
metaToInline (MetaInlines []) = Just $ Span nullAttr []
metaToInline (MetaInlines [x]) = Just x
metaToInline (MetaInlines xs) = Just $ Span nullAttr xs
metaToInline _ = Nothing

