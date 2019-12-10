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

module Include
    ( includeBlock
    )
where

import Config
import Tools
import UTF8

import Data.List
import qualified Data.Text as T
import Text.Pandoc

includeBlock :: (Pandoc -> IO Pandoc) -> Block -> IO [Block]

includeBlock _abp cb@(CodeBlock attr@(_blockId, _classes, namevals) _contents) =
    case lookup kInclude namevals of
        Just f  -> do
            newContents <- readFileUTF8 =<< expandPath f
            let newContents' = case (atoi <$> lookup kFromLine namevals, atoi <$> lookup kToLine namevals) of
                    (Nothing, Nothing)      -> newContents
                    (Just from, Nothing)    -> unlines $ drop (from-1) $ lines newContents
                    (Nothing, Just to)      -> unlines $ take to $ lines newContents
                    (Just from, Just to)    -> unlines $ take (to-from+1) $ drop (from-1) $ lines newContents
            let attr' = cleanAttr [] [kInclude, kFromLine, kToLine] attr
            return [CodeBlock attr' newContents']
        Nothing -> return [cb]

includeBlock abp d@(Div (_blockId, _classes, namevals) _contents) =
    case lookup kInclude namevals of
        Just f -> do
            let shift = maybe 0 atoi $ lookup kShift namevals
            name <- expandPath f
            newContents <- readFileUTF8 name
            let reader = makeReader name
            Pandoc _ blocks <- do
                doc <- reader (T.pack newContents)
                let shifted = shiftTitles shift doc
                abp shifted
            return blocks
        Nothing -> return [d]

includeBlock _ x = return [x]

makeReader :: FilePath -> (T.Text -> IO Pandoc)
makeReader name = runIOorExplode . reader options
    where
        options = def
            { readerExtensions = pandocExtensions
            }
        reader
            | ".md" `isSuffixOf` name = readMarkdown
            |  ".rst" `isSuffixOf` name = readRST
            |  ".latex" `isSuffixOf` name = readLaTeX
            |  ".html" `isSuffixOf` name = readHtml
            | otherwise = error $ "Unknown file format: " ++ name

shiftTitles :: Int -> Pandoc -> Pandoc
shiftTitles shift = bottomUp shiftTitle
    where
        shiftTitle :: Block -> Block
        shiftTitle (Header level attr inlines) = Header (level+shift) attr inlines
        shiftTitle x = x
