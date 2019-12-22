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
import Dependencies
import Environment
import Tools

import Text.Pandoc

includeBlock :: EnvMVar -> (Pandoc -> IO Pandoc) -> Block -> IO [Block]

includeBlock e _abp cb@(CodeBlock attr@(_blockId, _classes, namevals) _contents) =
    case lookup kInclude namevals of
        Just f  -> do
            (_, newContents) <- trackFile e f
            let newContents' = case (atoi <$> lookup kFromLine namevals, atoi <$> lookup kToLine namevals) of
                    (Nothing, Nothing)      -> newContents
                    (Just from, Nothing)    -> unlines $ drop (from-1) $ lines newContents
                    (Nothing, Just to)      -> unlines $ take to $ lines newContents
                    (Just from, Just to)    -> unlines $ drop (from-1) . take to $ lines newContents
            let attr' = cleanAttr [] [kInclude, kFromLine, kToLine] attr
            return [CodeBlock attr' newContents']
        Nothing -> return [cb]

includeBlock e abp d@(Div (_blockId, _classes, namevals) _contents) =
    case lookup kInclude namevals of
        Just f -> do
            let shift = maybe 0 atoi $ lookup kShift namevals
            (name, newContents) <- trackFile e f
            Pandoc _ blocks <- do
                doc <- parseDoc (Just name) newContents
                let shifted = shiftTitles shift doc
                abp shifted
            return blocks
        Nothing -> return [d]

includeBlock _ _ x = return [x]

shiftTitles :: Int -> Pandoc -> Pandoc
shiftTitles shift = bottomUp shiftTitle
    where
        shiftTitle :: Block -> Block
        shiftTitle (Header level attr inlines) = Header (level+shift) attr inlines
        shiftTitle x = x
