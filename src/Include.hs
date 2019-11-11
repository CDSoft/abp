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
where

import Config
import Tools
import UTF8

import Text.Pandoc.JSON

includeBlock :: Block -> IO Block
includeBlock cb@(CodeBlock (blockId, classes, namevals) _contents) =
    case lookup kInclude namevals of
        Just f  -> do
            newContents <- readFileUTF8 =<< expandPath f
            let newContents' = case (lookup kFromLine namevals, lookup kToLine namevals) of
                    (Nothing, Nothing)      -> newContents
                    (Just from, Nothing)    -> let from' = atoi from
                                               in unlines $ drop (from'-1) $ lines newContents
                    (Nothing, Just to)      -> let to' = atoi to
                                               in unlines $ take to' $ lines newContents
                    (Just from, Just to)    -> let (from', to') = (atoi from, atoi to)
                                               in unlines $ take (to'-from'+1) $ drop (from'-1) $ lines newContents
            return $ CodeBlock (blockId, classes, namevals) newContents'
        Nothing -> return cb
includeBlock x = return x