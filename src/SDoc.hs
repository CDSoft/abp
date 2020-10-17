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

{-# LANGUAGE OverloadedStrings #-}

module SDoc
    ( sDoc
    )
where

import Config
import qualified Data.Text as T
import Dependencies
import Environment
import Tools

import Text.Pandoc

sDoc :: EnvMVar -> (Pandoc -> IO Pandoc) -> Block -> IO [Block]
sDoc e abp cb@(Div _attr@(_blockId, _classes, namevals) _contents) =
    parse $ lookup kSDoc namevals

    where

        parse :: Maybe T.Text -> IO [Block]

        parse (Just filename) = do
            let shift = maybe 0 atoi $ lookup kShift namevals
            (name, content) <- trackFile e (T.unpack filename)
            let (lang, delimiters) = getCommentDelimiters name
            let meta = nullMeta
            blocks <- splitBlocks lang delimiters 1 content
            let doc = shiftTitles shift $ Pandoc meta blocks
            Pandoc _ blocks' <- abp doc
            return blocks'

        parse Nothing = return [cb]

sDoc _ _ x = return [x]

splitBlocks :: T.Text -> (T.Text, T.Text) -> Int -> T.Text -> IO [Block]
splitBlocks lang (open, close) = skipEmptyLines
    where
        skipEmptyLines :: Int -> T.Text -> IO [Block]
        skipEmptyLines line content
            | T.null content = return []
            | T.head content == '\n' = skipEmptyLines (line+1) (T.tail content)
            | otherwise = extractCode line content

        extractCode :: Int -> T.Text -> IO [Block]
        extractCode line content =
            case extractTo open line content of
                (_, "", "") -> return []
                (line', "", content') -> extractDoc line' content'
                (line', code, content') -> do
                    let attr = ( ""
                                , [lang, "numberLines"]
                                , [("startFrom", T.pack (show line))]
                                )
                    let block = CodeBlock attr code
                    blocks <- extractDoc line' content'
                    return $ block : blocks

        extractDoc :: Int -> T.Text -> IO [Block]
        extractDoc line content =
            case extractTo close line content of
                (_, "", "") -> return []
                (line', "", content') -> extractCode line' content'
                (line', doc, content') -> do
                    Pandoc _ block <- parseDoc Nothing doc
                    blocks <- skipEmptyLines line' content'
                    return $ block ++ blocks

extractTo :: T.Text -> Int -> T.Text -> (Int, T.Text, T.Text)
extractTo delimiter line content = (line', block, content'')
    where
        (block, content') = T.breakOn delimiter content
        content'' = T.drop (T.length delimiter) content'
        line' = line + T.count "\n" block

shiftTitles :: Int -> Pandoc -> Pandoc
shiftTitles shift = bottomUp shiftTitle
    where
        shiftTitle :: Block -> Block
        shiftTitle (Header level attr inlines) = Header (level+shift) attr inlines
        shiftTitle x = x

getCommentDelimiters :: FilePath -> (T.Text, (T.Text, T.Text))
getCommentDelimiters name =
    let ext = getExt name
    in case lookup ext kSDocSyntax of
            Just delimiters -> delimiters
            Nothing -> error $ name ++ ": unknown source code language"

getExt :: FilePath -> String
getExt name = ext name name
    where
        ext ('.':s) _ = ext s s
        ext (_:s) current = ext s current
        ext [] current = current
