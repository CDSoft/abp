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

{-# LANGUAGE TupleSections #-}

module Expand
where

import Config
import Environment
import Tools
import UTF8

import Control.Concurrent.MVar
import Control.Monad
import Data.Char (isSpace)
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Network.URI.Encode
import Text.Pandoc.JSON
import Text.Pandoc.Walk

{- Walk trhoug the whole document

    1. expand meta definitions
    2. expand blocks
        - filter blocks
        - load definition
    3. expand inlines
        - filter inlines
        - expand strings

-}

expandDoc :: EnvMVar -> Pandoc -> IO Pandoc
expandDoc e (Pandoc meta blocks) = do
    meta' <- expandMeta e meta
    blocks' <- mapM (walkM (expandBlock e)) blocks
               >>= mapM (walkM (expandInline e))
    return $ Pandoc meta' blocks'

expandMeta :: EnvMVar -> Meta -> IO Meta
expandMeta e meta = do
    defs <- M.fromList <$> mapM (expandMetaDef e) (M.toList (unMeta meta))
    let meta' = Meta { unMeta = defs }
    return meta'

expandMetaDef :: EnvMVar -> (String, MetaValue) -> IO (String, MetaValue)
expandMetaDef e (var, val) = do
    val' <- expandMetaValue e val
    forM_ (metaValueToInline val') (setVarIO e var)
    return (var, val')

expandMetaValue :: EnvMVar -> MetaValue -> IO MetaValue
expandMetaValue e = walkM (expandInline e)

{- Block/Inline filter -}
genericFilter :: EnvMVar -> Attr -> a -> IO a -> IO a
genericFilter e attrs disabledItem enabledItem = do
    disabled <- isDisabled e attrs
    if disabled
        then return disabledItem
        else enabledItem

inlineFilter :: EnvMVar -> Attr -> IO Inline -> IO Inline
inlineFilter e attrs = genericFilter e attrs (Str "")

blockFilter :: EnvMVar -> Attr -> IO Block -> IO Block
blockFilter e attrs = genericFilter e attrs Null

isDisabled :: EnvMVar -> Attr -> IO Bool
isDisabled e attrs = not <$> isEnabled e attrs

isEnabled :: EnvMVar -> Attr -> IO Bool
isEnabled e (_, _, namevals) = do
    let maybeDefName = lookup kIfdef namevals
        maybeVal = lookup kValue namevals
        maybeUndefName = lookup kIfndef namevals
    case (maybeDefName, maybeVal, maybeUndefName) of
            (Just defName, Nothing, _) -> isJust <$> (getVarIO e =<< expandString' e defName)
            (Just defName, Just value, _) -> do
                                                defName' <- expandString' e defName
                                                value' <- expandString' e value
                                                (==Just value') <$> getVar'IO e defName'
            (Nothing, _, Just undefName) -> isNothing <$> (getVarIO e =<< expandString' e undefName)
            _ -> return True

rawFilter :: Attr -> a -> IO a -> IO a
rawFilter attrs x f
    | isRaw attrs = return x
    | otherwise = f

isRaw :: Attr -> Bool
isRaw (_, classes, _) = kRaw `elem` classes

{- Expand inlines

    1. filter inlines
    2. expand strings

-}

{- filter inlines -}
expandInline :: EnvMVar -> Inline -> IO Inline
expandInline e x@(Code attrs _) = rawFilter attrs x $ inlineFilter e attrs (expandInline' e x)
expandInline e x@(Link attrs _ _) = rawFilter attrs x $ inlineFilter e attrs (expandInline' e x)
expandInline e x@(Image attrs _ _) = rawFilter attrs x $ inlineFilter e attrs (expandInline' e x)
expandInline e x@(Span attrs _) = rawFilter attrs x $ inlineFilter e attrs (expandInline' e x)
expandInline e x = expandInline' e x

{- expand strings -}
expandInline' :: EnvMVar -> Inline -> IO Inline
expandInline' e (Str s) = Str <$> expandString' e s
expandInline' e (Code attrs s) = Code <$> expandAttr e attrs <*> expandString' e s
expandInline' e (Math mathType s) = Math mathType <$> expandString' e s
expandInline' e (RawInline fmt s) = RawInline fmt <$> expandString' e s
expandInline' e (Link attrs x (url, title)) = Link <$> expandAttr e attrs <*> return x <*> ( (,) <$> expandURL e url <*> expandString' e title )
expandInline' e (Image attrs x (url, title)) = Image <$> expandAttr e attrs <*> return x <*> ( (,) <$> expandURL e url <*> expandString' e title )
expandInline' e (Span attrs x) = Span <$> expandAttr e attrs <*> return x
expandInline' _ x = return x

expandURL :: EnvMVar -> String -> IO String
expandURL e url = encodeWith allowed <$> expandString' e (decode url)
    where
        allowed c = (c `elem` ":/@#?=") || isAllowed c

{- Expand blocks

    1. filter blocks
    2. load definitions
    3. expand strings

-}

{- filter blocks -}
expandBlock :: EnvMVar -> Block -> IO Block
expandBlock e x@(CodeBlock attrs _) = rawFilter attrs x $ blockFilter e attrs (expandBlock' e x)
expandBlock e x@(Header _ attrs _) = rawFilter attrs x $ blockFilter e attrs (expandBlock' e x)
expandBlock e x@(Div attrs _) = rawFilter attrs x $ blockFilter e attrs (expandBlock' e x)
expandBlock e x = expandBlock' e x

expandBlock' :: EnvMVar -> Block -> IO Block

{- load definitions -}
expandBlock' e (CodeBlock (_blockId, classes, namevals) contents)
    | isJust externalMetaFile = expandMetaFromFile externalMetaFile
    | isMetaClass = expandMetaFromString contents
    where
        isMetaClass = kMeta `elem` classes
        externalMetaFile = lookup kMeta namevals

        expandMetaFromFile (Just name) = do
            defs <- expandString' e name >>= readFileUTF8
            expandMetaFromString (unlines [defs, contents])
        expandMetaFromFile Nothing = expandMetaFromString contents

        expandMetaFromString s = do
            forM_ (lines s) parseDef
            return Null

        parseDef s = do
            let s1 = dropWhile (\c -> isSpace c || c == '-') s
                (var, s2) = break (\c -> isSpace c || c == ':') s1
                s3 = dropWhile isSpace s2
                val = case s3 of
                        ':':s4 -> reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace s4
                        _ -> ""
            val' <- expandString e val
            setVarIO e var val'

{- expand strings -}
expandBlock' e x = expandBlock'' e x

expandBlock'' :: EnvMVar -> Block -> IO Block
expandBlock'' e (CodeBlock attrs s) = CodeBlock <$> expandAttr e attrs <*> expandString' e s
expandBlock'' e (RawBlock fmt s) = RawBlock fmt <$> expandString' e s
expandBlock'' e (Header level attrs x) = Header level <$> expandAttr e attrs <*> return x
expandBlock'' e (Div attrs x) = Div <$> expandAttr e attrs <*> return x
expandBlock'' _ x = return x

expandAttr :: EnvMVar -> Attr -> IO Attr
expandAttr e (blockId, classes, namevals) = do
    blockId' <- expandString' e blockId
    classes' <- forM classes (expandString' e)
    namevals' <- forM namevals $ \(name, val) -> (name,) <$> expandString' e val
    return (blockId', classes', namevals')

expandString :: EnvMVar -> String -> IO Inline
expandString e s = do
    vs <- vars <$> readMVar e
    return $ case expand vs s of
        [inline] -> inline
        inlines -> Span nullAttr inlines

    where

        expand :: [(String, Inline)] -> String -> [Inline]
        expand vs cs = expandRegularText vs cs ""

        expandRegularText vs str@(c:cs) revCurrent =
            case stripPrefix kVarOpen str of
                Just cs' -> Str (reverse revCurrent) : expandVar vs cs' ""
                Nothing -> expandRegularText vs cs (c:revCurrent)
        expandRegularText _ [] revCurrent = [Str (reverse revCurrent)]

        expandVar vs str@(c:cs) revName =
            let name = reverse revName
            in case stripPrefix kVarClose str of
                Just cs' -> case lookup name vs of
                    Just val -> val : expandRegularText vs cs' ""
                    Nothing -> expandRegularText vs cs' (reverse (kVarOpen++name++kVarClose))
                Nothing -> expandVar vs cs (c:revName)
        expandVar vs [] revName = expandRegularText vs [] (reverse (kVarOpen++reverse revName))

expandString' :: EnvMVar -> String -> IO String
expandString' e s = inlineToString <$> expandString e s

metaValueToInline :: MetaValue -> Maybe Inline
metaValueToInline (MetaString s) = Just $ Str s
metaValueToInline (MetaInlines xs) = Just $ Span nullAttr xs
metaValueToInline (MetaList xs) = Just $ Span nullAttr $ intersperse (Span nullAttr [Str ",", Space]) $ mapMaybe metaValueToInline xs
metaValueToInline _ = Nothing
