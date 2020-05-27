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

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Expand
    ( expandDoc
    , expandString
    )
where

import Config
import qualified Data.Text as T
import Dependencies
import Environment
import Tools

import Control.Concurrent.MVar
import Control.Monad
import Data.Char (isSpace)
import Data.List
import Data.List.Extra
import qualified Data.Map as M
import Data.Maybe
import Network.URI.Encode
import Text.Pandoc
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

expandDoc :: EnvMVar -> (Pandoc -> IO Pandoc) -> Pandoc -> IO Pandoc
expandDoc e abp (Pandoc meta blocks) = do
    meta' <- expandMeta e meta
    blocks' <- mapM (walkM (expandBlock e abp)) blocks
               >>= mapM (walkM (expandInline e))
    return $ Pandoc meta' blocks'

expandMeta :: EnvMVar -> Meta -> IO Meta
expandMeta e meta = do
    defs <- M.fromList <$> mapM (expandMetaDef e) (M.toList (unMeta meta))
    let meta' = Meta { unMeta = defs }
    return meta'

expandMetaDef :: EnvMVar -> (T.Text, MetaValue) -> IO (T.Text, MetaValue)
expandMetaDef e (var, val) = do
    val' <- expandMetaValue e val
    forM_ (metaValueToInline val') (setVar e var)
    return (var, val')

expandMetaValue :: EnvMVar -> MetaValue -> IO MetaValue
expandMetaValue e = walkM (expandInline e)

{- Block/Inline filter -}
genericFilter :: EnvMVar -> Attr -> a -> IO a -> IO a
genericFilter e attrs disabledItem enabledItem = do
    disabled <- itemIsDisabled e attrs
    if disabled
        then return disabledItem
        else enabledItem

inlineFilter :: EnvMVar -> Attr -> IO Inline -> IO Inline
inlineFilter e attrs = genericFilter e attrs (Str "")

blockFilter :: EnvMVar -> Attr -> IO Block -> IO Block
blockFilter e attrs = genericFilter e attrs Null

itemIsDisabled :: EnvMVar -> Attr -> IO Bool
itemIsDisabled e attrs = not <$> itemIsEnabled e attrs

itemIsEnabled :: EnvMVar -> Attr -> IO Bool
itemIsEnabled e (_, _, namevals) = do
    let maybeDefName = lookup kIfdef namevals
        maybeVal = lookup kValue namevals
        maybeUndefName = lookup kIfndef namevals
    case (maybeDefName, maybeVal, maybeUndefName) of
            (Just defName, Nothing, _) -> isJust <$> (getVar e =<< expandString' e defName)
            (Just defName, Just value, _) -> do
                                                defName' <- expandString' e defName
                                                value' <- expandString' e value
                                                (==Just value') <$> getVarStr e defName'
            (Nothing, _, Just undefName) -> isNothing <$> (getVar e =<< expandString' e undefName)
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
expandInline' e (Str s) = expandString e s -- Str <$> expandString' e s
expandInline' e (Code attrs s) = Code <$> expandAttr e attrs <*> expandString' e s
expandInline' e (Math mathType s) = Math mathType <$> expandString' e s
expandInline' e (RawInline fmt s) = RawInline fmt <$> expandString' e s
expandInline' e (Link attrs x (url, title)) = Link <$> expandAttr e attrs <*> return x <*> ( (,) <$> expandURL e url <*> expandString' e title )
expandInline' e (Image attrs x (url, title)) = Image <$> expandAttr e attrs <*> return x <*> ( (,) <$> expandURL e url <*> expandString' e title )
expandInline' e (Span attrs x) = Span <$> expandAttr e attrs <*> return x
expandInline' _ x = return x

expandURL :: EnvMVar -> T.Text -> IO T.Text
expandURL e url = T.pack . encodeWith allowed . T.unpack <$> expandString' e ((T.pack . decode . T.unpack) url)
    where
        allowedChars :: String
        allowedChars = ":/@#?="
        allowed c = (c `elem` allowedChars) || isAllowed c

{- Expand blocks

    1. filter blocks
    2. load definitions
    3. expand strings

-}

{- filter blocks -}
expandBlock :: EnvMVar -> (Pandoc -> IO Pandoc) -> Block -> IO Block
expandBlock e abp x@(CodeBlock attrs _) = rawFilter attrs x $ blockFilter e attrs (expandBlock' e abp x)
expandBlock e abp x@(Header _ attrs _) = rawFilter attrs x $ blockFilter e attrs (expandBlock' e abp x)
expandBlock e abp x@(Div attrs _) = rawFilter attrs x $ blockFilter e attrs (expandBlock' e abp x)
expandBlock e abp x = expandBlock' e abp x

expandBlock' :: EnvMVar -> (Pandoc -> IO Pandoc) -> Block -> IO Block

{- load definitions -}
expandBlock' e abp cb@(CodeBlock (_blockId, classes, namevals) contents) =
    case (maybeExternalMetaFile, isMetaClass) of
        (Just externalMetaFile, _) -> expandMetaFromFile externalMetaFile
        (Nothing, True) -> expandMetaFromString (T.unpack contents)
        (Nothing, False) -> expandBlock'' e cb -- not a definition block => expand strings
    where
        isMetaClass = kMeta `elem` classes
        maybeExternalMetaFile = lookup kMeta namevals

        expandMetaFromFile name = do
            (_, defs) <- trackFile e =<< T.unpack <$> expandString' e name
            expandMetaFromString (T.unpack $ T.unlines [defs, contents])

        expandMetaFromString s = do
            forM_ (lines s) parseDef
            return Null

        parseDef s = do
            let s1 = dropWhile (\c -> isSpace c || c == '-') s
                (var, s2) = break (\c -> isSpace c || c == ':') s1
                s4 = trim $ case dropWhile isSpace s2 of
                        ':':s3 -> s3
                        s3 -> s3
            maybeInline <- stringToInline abp (T.pack s4)
            forM_ maybeInline (setVar e (T.pack var))

{- expand strings -}
expandBlock' e _ x = expandBlock'' e x

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

expandString :: EnvMVar -> T.Text -> IO Inline
expandString e s = do
    vs <- vars <$> readMVar e
    return $ case expand vs s of
        [inline] -> inline
        inlines -> Span nullAttr inlines

    where

        expand :: [(T.Text, Inline)] -> T.Text -> [Inline]
        expand vs cs = expandRegularText vs (T.unpack cs) ""

        expandRegularText :: [(T.Text, Inline)] -> String -> String -> [Inline]
        expandRegularText vs str@(c:cs) revCurrent =
            case stripPrefix kVarOpen str of
                Just cs' -> Str (T.pack $ reverse revCurrent) : expandVar vs cs' ""
                Nothing -> expandRegularText vs cs (c:revCurrent)
        expandRegularText _ [] revCurrent = [Str (T.pack $ reverse revCurrent)]

        expandVar :: [(T.Text, Inline)] -> String -> String -> [Inline]
        expandVar vs str@(c:cs) revName =
            let name = reverse revName
            in case stripPrefix kVarClose str of
                Just cs' -> case lookup (T.pack name) vs of
                    Just val -> val : expandRegularText vs cs' ""
                    Nothing -> expandRegularText vs cs' (reverse (kVarOpen++name++kVarClose))
                Nothing -> expandVar vs cs (c:revName)
        expandVar vs [] revName = expandRegularText vs [] (reverse (kVarOpen++reverse revName))

expandString' :: EnvMVar -> T.Text -> IO T.Text
expandString' e s = inlineToPlainText =<< expandString e s

metaValueToInline :: MetaValue -> Maybe Inline
metaValueToInline (MetaList xs) = Just $ Span nullAttr $ intersperse (Span nullAttr [Str ",", Space]) $ mapMaybe metaValueToInline xs
metaValueToInline (MetaBool True) = Just $ Str "true"
metaValueToInline (MetaBool False) = Just $ Str "false"
metaValueToInline (MetaString s) = Just $ Str s
metaValueToInline (MetaInlines xs) = Just $ Span nullAttr xs
metaValueToInline _ = Nothing

stringToInline :: (Pandoc -> IO Pandoc) -> T.Text -> IO (Maybe Inline)
stringToInline abp s = do
    Pandoc _ blocks <- parseDoc Nothing s >>= abp
    return $ blockToInline blocks

blockToInline :: [Block] -> Maybe Inline
blockToInline [] = Just $ Span nullAttr []
blockToInline [Plain [x]] = Just x
blockToInline [Plain xs] = Just $ Span nullAttr xs
blockToInline [Para [x]] = Just x
blockToInline [Para xs] = Just $ Span nullAttr xs
blockToInline [LineBlock [[x]]] = Just x
blockToInline [LineBlock [xs]] = Just $ Span nullAttr xs
blockToInline [Div _ blocks] = blockToInline blocks
blockToInline _ = Nothing
