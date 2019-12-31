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

-- TODO : if[n]def : tester aussi les boolean (yes/no/true/false/0/1) ccase indépendant

module Expand
    ( expandDoc
    , expandString
    )
where

import Config
import Dependencies
import Environment
import Tools

import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Network.URI.Encode
import Text.Pandoc
import Text.Pandoc.Walk

import System.IO
--
--TODO : meta doit pouvoir contenir du lua et du yaml!
--          si yaml échoue, utiliser lua

{- Walk through the whole document

    1. expand meta definitions
    2. expand blocks
        - filter blocks
        - load definition
    3. expand inlines
        - filter inlines
        - expand strings

-}

expandDoc :: Env -> Pandoc -> IO Pandoc
expandDoc e (Pandoc meta blocks) = do
    --hPrint stderr ("expandDoc", blocks)
    meta' <- expandMeta e meta
    blocks' <- mapM (walkM (expandBlock e)) blocks
               >>= mapM (walkM (expandInline e))
    --hPrint stderr ("expandDoc => ", blocks')
    return $ Pandoc meta' blocks'

expandMeta :: Env -> Meta -> IO Meta
expandMeta e meta = do
    meta' <- forM (M.toList (unMeta meta)) (expandMetaDef e)
    --meta' <- M.mapM (expandMetaDef e) (unMeta meta)
    return Meta { unMeta = M.fromList meta' }
    --defs <- M.fromList <$> mapM (expandMetaDef e) (M.toList (unMeta meta))
    --let meta' = Meta { unMeta = defs }
    --return meta'

expandMetaDef :: Env -> (String, MetaValue) -> IO (String, MetaValue)
expandMetaDef e (var, val) = do
    val' <- expandMetaValue e val
    case metaToInline val' of
        Just inline -> setVar e var =<< inlineToMarkdown inline
        Nothing -> return ()
    return (var, val')

expandMetaValue :: Env -> MetaValue -> IO MetaValue
expandMetaValue e = walkM (expandInline e)

{- Block/Inline filter -}
genericFilter :: Env -> Attr -> a -> IO a -> IO a
genericFilter e attrs disabledItem enabledItem = do
    disabled <- itemIsDisabled e attrs
    if disabled
        then return disabledItem
        else enabledItem

inlineFilter :: Env -> Attr -> IO Inline -> IO Inline
inlineFilter e attrs = genericFilter e attrs (Str "")

blockFilter :: Env -> Attr -> IO Block -> IO Block
blockFilter e attrs = genericFilter e attrs Null

itemIsDisabled :: Env -> Attr -> IO Bool
itemIsDisabled e attrs = not <$> itemIsEnabled e attrs

itemIsEnabled :: Env -> Attr -> IO Bool
itemIsEnabled e (_, _, namevals) = do
    let maybeDefName = lookup kIfdef namevals
        maybeVal = lookup kValue namevals
        maybeUndefName = lookup kIfndef namevals
    case (maybeDefName, maybeVal, maybeUndefName) of
            (Just defName, Nothing, _) -> do
                hPrint stderr =<< ((("defName: "++defName++"=")++) . show <$> getVar e defName)
                isTrue <$> getVar e defName
            (Just defName, Just value, _) -> (==Just value) <$> getVar e defName
            (Nothing, _, Just undefName) -> not . isTrue <$> getVar e undefName
            _ -> return True

isTrue :: Maybe String -> Bool
isTrue Nothing = False
isTrue (Just s) | map toLower s `elem` ["no", "n", "false", "nil"] = False
isTrue _ = True

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
expandInline :: Env -> Inline -> IO Inline
expandInline e x@(Code attrs _) = do
    attrs' <- expandAttrs e attrs
    rawFilter attrs' x $ inlineFilter e attrs' (expandInline' e x)
expandInline e x@(Link attrs _ _) = do
    attrs' <- expandAttrs e attrs
    rawFilter attrs' x $ inlineFilter e attrs' (expandInline' e x)
expandInline e x@(Image attrs _ _) = do
    attrs' <- expandAttrs e attrs
    rawFilter attrs' x $ inlineFilter e attrs' (expandInline' e x)
expandInline e x@(Span attrs _) = do
    attrs' <- expandAttrs e attrs
    rawFilter attrs' x $ inlineFilter e attrs' (expandInline' e x)
expandInline e x = expandInline' e x

{- expand strings -}
expandInline' :: Env -> Inline -> IO Inline
expandInline' e (Str s) = markdownToInline noFilter =<< expandString e s -- Str <$> expandString' e s
expandInline' e (Code attrs s) = Code <$> expandAttr e attrs <*> expandString e s
expandInline' e (Math mathType s) = Math mathType <$> expandString e s
expandInline' e (RawInline fmt s) = RawInline fmt <$> expandString e s
expandInline' e (Link attrs x (url, title)) = Link <$> expandAttr e attrs <*> return x <*> ( (,) <$> expandURL e url <*> expandString e title )
expandInline' e (Image attrs x (url, title)) = Image <$> expandAttr e attrs <*> return x <*> ( (,) <$> expandURL e url <*> expandString e title )
expandInline' e (Span attrs x) = Span <$> expandAttr e attrs <*> return x
expandInline' _ x = return x

expandAttrs :: Env -> Attr -> IO Attr
expandAttrs e (blockId, classes, namevals) = do
    blockId' <- expandString e blockId
    classes' <- mapM (expandString e) classes
    namevals' <- forM namevals $ \(name, val) -> do
        name' <- expandString e name
        val' <- expandString e val
        return (name', val')
    return (blockId', classes', namevals')

expandURL :: Env -> String -> IO String
expandURL e url = encodeWith allowed <$> expandString e (decode url)
    where
        allowed c = (c `elem` ":/@#?=") || isAllowed c

{- Expand blocks

    1. filter blocks
    2. load definitions
    3. expand strings

-}

{- filter blocks -}
expandBlock :: Env -> Block -> IO Block
expandBlock e x@(CodeBlock attrs _) = do
    attrs' <- expandAttrs e attrs
    rawFilter attrs' x $ blockFilter e attrs' (expandBlock' e x)
expandBlock e x@(Header _ attrs _) = do
    attrs' <- expandAttrs e attrs
    rawFilter attrs' x $ blockFilter e attrs' (expandBlock' e x)
expandBlock e x@(Div attrs _) = do
    attrs' <- expandAttrs e attrs
    rawFilter attrs' x $ blockFilter e attrs' (expandBlock' e x)
expandBlock e x = expandBlock' e x

expandBlock' :: Env -> Block -> IO Block

{- load definitions -}
expandBlock' e cb@(CodeBlock (_blockId, classes, namevals) contents) =
    case (maybeExternalMetaFile, isMetaClass) of
        (Just externalMetaFile, _) -> expandMetaFromFile externalMetaFile
        (Nothing, True) -> expandMetaFromString contents
        (Nothing, False) -> expandBlock'' e cb -- not a definition block => expand strings
    where
        isMetaClass = kMeta `elem` classes
        maybeExternalMetaFile = lookup kMeta namevals

        expandMetaFromFile name = do
            void $ trackFile e name
            -- TODO : yaml or lua
            runFile e name
            return Null

        expandMetaFromString s = do
            -- TODO : yaml or lua
            runString e s
            return Null

{- expand strings -}
expandBlock' e x = expandBlock'' e x

expandBlock'' :: Env -> Block -> IO Block
expandBlock'' e (CodeBlock attrs s) = CodeBlock <$> expandAttr e attrs <*> expandString e s
expandBlock'' e (RawBlock fmt s) = RawBlock fmt <$> expandString e s
expandBlock'' e (Header level attrs x) = Header level <$> expandAttr e attrs <*> return x
expandBlock'' e (Div attrs x) = Div <$> expandAttr e attrs <*> return x
expandBlock'' _ x = return x

expandAttr :: Env -> Attr -> IO Attr
expandAttr e (blockId, classes, namevals) = do
    blockId' <- expandString e blockId
    classes' <- forM classes (expandString e)
    namevals' <- forM namevals $ \(name, val) -> (name,) <$> expandString e val
    return (blockId', classes', namevals')

expandString :: Env -> String -> IO String
expandString e = expand

    where

        expand :: String -> IO String
        expand cs = expandRegularText cs ""

        expandRegularText :: String -> String -> IO String
        expandRegularText str@(c:cs) revCurrent =
            case stripPrefix kVarOpen str of
                Just cs' -> (reverse revCurrent ++) <$> expandVar cs' ""
                Nothing -> expandRegularText cs (c:revCurrent)
        expandRegularText [] revCurrent = return (reverse revCurrent)

        expandVar :: String -> String -> IO String
        expandVar str@(c:cs) revName =
            let name = reverse revName
            in case stripPrefix kVarClose str of
                Just cs' -> do
                    val <- fromMaybe (kVarOpen++name++kVarClose) <$> evalString e name
                    (val ++) <$> expandRegularText cs' ""
                Nothing -> expandVar cs (c:revName)
        expandVar [] revName = expandRegularText [] (reverse (kVarOpen++reverse revName))
