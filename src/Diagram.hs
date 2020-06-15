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

module Diagram
    ( diagramEnv
    , diagramBlock
    )
where

import Config
import Environment
import Expand
import Tools
import UTF8

import Control.Monad
import Crypto.Hash
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List
import Data.List.Extra
import Data.Maybe
import qualified Data.Text as T
import System.Directory
import System.Exit
import System.FilePath.Posix
import System.IO
import qualified System.IO.Strict as SIO
import System.IO.Temp
import System.Process
import Text.Pandoc.JSON

diagramEnv :: EnvMVar -> IO ()
diagramEnv e = do
    storeCustomPath e kAbpPlantuml kPlantumlJar
    storeCustomPath e kAbpDitaa kDitaaJar
    e' <- readEnv e
    forM_ (kDiagramRenderers (format e')) $ \(name, render) ->
        setVar e name =<< expandString e render

storeCustomPath :: EnvMVar -> T.Text -> T.Text -> IO ()
storeCustomPath e envVarName defaultBaseName = do
    name <- getVarStr e envVarName
    path <- getVarStr e kAbpPath
    let value = case (name, path) of
            (Just customValue, _) -> customValue
            (Nothing, Just abpPath) -> T.pack $ takeDirectory (T.unpack abpPath) </> T.unpack defaultBaseName
            (Nothing, Nothing) -> defaultBaseName
    setVar e envVarName (Str value)

diagramBlock :: Env -> Block -> IO [Block]
diagramBlock e cb@(CodeBlock attr@(_blockId, _classes, namevals) contents) = do
    let maybeRender = lookup kRender namevals
        maybeImg = lookup kImg namevals
        maybeOutputPath = lookup kOut namevals
        maybeTarget = lookup kTarget namevals
        hashDigest = sha1 contents
    case maybeRender of
        Nothing -> return [cb]
        Just render -> do
            let ext = (T.pack . getExt . T.unpack) render
            let img = fromMaybe (error $ T.unpack kImg ++ " field missing in a diagram block") maybeImg
            out <- expandPath $ case maybeOutputPath of
                        Just out' -> T.unpack out' </> takeFileName (T.unpack img)
                        Nothing -> T.unpack img
            let meta = out ++ T.unpack ext ++ ".meta"
            let metaContent = T.unlines [ T.concat ["source: ", hashDigest]
                                        , T.concat ["render: ", render]
                                        , T.concat ["img: ", img]
                                        , T.concat ["out: ", T.pack out]
                                        ]
            imgExists <- doesFileExist out
            metaExists <- doesFileExist meta
            oldMeta <- if metaExists then readFileUTF8 meta else return ""
            when (not imgExists || metaContent /= oldMeta) $
                withSystemTempFile "abp" $ \path handle -> do
                    hWriteFileUTF8 handle (T.unpack contents)
                    hClose handle
                    writeFileUTF8 meta (T.unpack metaContent)
                    let render' = makeCmd path out (T.unpack render)
                    renderDiagram e render' contents
            let title = fromMaybe "" (lookup kTitle namevals)
            let attrs' = cleanAttr [] [kRender, kImg, kOut, kTarget, kTitle] attr
            let image = Image attrs' [ Str title ] (T.concat [img, ext], title)
            return $ case maybeTarget of
                Just target -> [Para [ Link nullAttr [ image ] (target, title) ]]
                Nothing -> [Para [ image ]]

diagramBlock _ x = return [x]

sha1 :: T.Text -> T.Text
sha1 s = T.pack $ show sourceHash
    where
        sourceHash = hash (BS.pack $ T.unpack s) :: Digest SHA1

getExt :: String -> String
getExt s | T.unpack kDiagArgOut `isPrefixOf` s =
    case dropPrefix (T.unpack kDiagArgOut) s of
        '.':s' -> case takeWhile isAlphaNum s' of
                    s''@(_:_) -> '.':s''
                    _ -> ""
        _ -> ""
getExt (_:s) = getExt s
getExt [] = ""

makeCmd :: String -> String -> String -> String
makeCmd src img = replace (T.unpack kDiagArgIn) src . replace (T.unpack kDiagArgOut) img

renderDiagram :: Env -> String -> T.Text -> IO ()
renderDiagram e cmd contents =
    withCreateProcess (shell cmd) { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe } $
        \(Just hIn) (Just hOut) (Just hErr) hProc -> do
            hClose hIn
            hSetEncoding hOut utf8
            _out <- SIO.hGetContents hOut
            err <- SIO.hGetContents hErr
            exitCode <- waitForProcess hProc
            case exitCode of
                ExitFailure _ -> do
                    unless (quiet e) $ do
                        hPutStrLn stderr "Diagram failed:"
                        hPutStrLn stderr (T.unpack contents)
                        hPutStrLn stderr "Errors:"
                        hPutStrLn stderr err
                    exitWith exitCode
                ExitSuccess -> return ()
