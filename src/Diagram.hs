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

module Diagram
where

import Config
import Environment
import Expand
import Tools
import UTF8

import Control.Monad
import Crypto.Hash
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.List.Extra
import Data.Maybe
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
    e' <- readEnv e
    let envAbpPlantuml = case (getVar' e' kAbpPlantuml, getVar' e' kAbpPath) of
            (Just abpPlantuml, _) -> abpPlantuml
            (Nothing, Just abpPath) -> takeDirectory abpPath </> kPlantumlJar
            (Nothing, Nothing) -> kPlantumlJar
    setVarIO e kAbpPlantuml (Str envAbpPlantuml)
    forM_ kDiagramRenderers $ \(name, render) ->
        setVarIO e name =<< expandString e render

diagramBlock :: Env -> Block -> IO Block
diagramBlock e cb@(CodeBlock _attrs@(blockId, classes, namevals) contents) = do
    let maybeRender = lookup kRender namevals
        maybeImg = lookup kImg namevals
        maybeOutputPath = lookup kOut namevals
        maybeTarget = lookup kTarget namevals
        hashDigest = sha1 contents
    case maybeRender of
        Nothing -> return cb
        Just render ->
            withSystemTempFile "abp" $ \path handle -> do
                hWriteFileUTF8 handle contents
                hClose handle
                let img = case maybeImg of
                            Just img' -> makeImg hashDigest img'
                            Nothing -> error $ kImg ++ " field missing in a diagram block"
                out <- expandPath $ case maybeOutputPath of
                            Just out' -> out' </> takeFileName img
                            Nothing -> img
                let meta = out -<.> "meta"
                let metaContent = unlines [ "source: " ++ hashDigest
                                          , "render: " ++ render
                                          , "img: " ++ img
                                          , "out: " ++ out
                                          ]
                let render' = makeCmd path out render
                imgExists <- doesFileExist out
                metaExists <- doesFileExist meta
                oldMeta <- if metaExists then readFileUTF8 meta else return ""
                when (not imgExists || metaContent /= oldMeta) $ do
                    writeFileUTF8 meta metaContent
                    writeFileUTF8 path contents
                    renderDiagram e render' contents
                let title = fromMaybe "" (lookup kTitle namevals)
                let attrs' = ( blockId
                             , classes
                             , [ (name, val) | (name, val) <- namevals
                                             , name `notElem` [kRender, kImg, kOut, kTarget, kTitle]
                               ]
                              )
                let image = Image attrs' [ Str title ] (img, title)
                return $ case maybeTarget of
                    Just target -> Para [ Link nullAttr [ image ] (target, title) ]
                    Nothing -> Para [ image ]

diagramBlock _ x = return x

sha1 :: String -> String
sha1 s = show sourceHash
    where
        sourceHash = hash (BS.pack s) :: Digest SHA1


makeCmd :: String -> String -> String -> String
makeCmd src img = replaceArg
    where
        replaceArg :: String -> String
        replaceArg s
            | kDiagArgIn `isPrefixOf` s = src ++ replaceArg (dropPrefix kDiagArgIn s)
            | kDiagArgOut `isPrefixOf` s = img ++ replaceArg (dropPrefix kDiagArgOut s)
            | otherwise = case s of
                (c:cs) -> c : replaceArg cs
                [] -> []

makeImg :: String -> String -> String
makeImg img = replaceArg
    where
        replaceArg :: String -> String
        replaceArg s
            | kDiagArgOut `isPrefixOf` s = img ++ replaceArg (dropPrefix kDiagArgOut s)
            | otherwise = case s of
                (c:cs) -> c : replaceArg cs
                [] -> []

renderDiagram :: Env -> String -> String -> IO ()
renderDiagram e cmd contents = do
    (_, Just hOut, Just hErr, hProc) <- createProcess (shell cmd) { std_out = CreatePipe, std_err = CreatePipe }
    hSetEncoding hOut utf8
    _out <- SIO.hGetContents hOut
    err <- SIO.hGetContents hErr
    exitCode <- waitForProcess hProc
    case exitCode of
        ExitFailure _ -> do
            unless (quiet e) $ do
                hPutStrLn stderr "Diagram failed:"
                hPutStrLn stderr contents
                hPutStrLn stderr "Errors:"
                hPutStrLn stderr err
            exitWith exitCode
        ExitSuccess -> return ()
