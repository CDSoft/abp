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

module Script
    ( scriptBlock
    , scriptInline
    )
where

import Config
import Environment
import Tools
import UTF8

import Control.Monad
import System.Exit
import System.IO
import System.IO.Temp
import Text.Pandoc.JSON

-- TODO: remove used attributes or classes

scriptBlock :: Env -> Block -> IO [Block]
scriptBlock e cb@(CodeBlock attr@(_blockId, _classes, namevals) contents) =
    case lookup kCmd namevals of
        Just cmd  -> do
            output <- runScript e cmd contents
            let (blockId', _classes', namevals') = cleanAttr [] [kCmd, kClasses] attr
            let classes' = case lookup kClasses namevals of
                            Just newClasses -> [newClasses]
                            Nothing -> []
            return [CodeBlock (blockId', classes', namevals') output]
        Nothing -> return [cb]
scriptBlock _ x = return [x]

scriptInline :: Env -> Inline -> IO [Inline]
scriptInline e c@(Code attr@(_blockId, _classes, namevals) contents) =
    case lookup kCmd namevals of
        Just cmd  -> do
            output <- trim <$> runScript e cmd contents
            let (blockId', _classes', namevals') = cleanAttr [] [kCmd, kClasses] attr
            let classes' = case lookup kClasses namevals of
                            Just newClasses -> [newClasses]
                            Nothing -> []
            return [Code (blockId', classes', namevals') output]
        Nothing -> return [c]
scriptInline _ x = return [x]

runScript :: Env -> String -> String -> IO String
runScript e cmd contents =
    withSystemTempFile "abp" $ \path handle -> do
        hWriteFileUTF8 handle contents
        hClose handle
        --setFileMode path 0o550
        res <- readProcessUTF8 (makeCmd cmd path) []
        case res of
            Left (err, exitCode) -> do
                unless (quiet e) $ do
                    hPutStrLn stderr "Script failed:"
                    hPutStrLn stderr contents
                    hPutStrLn stderr "Errors:"
                    hPutStrLn stderr err
                exitWith exitCode
            Right out -> return out

makeCmd :: String -> String -> String
makeCmd cmd arg = replaceArg cmd False
    where
        replaceArg :: String -> Bool -> String
        replaceArg (c1:c2:cs) found
            | c1 == kScriptArg && c2 == kScriptArg = kScriptArg : replaceArg cs found
        replaceArg (c:cs) _
            | c == kScriptArg = arg ++ replaceArg cs True
        replaceArg (c:cs) found = c : replaceArg cs found
        replaceArg [] False = ' ' : arg
        replaceArg [] True = []

