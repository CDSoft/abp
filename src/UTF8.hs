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

module UTF8 ( setUTF8Encoding
            , readFileUTF8
            , writeFileUTF8
            , hWriteFileUTF8
            , readProcessUTF8
            )
where

import qualified Data.Text as T
import System.Exit
import System.IO
import qualified System.IO.Strict as SIO
import System.Process

-- setUTF8Encoding sets UTF8 as the current encoding of a file handle
setUTF8Encoding :: Handle -> IO ()
setUTF8Encoding h = hSetEncoding h utf8

-- "readFileUTF8 name" reads an UTF-8 file.
readFileUTF8 :: FilePath -> IO T.Text
readFileUTF8 name = do
    h <- openFile name ReadMode
    hSetEncoding h utf8
    -- the file must not be read lazily
    -- (in some case we want to be able to read files
    -- that have been previously produced by the same document)
    content <- T.pack <$> SIO.hGetContents h
    hClose h
    return content

-- "writeFileUTF8 name content" writes an UTF-8 file.
writeFileUTF8 :: FilePath -> String -> IO ()
writeFileUTF8 name content = do
    handle <- openBinaryFile name WriteMode
    hWriteFileUTF8 handle content
    hClose handle

-- "hwriteFileUTF8 handle content" writes an UTF-8 file.
hWriteFileUTF8 :: Handle -> String -> IO ()
hWriteFileUTF8 handle content = do
    hSetEncoding handle utf8
    hPutStr handle content

-- "readProcessUTF8 cmd arg" executes "cmd args"
-- and returns the standard output produced by the command.
readProcessUTF8 :: String -> [String] -> IO (Either (String, ExitCode) T.Text)
readProcessUTF8 cmd args = do
    (_, Just hOut, Just hErr, hProc) <- createProcess (shell (cmd ++ " " ++ unwords args)) { std_out = CreatePipe, std_err = CreatePipe }
    hSetEncoding hOut utf8
    out <- T.pack <$> SIO.hGetContents hOut
    err <- SIO.hGetContents hErr
    exitCode <- waitForProcess hProc
    return $ case exitCode of
        ret@(ExitFailure _) -> Left (err, ret)
        ExitSuccess -> Right out
