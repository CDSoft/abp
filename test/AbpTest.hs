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

module AbpTest
where

import AbstractProcessor
import Config

import qualified Data.Text as T
import System.Environment
import System.Exit
import Test.Hspec
import Text.Pandoc

readerOptions :: ReaderOptions
readerOptions = def { readerExtensions = pandocExtensions
                    }

writerOptions :: WriterOptions
writerOptions = def { writerExtensions = pandocExtensions
                    }

reader :: T.Text -> IO Pandoc
reader = runIOorExplode . readMarkdown readerOptions

writer :: Pandoc -> IO T.Text
writer = runIOorExplode . writeMarkdown writerOptions

-- run tests with the default output format
(==>) :: HasCallStack => T.Text -> T.Text -> IO ()
(==>) = runTest Nothing

-- run tests with the HTML output
(*==>) :: HasCallStack => T.Text -> T.Text -> IO ()
(*==>) = runTest (Just (Format "html"))

runTest :: HasCallStack => Maybe Format -> T.Text -> T.Text -> IO ()
runTest format input expectedOutput = do
    output <- reader input >>= abp format >>= writer
    outputAst <- reader output
    expectedOutputAst <- reader expectedOutput
    outputAst `shouldBe` expectedOutputAst

-- run tests and expect an exception
(==>!) :: HasCallStack => T.Text -> ExitCode -> IO ()
(==>!) input exitCode = do
    inputAst <- reader input
    setEnv (T.unpack kAbpQuiet) "1"
    abp Nothing inputAst `shouldThrow` (==exitCode)
