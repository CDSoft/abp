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

module AbpTest
where

import AbstractProcessor
import Config

import Data.Text
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

reader :: Text -> IO Pandoc
reader = runIOorExplode . readMarkdown readerOptions

writer :: Pandoc -> IO Text
writer = runIOorExplode . writeMarkdown writerOptions

(==>) :: (HasCallStack) => Text -> Text -> IO ()
(==>) input expectedOutput = do
    output <- reader input >>= abp Nothing >>= writer
    outputAst <- reader output
    expectedOutputAst <- reader expectedOutput
    outputAst `shouldBe` expectedOutputAst

(==>*) :: (HasCallStack) => Text -> Text -> IO ()
(==>*) input expectedOutput = do
    output <- reader input >>= abp (Just (Format "html")) >>= writer
    outputAst <- reader output
    expectedOutputAst <- reader expectedOutput
    outputAst `shouldBe` expectedOutputAst

(!=>) :: (HasCallStack) => Text -> ExitCode -> IO ()
(!=>) input exitCode = do
    inputAst <- reader input
    setEnv kAbpQuiet "1"
    abp Nothing inputAst `shouldThrow` (==exitCode)
