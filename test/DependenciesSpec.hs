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

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module DependenciesSpec where

import AbpTest
import Config

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Text as T
import NeatInterpolation
import System.Environment
import System.FilePath.Posix
import System.IO.Temp
import Test.Hspec

spec :: Spec
spec = describe "dependencies" $
    it "creates a make dependency file" $ withTmp $ \tmp -> do
        writeFile (T.unpack tmp</>"a.md") $ T.unpack [text|
                                                        Hello

                                                        :::{include=$tmp/b.rst}
                                                        :::
                                                     |]
        writeFile (T.unpack tmp</>"b.rst") $ T.unpack [text|
                                                        World!
                                                      |]
        bracket (setEnv (T.unpack kAbpTarget) (T.unpack tmp</>"index.html"))
                (\_ -> unsetEnv (T.unpack kAbpTarget))
                (\_ -> [text|
                            :::{include=$tmp/a.md}
                            :::
                       |] ==> "Hello\n\nWorld!"
                )
        deps <- readFile (T.unpack tmp</>"index.html.d")
        deps `shouldBe` unwords [ T.unpack tmp</>"index.html:"
                                , T.unpack tmp</>"a.md"
                                , T.unpack tmp</>"b.rst"
                                ] ++ "\n"

withTmp :: ( Control.Monad.IO.Class.MonadIO m
           , Control.Monad.Catch.MonadMask m
           ) =>
           (T.Text -> m a) -> m a
withTmp io = withSystemTempDirectory "abp" $ \tmp -> io $ T.pack tmp
