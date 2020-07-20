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

module EnvironmentSpec where

import AbpTest

import qualified Data.Text as T
import NeatInterpolation
import System.Environment
import Test.Hspec

spec :: Spec
spec = describe "environment" $ do
    it "reads environment variables" $ do
        home <- T.pack <$> getEnv "HOME"
        [text|`{{HOME}}`|] ==> [text|`$home`|]
        [text|`{{unknown_var}}`|] ==> [text|`{{unknown_var}}`|]
    it "reads the abp binary path" $ do
        path <- T.pack <$> getExecutablePath
        [text|`{{ABP_PATH}}`|] ==>[text|`$path`|]
    it "defines the output format" $
        [text|{{format}}|] ==>* "html"
