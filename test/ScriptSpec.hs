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

module ScriptSpec where

import AbpTest

import NeatInterpolation
import System.Exit
import Test.Hspec

spec :: Spec
spec = describe "script execution" $ do
    it "executes inline code" $
        "`echo $((1+1))`{.foo cmd=sh}" ==> "`2`{.foo}"
    it "executes code blocks" $
        [text|
            ```{.bar cmd=sh}
            echo $((21*2))
            ```
        |] ==> [text|
                    ``` bar
                    42
                    ```
               |]
    it "catches errors" $
        [text|
            ```{cmd=sh}
            echo Unexpected error >&2
            exit 42
            ```
        |] !=> ExitFailure 42
    it "accepts a flexible command line" $
        [text|
            ```{.haskell cmd="pandoc % -t native"}
            **cool**
            ```
        |] ==> [text|
                    ```{.haskell}
                    [Para [Strong [Str "cool"]]]
                    ```
               |]
    it "allows '%' on the command line" $
        "`echo $1`{cmd='sh % %%'}" ==> "`%`"
