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

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module ScriptSpec where

import AbpTest

import NeatInterpolation
import System.Exit
import Test.Hspec

spec :: Spec
spec = describe "script execution" $ do
    it "executes inline code" $
        "`echo -e $((1+1))`{.sh cmd=sh}" ==> "`2`"
    it "executes inline code and updates the inline class" $
        "`echo -e $((1+1))`{classes=new_class cmd=sh}" ==> "`2`{.new_class}"
    it "executes code blocks" $
        [text|
            ```{.sh cmd=sh}
            echo $((21*2))
            ```
        |] ==> [text|
                    ```
                    42
                    ```
               |]
    it "executes code blocks and updates the block class" $
        [text|
            ```{.sh classes=new_class cmd=sh}
            echo $((21*2))
            ```
        |] ==> [text|
                    ```{.new_class}
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
            ```{.markdown classes=haskell cmd="pandoc % -t native"}
            **cool**
            ```
        |] ==> [text|
                    ```{.haskell}
                    [Para [Strong [Str "cool"]]]
                    ```
               |]
    it "allows '%' on the command line" $
        "`echo $1`{cmd='sh % %%'}" ==> "`%`"
