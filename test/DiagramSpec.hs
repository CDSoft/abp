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

module DiagramSpec where

import AbpTest

import NeatInterpolation
import System.Exit
import Test.Hspec

spec :: Spec
spec = describe "diagrams" $ do
    it "converts diagrams to images with target" $
        [text|
            ```{.dot title="image title" render="dot -o %o %i" img="/tmp/abp_dot_test_1.png" target="url"}
            ```
        |] ==> [text|
                [![image title](/tmp/abp_dot_test_1.png "image title"){.dot}](url "image title")
               |]
    it "converts diagrams to images without target" $
        [text|
            ```{.dot title="image title" render="dot -o %o %i" img="/tmp/abp_dot_test_1.png"}
            ```
        |] ==> [text|
                ![image title](/tmp/abp_dot_test_1.png "image title"){.dot}
               |]
    it "catches errors" $
        [text|
            ```{.dot title="image title" render="dot -o %o %i" img="/tmp/abp_dot_test_2.png"}
            This is an error!
            ```
        |] !=> ExitFailure 1
