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

module SDocSpec where

import AbpTest

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Text as T
import NeatInterpolation
import System.FilePath.Posix
import System.IO.Temp
import Test.Hspec

spec :: Spec
spec = describe "include" $ do
    it "includes a C program and extract documentation" $ withTmp $ \tmp -> do
        writeFile (T.unpack tmp</>"foo.h") $
            T.unpack [text|
                /*:
                # Title
                :*/

                /*:
                ## Subtitle 1

                Markdown documentation from `foo.h`
                :*/

                int foo(int x);

                /*:
                ## Subtitle 2
                :*/

                const int answer = 42;
            |]
        [text|
            :::{sdoc=$tmp/foo.h shift=1}
            :::
        |] ==> [text|
                ## Title

                ### Subtitle 1

                Markdown documentation from `foo.h`

                ```{.c .numberLines startFrom=11}
                int foo(int x);
                ```

                ### Subtitle 2

                ```{.c .numberLines startFrom=17}
                const int answer = 42;
                ```
               |]

withTmp :: ( Control.Monad.IO.Class.MonadIO m
           , Control.Monad.Catch.MonadMask m
           ) =>
           (T.Text -> m a) -> m a
withTmp io = withSystemTempDirectory "abp" $ \tmp -> io $ T.pack tmp
