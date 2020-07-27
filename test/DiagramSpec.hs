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

module DiagramSpec where

import AbpTest

import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Text as T
import NeatInterpolation
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO.Temp
import Test.Hspec

spec :: Spec
spec = describe "diagrams" $ do
    it "converts diagrams to images with target" $ withTmp $ \tmp ->
        [text|
            ```{.dot title="image title" render="dot -o %o %i" img="$tmp/abp_dot_test_1.png" target="url"}
            digraph { A -> B }
            ```
        |] ==> [text|
                [![image title]($tmp/abp_dot_test_1.png "image title"){.dot}](url "image title")
               |]
    it "converts diagrams to images without target" $ withTmp $ \tmp ->
        [text|
            ```{.dot title="image title" render="dot -o %o %i" img="$tmp/abp_dot_test_2.png"}
            ```
        |] ==> [text|
                ![image title]($tmp/abp_dot_test_2.png "image title"){.dot}
               |]
    it "can save images in a different output directory" $ withTmp $ \tmp -> do
        createDirectory $ T.unpack tmp </> "other_directory"
        [text|
            ```{.dot title="image title" render="dot -o %o %i" img="$tmp/abp_dot_test_3.png" out="$tmp/other_directory"}
            ```
        |] ==> [text|
                ![image title]($tmp/abp_dot_test_3.png "image title"){.dot}
               |]
    it "extract the image format from the command line" $ withTmp $ \tmp ->
        [text|
            ```{.dot title="image title" render="dot -o %o.png %i" img="$tmp/abp_dot_test_4"}
            ```
        |] ==> [text|
                ![image title]($tmp/abp_dot_test_4.png "image title"){.dot}
               |]
    it "catches errors" $ withTmp $ \tmp ->
        [text|
            ```{.dot title="image title" render="dot -o %o %i" img="$tmp/abp_dot_test_5.png"}
            This is an error!
            ```
        |] ==>! ExitFailure 1
    it "takes a default PlantUML JAR in the same directory than abp" $ do
        path <- T.pack . (</> "plantuml.jar") . takeDirectory <$> getExecutablePath
        "`{{plantuml}}`" ==> [text|`java -jar $path -pipe -charset UTF-8 -tsvg < %i > %o.svg`|]
    it "takes a custom PlantUML JAR in the PLANTUML environment variable" $ do
        let path = "/custom/path/to/custom_plantuml.jar"
            path' = T.pack path
        setEnv "PLANTUML" path
        "`{{plantuml}}`" ==> [text|`java -jar $path' -pipe -charset UTF-8 -tsvg < %i > %o.svg`|]
        unsetEnv "PLANTUML"
    it "takes a default ditaa JAR in the same directory than abp" $ do
        path <- T.pack . (</> "ditaa.jar") . takeDirectory <$> getExecutablePath
        "`{{ditaa}}`" ==> [text|`java -jar $path --svg -o -e UTF-8 %i %o.svg`|]
    it "takes a custom ditaa JAR in the DITAA environment variable" $ do
        let path = "/custom/path/to/custom_ditaa.jar"
            path' = T.pack path
        setEnv "DITAA" path
        "`{{ditaa}}`" ==> [text|`java -jar $path' --svg -o -e UTF-8 %i %o.svg`|]
        unsetEnv "DITAA"

withTmp :: ( Control.Monad.IO.Class.MonadIO m
           , Control.Monad.Catch.MonadMask m
           ) =>
           (T.Text -> m a) -> m a
withTmp io = withSystemTempDirectory "abp" $ \tmp -> io $ T.pack tmp
