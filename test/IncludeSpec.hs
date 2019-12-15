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

module IncludeSpec where

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
    it "includes a file in a code block" $ withTmp $ \tmp -> do
        writeFile (T.unpack tmp</>"foo.hs") $ T.unpack [text|
                                                            main :: IO ()
                                                            main = print "Hello"
                                                       |]
        [text|
            ```{.haskell include=$tmp/foo.hs}
            not used
            ```
        |] ==> [text|
                ``` haskell
                main :: IO ()
                main = print "Hello"
                ```
               |]
    it "includes a file in a code block from a give line" $ withTmp $ \tmp -> do
        writeFile (T.unpack tmp</>"foo.hs") $ T.unpack [text|
                                                            not included
                                                            not included
                                                            main :: IO ()
                                                            main = print "Hello"
                                                       |]
        [text|
            ```{.haskell include=$tmp/foo.hs fromline=3}
            not used
            ```
        |] ==> [text|
                ``` haskell
                main :: IO ()
                main = print "Hello"
                ```
               |]

    it "includes a file in a code block upto a give line" $ withTmp $ \tmp -> do
        writeFile (T.unpack tmp</>"foo.hs") $ T.unpack [text|
                                                            main :: IO ()
                                                            main = print "Hello"
                                                            not included
                                                            not included
                                                       |]
        [text|
            ```{.haskell include=$tmp/foo.hs toline=2}
            not used
            ```
        |] ==> [text|
                ``` haskell
                main :: IO ()
                main = print "Hello"
                ```
               |]

    it "includes a file in a code block between two lines" $ withTmp $ \tmp -> do
        writeFile (T.unpack tmp</>"foo.hs") $ T.unpack [text|
                                                            not included
                                                            not included
                                                            main :: IO ()
                                                            main = print "Hello"
                                                            not included
                                                            not included
                                                       |]
        [text|
            ```{.haskell include=$tmp/foo.hs fromline=3 toline=4}
            not used
            ```
        |] ==> [text|
                ``` haskell
                main :: IO ()
                main = print "Hello"
                ```
               |]

    it "includes a Markdown file in a div block" $ withTmp $ \tmp -> do
        writeFile (T.unpack tmp</>"foo.md") $ T.unpack [text|
                                                            # foo title
                                                            ## foo subtitle

                                                            hi
                                                       |]
        [text|
            # title 1
            :::{include=$tmp/foo.md}
            not used
            :::
            # title 2
        |] ==> [text|
                # title 1

                # foo title

                ## foo subtitle

                hi

                # title 2
               |]

    it "includes a reStructuredText file in a div block" $ withTmp $ \tmp -> do
        writeFile (T.unpack tmp</>"foo.rst") $ T.unpack [text|
                                                            foo title
                                                            =========
                                                            foo subtitle
                                                            ------------

                                                            hi
                                                        |]
        [text|
            # title 1
            :::{include=$tmp/foo.rst}
            not used
            :::
            # title 2
        |] ==> [text|
                # title 1

                # foo title

                ## foo subtitle

                hi

                # title 2
               |]

    it "includes a LaTeX file in a div block" $ withTmp $ \tmp -> do
        writeFile (T.unpack tmp</>"foo.latex") $ T.unpack [text|
                                                            \section{foo title}
                                                            \subsection{foo subtitle}
                                                            hi
                                                        |]
        [text|
            # title 1
            :::{include=$tmp/foo.latex}
            not used
            :::
            # title 2
        |] ==> [text|
                # title 1

                # foo title

                ## foo subtitle

                hi

                # title 2
               |]

    it "includes a HTML file in a div block" $ withTmp $ \tmp -> do
        writeFile (T.unpack tmp</>"foo.html") $ T.unpack [text|
                                                            <h1> foo title </h1>
                                                            <h2> foo subtitle </h2>
                                                            hi
                                                        |]
        [text|
            # title 1
            :::{include=$tmp/foo.html}
            not used
            :::
            # title 2
        |] ==> [text|
                # title 1

                # foo title

                ## foo subtitle

                hi

                # title 2
               |]

    it "includes a Markdown file in a div block and shift title levels" $ withTmp $ \tmp -> do
        writeFile (T.unpack tmp</>"foo.md") $ T.unpack [text|
                                                            # foo title
                                                            ## foo subtitle

                                                            hi
                                                       |]
        [text|
            # title 1
            :::{include=$tmp/foo.md shift=1}
            not used
            :::
            # title 2
        |] ==> [text|
                # title 1

                ## foo title

                ### foo subtitle

                hi

                # title 2
               |]

    it "includes a reStructuredText file in a div block and shift title levels" $ withTmp $ \tmp -> do
        writeFile (T.unpack tmp</>"foo.rst") $ T.unpack [text|
                                                            foo title
                                                            =========
                                                            foo subtitle
                                                            ------------

                                                            hi
                                                        |]
        [text|
            # title 1
            :::{include=$tmp/foo.rst shift=1}
            not used
            :::
            # title 2
        |] ==> [text|
                # title 1

                ## foo title

                ### foo subtitle

                hi

                # title 2
               |]

    it "includes a LaTeX file in a div block and shift title levels" $ withTmp $ \tmp -> do
        writeFile (T.unpack tmp</>"foo.latex") $ T.unpack [text|
                                                            \section{foo title}
                                                            \subsection{foo subtitle}
                                                            hi
                                                        |]
        [text|
            # title 1
            :::{include=$tmp/foo.latex shift=1}
            not used
            :::
            # title 2
        |] ==> [text|
                # title 1

                ## foo title

                ### foo subtitle

                hi

                # title 2
               |]

    it "includes a HTML file in a div block and shift title levels" $ withTmp $ \tmp -> do
        writeFile (T.unpack tmp</>"foo.html") $ T.unpack [text|
                                                            <h1> foo title </h1>
                                                            <h2> foo subtitle </h2>
                                                            hi
                                                        |]
        [text|
            # title 1
            :::{include=$tmp/foo.html shift=1}
            not used
            :::
            # title 2
        |] ==> [text|
                # title 1

                ## foo title

                ### foo subtitle

                hi

                # title 2
               |]

withTmp :: ( Control.Monad.IO.Class.MonadIO m
           , Control.Monad.Catch.MonadMask m
           ) =>
           (T.Text -> m a) -> m a
withTmp io = withSystemTempDirectory "abp" $ \tmp -> io $ T.pack tmp
