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

module ExpandSpec where

import AbpTest

import NeatInterpolation
import Test.Hspec

spec :: Spec
spec = describe "variable expansion" $ do

    it "expands variables from the document metadata" $
        [text|
            % a great title
            % the author

            '{{title}}' has been written by {{author}} (date: {{date}})
        |] ==> "'a great title' has been written by the author (date: {{date}})"

    it "does not expand unknown variables" $ do
        "{{foo}}" ==> "{{foo}}"
        "```meta\nfoo: bar\n```\n\n{{foo}}" ==> "bar"
        "```meta\nfoo: bar\n```\n\n{{fooo}}" ==> "{{fooo}}"
        "```meta\nfoo: bar\n```\n\n{{ foo}}" ==> "{{ foo}}"
        "```meta\nfoo: bar\n```\n\n{{foo }}" ==> "{{foo }}"
        "```meta\nfoo: bar\n```\n\n{{f oo}}" ==> "{{f oo}}"

    it "does not expand raw inlines" $
        [text|
            ```meta
            foo: bar
            ```

            not expanded:

            - `{{foo}}`{.raw}
            - [`{{foo}}`{.raw}]({{foo}}){.raw}
            - [`{{foo}}`{.raw}]{.raw}
        |] ==> [text|
            not expanded:

            - `{{foo}}`{.raw}
            - [`{{foo}}`{.raw}]({{foo}}){.raw}
            - [`{{foo}}`{.raw}]{.raw}
        |]

    it "does not expand raw blocks" $
        [text|
            ```meta
            foo: bar
            ```

            ```{.raw}
            {{foo}}
            ```
        |] ==> [text|
            ```{.raw}
            {{foo}}
            ```
        |]


    it "gets variable definitions from code blocks" $
        [text|
            ```meta
            foo: bar
            ok
            ```

            foo = {{foo}};
            ok = {{ok}};
            nok = {{nok}};
        |] ==> "foo = bar; ok = ; nok = {{nok}};"

    it "gets variable definitions from a YAML file" $
        [text|
            ```{meta=package.yaml}
            ```

            name = {{name}}
        |] ==> "name = abp"

    it "defines variables from other variables" $
        [text|
            ```meta
            bar: buzz
            foo: !{{bar}}!
            ```

            foo = {{foo}}
        |] ==> "foo = !buzz!"

    context "conditional blocks" $ do
        it "takes definitions from the active blocks" $ do
            [text|
                ```meta
                lang: en
                ```

                ```{.meta ifdef=lang value=en}
                hi: Hello
                ```

                ```{.meta ifdef=lang value=fr}
                hi: Bonjour
                ```

                hi = {{hi}}
            |] ==> "hi = Hello"
            [text|
                ```meta
                lang: fr
                ```

                ```{.meta ifdef=lang value=en}
                hi: Hello
                ```

                ```{.meta ifdef=lang value=fr}
                hi: Bonjour
                ```

                hi = {{hi}}
            |] ==> "hi = Bonjour"
        it "takes active blocks" $ do
            [text|
                ```meta
                lang: en
                ```

                ::: {ifdef=lang value=en}
                Hello
                :::

                :::{ifdef=lang value=fr}
                Bonjour
                :::
            |] ==> [text|
                        ::: {ifdef=lang value=en}
                        Hello
                        :::
                   |]
            [text|
                ```meta
                lang: fr
                ```

                ::: {ifdef=lang value=en}
                Hello
                :::

                :::{ifdef=lang value=fr}
                Bonjour
                :::
            |] ==> [text|
                        ::: {ifdef=lang value=fr}
                        Bonjour
                        :::
                   |]
            "```meta\n\
            \debug:\n\
            \```\n\n\
            \::: {ifdef=debug}\n\
            \debug is defined\n\
            \:::\n\n\
            \:::{ifndef=debug}\n\
            \debug is not defined\n\
            \:::\n\n\
            \ " ==> "::: {ifdef=debug}\n\
                   \debug is defined\n\
                   \:::"
            "```meta\n\
            \release:\n\
            \```\n\n\
            \::: {ifdef=debug}\n\
            \debug is defined\n\
            \:::\n\n\
            \:::{ifndef=debug}\n\
            \debug is not defined\n\
            \:::\n\n\
            \ " ==> "::: {ifndef=debug}\n\
                   \debug is not defined\n\
                   \:::"

    context "conditional inlines" $
        it "takes active inlines" $ do
            let debug = [text|
                            ```meta
                            target: debug
                            ```
                        |]
            [text|
                $debug
                target: {{target}} `666`{ifdef=target value=debug} `42`{ifdef=target value=release}
            |] ==> "target: debug `666`{ifdef=target value=debug}"
            [text|
                $debug
                target: {{target}} [link1](url1){ifdef=target value=debug} [link2](url2){ifdef=target value=release}
            |] ==> "target: debug [link1](url1){ifdef=target value=debug}"
            [text|
                $debug
                target: {{target}} ![link1](url1){ifdef=target value=debug} ![link2](url2){ifdef=target value=release}
            |] ==> "target: debug ![link1](url1){ifdef=target value=debug}"
            [text|
                $debug
                target: {{target}} [span 1]{ifdef=target value=debug} [span 2]{ifdef=target value=release}
            |] ==> "target: debug [span 1]{ifdef=target value=debug}"

    it "expands strings in inlines" $ do
        "```meta\nfoo: bar\n```\n\n{{foo}}" ==> "bar"
        "```meta\nfoo: bar\n```\n\n*{{foo}}*" ==> "*bar*"
        "```meta\nfoo: bar\n```\n\n**{{foo}}**" ==> "**bar**"
        "```meta\nfoo: bar\n```\n\n~~{{foo}}~~" ==> "~~bar~~"
        "```meta\nfoo: bar\n```\n\n~{{foo}}~" ==> "~bar~"
        "```meta\nfoo: bar\n```\n\n^{{foo}}^" ==> "^bar^"
        "```meta\nfoo: bar\n```\n\n```{{foo}}```{foo=\"{{foo}}\"}" ==> "```bar```{foo=bar}"
        "```meta\nfoo: bar\n```\n\n${{foo}}$" ==> "$bar$"
        "```meta\nfoo: bar\nurl: example.com\n```\n\n[{{foo}}]({{url}}){foo=\"{{foo}}\"}" ==> "[bar](example.com){foo=bar}"
        "```meta\nfoo: bar\nurl: example.com\n```\n\n![{{foo}}]({{url}}){foo=\"{{foo}}\"}" ==> "![bar](example.com){foo=bar}"
        "```meta\nfoo: bar\n```\n\n[{{foo}}]{foo=\"{{foo}}\"}" ==> "[bar]{foo=bar}"

    it "expands strings in blocks" $ do
        "```meta\nfoo: bar\nx: y\n```\n\n{{foo}}" ==> "bar"
        "```meta\nfoo: bar\nx: y\n```\n\n```{x=\"{{x}}\"}\n{{foo}}\n```" ==> "```{x=\"y\"}\nbar\n```"
        "```meta\nfoo: bar\nx: y\n```\n\n## {{foo}} {x=\"{{x}}\"}" ==> "## bar {#foo x=\"y\"}"
        "```meta\nfoo: bar\nx: y\n```\n\n:::{x=\"{{x}}\"}\n{{foo}}\n:::\n" ==> ":::{x=y}\nbar\n:::\n"
