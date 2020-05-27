% Abstract Processor (for Pandoc)
% Christophe Delord - <http://cdelord.fr/abp>
% 15 Mai 2020

```{meta="{{ABP_ROOT}}package.yaml"}
archive: {{name}}-{{version}}.tar.gz
```

[ABP]: http://cdelord.fr/abp "ABP - Abstract Preprocessor (for Pandoc)"
[{{archive}}]: http://cdelord.fr/abp/{{archive}}
[PP]: http://cdelord.fr/pp "PP - Generic Preprocessor (for Pandoc)"
[GraphViz]: http://graphviz.org/
[PlantUML]: http://plantuml.sourceforge.net/
[ditaa]: http://ditaa.sourceforge.net/
[blockdiag]: http://blockdiag.com/
[Asymptote]: http://asymptote.sourceforge.net/
[mermaid]: https://mermaidjs.github.io/
[R]: https://www.r-project.org/
[Pandoc]: http://pandoc.org/
[Pandoc filter]: http://pandoc.org/filters.html
[Python]: https://www.python.org/
[Lua]: http://www.lua.org/
[Haskell]: https://www.haskell.org/
[Stack]: https://docs.haskellstack.org/en/stable/README/
[GitHub]: https://github.com/CDSoft/abp
[Pandoc csv2table filter]: http://hackage.haskell.org/package/pandoc-csv2table
[cdelord.fr]: http://cdelord.fr

ABP - Abstract preprocessor (for Pandoc)
========================================

ABP is a [Pandoc filter] that works on internal Pandoc's AST.

It provides several interesting features:

- variable expansion (minimalistic templating)
- conditional blocks
- file inclusion (e.g. for source code examples)
- script execution (e.g. to include the result of a command)
- diagrams ([Graphviz], [PlantUML], [ditaa], [Asymptote], [blockdiag], [mermaid]...)
- CSV tables

Open source
===========

[ABP] is an Open source software.
Anybody can contribute on [GitHub] to:

- suggest or add new features
- report or fix bugs
- improve the documentation
- add some nicer examples
- find new usages
- ...

Installation
============

1. Download and extract [{{archive}}].
2. Run `stack init && stack install`.
3. Run `stack install pandoc` if you need to install [Pandoc].

This will install `abp` and `pandoc` in `~/.local/bin`.

[ABP] is written in [Haskell] and is built with [Stack].

Usage
=====

`abp` is a [Pandoc filter] and is not meant to be called directly.

``` sh
$ pandoc -F abp ...
```

A complete example is given as a Makefile in the doc directory.

Cheat sheet
===========

Syntactic item              Class           Attributes              Description
--------------------------- --------------- ----------------------- ---------------------------------
any string                                                          `{{var}}` is replaced by the value of `var` if it is defined (variables can be environment variables or YAML definitions)
div block                   `comment`                               commented block
div block                                   `include=file`          replaces the div block with the content of `file` (rendered according to its format)
div block                                   `shift=n`               adds `n` to header levels in an imported div block
any block                   `raw`                                   strings are not expanded in this block
code block                  `meta`                                  definitions for the string expansion (YAML subset), defined in the code block
code block                                  `meta=file`             definitions for the string expansion (YAML subset), defined in `file`
any block                                   `ifdef=name`            block emitted only if `name` is defined
any block                                   `ifdef=name value=val`  block emitted only if `name` is defined and its value is `value`
any block                                   `ifndef=name`           block emitted only if `name` is not defined
code block, inline code                     `include=file`          replaces the code block content with the content of `file`
code block, inline code                     `fromline=n`            includes a file from line number `n`
code block, inline code                     `toline=n`              includes a file up to line number `n`
code block, inline code                     `cmd="shell command"`   replaces the code block by the result of the shell command
code block                                  `render="command"`      replaces the code block by a link to the image produced by the command (`%i` is the input file name, its content is the content of the code block, `%o` is the output file name)
code block                                  `img="image path"`      URL of the image produced by `render`
code block                                  `out="image path"`      path of the image produced by `render` (optional, the default value is `img`)
CSV tables                  `table`                                 see [Pandoc csv2table filter]

Commented blocks
================

Div blocks with the `comment` class are commented:

``` markdown
::: comment
This block is a comment and is discarded by abp.
:::
```

String expansion
================

`abp` stores variables in an environment used to expand strings.
Variables can be defined by a YAML file (currently a very limited subset of YAML) with the `meta` class.
The `meta` attribute can also be used to point to an external file.
Variables can only contain inline elements, not blocks.

The initial environment contains:

- the environment variables
- the document metadata (title, author, date)
- the output document format (first argument of `abp` given by `pandoc`)

Variable names are enclosed between double curly brackets.
E.g. `{{title}}`{.raw} will be replaced with the document title.

Elements with the `raw` class won't be expanded.
E.g. `` `{{title}}`{.raw} ``{.raw} won't be replaced with the document title.

E.g.:

~~~markdown
```meta
foo: bar (note: this is parsed as **Markdown**)
```

foo is {{foo}}.
~~~

~~~markdown
```{meta=foo.yaml}
This text is ignored, definitions are in foo.yaml.
```

foo is defined in `foo.yaml` and is {{foo}}.
~~~

Conditional blocks
==================

Blocks can be conditionally kept or omitted. The condition is described with attributes.

```markdown
:::{ifdef="name" value="value"}
This block is emitted only if the variable "name" is defined
and its value is "value"
:::
```

```markdown
:::{ifdef="name"}
This block is emitted only if the variable "name" is defined
(whatever its value)
:::
```

```markdown
:::{ifndef="name"}
This block is emitted only if the variable "name" is **not** defined
:::
```

Div inclusion
=============

Fragments of documents can be imported from external files.
The `include` attribute contains the name of the file to include.
The content of the file is parsed according to its format (deduced from its name)
and replaces the div block content.

~~~markdown
:::{include=file.md}
This text is optional and will be replaced by the content of file.md.
:::
~~~

The included file can be in a different format
(e.g. a markdown file can include a reStructuredText file).

Block inclusion
===============

Code examples can be imported from external files.
The `include` attribute contains the name of the file to include.
The content of the file replaces the code block content.

~~~markdown
```{.c include=foo.c fromline=3 toline=10}
This text is optional and will be replaced by the content of foo.c.
```
~~~

The optional `fromline` and `toline` defines the first and last lines to be included.

Scripts
=======

Scripts can be executed by inline or code blocks.
The `cmd` attribute defines the command to execute.
The content of the block is in a temporary file which name is added to the command.
If the command contains the `%` char, it is replaced by the temporary file name.
If the command does not contain any `%`, the file name is appended to the command.
The result replaces the content of the code block.

+-----------------------------------------------------------------------------------------------+---------------------------------------------------------------------------------------------------+
| Source                                                                                        | Result                                                                                            |
+===============================================================================================+===================================================================================================+
| ~~~ {.markdown .raw}                                                                          |                                                                                                   |
| ```{.python cmd=python}                                                                       | ```{.python cmd=python}                                                                           |
| print("Hello from Python!")                                                                   | print("Hello from Python!")                                                                       |
| ```                                                                                           | ```                                                                                               |
| ~~~                                                                                           |                                                                                                   |
+-----------------------------------------------------------------------------------------------+---------------------------------------------------------------------------------------------------+
| ~~~ {.markdown .raw}                                                                          |                                                                                                   |
| Python says `print("Hello from Python!")`{cmd=python}                                         | Python says `print("Hello from Python!")`{cmd=python}                                             |
| ~~~                                                                                           |                                                                                                   |
+-----------------------------------------------------------------------------------------------+---------------------------------------------------------------------------------------------------+

Diagrams
========

Code blocks containing diagrams are replaced with an image resulting from the diagram source code.

The render command is the `render` field.
The output image can be a hash computed from the diagram source code or the value of the `img` field.
The optional `out` field overloads `img` to change the output directory when rendering the diagram.

In the `render` command, `%i` is replaced by the name of the input document (generated from the content of the code block)
and `%o` by the name of the output image file (generated from the `img` field).

The file format (extension) must be in the `render` field, after the `%o` tag (e.g.: `%o.png`), not in the `img` field.

+-----------------------------------------------------------------------------------------------+---------------------------------------------------------------------------------------------------+
| Source                                                                                        | Result                                                                                            |
+===============================================================================================+===================================================================================================+
| ~~~ {.markdown .raw}                                                                          |                                                                                                   |
| ```{render="{{plantuml}}" img="img/abp_plantuml_test" out="{{doc}}/img"}                      | ```{render="java -jar ~/.local/bin/plantuml.jar -pipe -tsvg -charset UTF-8 < %i > %o.svg" img="img/abp_plantuml_test" out="{{doc}}/img" height=96}    |
| @startuml                                                                                     | @startuml                                                                                         |
| Alice -> Bob: hello                                                                           | Alice -> Bob: test                                                                                |
| @enduml                                                                                       | @enduml                                                                                           |
| ```                                                                                           | ```                                                                                               |
| ~~~                                                                                           |                                                                                                   |
+-----------------------------------------------------------------------------------------------+---------------------------------------------------------------------------------------------------+

Some render commands are predefined:

Diagram             Predefined variable                     Render command
------------------- --------------------------------------- ----------------------------------------------------------
[GraphViz]          `{{dot}}`{.raw}                         `{{dot}}`
                    `{{dot.svg}}`{.raw}                     `{{dot.svg}}`
                    `{{dot.png}}`{.raw}                     `{{dot.png}}`
                    `{{dot.pdf}}`{.raw}                     `{{dot.pdf}}`
[PlantUML]          `{{plantuml}}`{.raw}                    `{{plantuml}}`
                    `{{plantuml.svg}}`{.raw}                `{{plantuml.svg}}`
                    `{{plantuml.png}}`{.raw}                `{{plantuml.png}}`
                    `{{plantuml.pdf}}`{.raw}                `{{plantuml.pdf}}`
[Asymptote]         `{{asy}}`{.raw}                         `{{asy}}`
                    `{{asy.svg}}`{.raw}                     `{{asy.svg}}`
                    `{{asy.png}}`{.raw}                     `{{asy.png}}`
                    `{{asy.pdf}}`{.raw}                     `{{asy.pdf}}`
[blockdiag]         `{{blockdiag}}`{.raw}                   `{{blockdiag}}`
                    `{{blockdiag.svg}}`{.raw}               `{{blockdiag.svg}}`
                    `{{blockdiag.png}}`{.raw}               `{{blockdiag.png}}`
                    `{{blockdiag.pdf}}`{.raw}               `{{blockdiag.pdf}}`
[mermaid]           `{{mmdc}}`{.raw}                        `{{mmdc}}`
                    `{{mmdc.svg}}`{.raw}                    `{{mmdc.svg}}`
                    `{{mmdc.png}}`{.raw}                    `{{mmdc.png}}`
                    `{{mmdc.pdf}}`{.raw}                    `{{mmdc.pdf}}`
[ditaa]             `{{ditaa}}`{.raw}                       `{{ditaa}}`
                    `{{ditaa.svg}}`{.raw}                   `{{ditaa.svg}}`
                    `{{ditaa.png}}`{.raw}                   `{{ditaa.png}}`

Notes:

- `dot`: [GraphViz] support also includes `dot`, `neato`, `twopi`, `circo`, `fdp`, `sfdp`, `patchwork` and `osage`.

- `plantuml`: `{{PLANTUML}}`{.raw} can be defined as an environment variable.
    Its default value is the directory of the `abp` executable appended with `"plantuml.jar"`.

- `ditaa`: `{{DITAA}}`{.raw} can be defined as an environment variable.
    Its default value is the directory of the `abp` executable appended with `"ditaa.jar"`.

- `blockdiag`: [Blockdiag] support also includes `actdiag`, `blockdiag`, `nwdiag`, `packetdiag`, `rackdiag` and `seqdiag`.

- renderers without an explicit image format are built differently according to the output document format.

    - For PDF (LaTeX) documents, the default format is PNG
    - For other documents, the default format is SVG
    - The file extension is added to the `img` field

E.g.:

+-----------------------------------------------------------------------------------------------+---------------------------------------------------------------------------------------------------+
| Source                                                                                        | Result                                                                                            |
+===============================================================================================+===================================================================================================+
| ~~~ {.markdown .raw}                                                                          |                                                                                                   |
| ```{.dot render="{{dot}}" img="img/abp_diagram_example" out="{{doc}}/img" height=128}         | ```{.dot render="{{dot}}" img="img/abp_diagram_example" out="{{doc}}/img" height=128 }            |
| digraph {                                                                                     | digraph {                                                                                         |
|     rankdir=LR;                                                                               |     rankdir=LR;                                                                                   |
|     input -> pandoc -> output                                                                 |     input -> pandoc -> output                                                                     |
|     pandoc -> abp -> {pandoc, diagrams}                                                       |     pandoc -> abp -> {pandoc, diagrams}                                                           |
|     { rank=same; pandoc, abp }                                                                |     { rank=same; pandoc, abp }                                                                    |
|     { rank=same; diagrams, output }                                                           |     { rank=same; diagrams, output }                                                               |
| }                                                                                             | }                                                                                                 |
| ```                                                                                           | ```                                                                                               |
| ~~~                                                                                           |                                                                                                   |
+-----------------------------------------------------------------------------------------------+---------------------------------------------------------------------------------------------------+

Filters can be combined. E.g.: a diagram can be stored in an external file, included and rendered by `abp`.

+-----------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------+
| Source                                                                                                    | Result                                                                                                    |
+===========================================================================================================+===========================================================================================================+
| ~~~ {.markdown .raw}                                                                                      |                                                                                                           |
| The file `hello.dot` contains:                                                                            | The file `hello.dot` contains:                                                                            |
|                                                                                                           |                                                                                                           |
| ```{.dot include="{{doc}}/hello.dot" fromline=21 toline=24}                                               | ```{.dot include="{{doc}}/hello.dot" fromline=21 toline=24}                                               |
| ```                                                                                                       | ```                                                                                                       |
| ~~~                                                                                                       |                                                                                                           |
+-----------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------+
| ~~~ {.markdown .raw}                                                                                      |                                                                                                           |
| and is rendered as:                                                                                       | and is rendered as:                                                                                       |
|                                                                                                           |                                                                                                           |
| ```{render="{{dot}}" img="img/hello" out="{{doc}}/img" height=48 include="{{doc}}/hello.dot"}             | ```{render="{{dot}}" img="img/hello" out="{{doc}}/img" height=48 include="{{doc}}/hello.dot"}             |
| ```                                                                                                       | ```                                                                                                       |
| ~~~                                                                                                       |                                                                                                           |
+-----------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------+

CSV tables
==========

CSV tables are rendered using the [Pandoc csv2table filter].
The filter has been included to ABP without any modification.

The table content can be defined in the code block or in an external file.

+-----------------------------------------------------------------------------------------------+---------------------------------------------------------------------------------------------------+
| Source                                                                                        | Result                                                                                            |
+===============================================================================================+===================================================================================================+
| ~~~ {.markdown .raw}                                                                          |                                                                                                   |
| ```{.table aligns="LCR" caption="This is the **caption**" header="yes"}                       | ```{.table aligns="LCR" caption="This is the **caption**" header="yes"}                           |
| Fruit, Quantity, Price                                                                        | Fruit, Quantity, Price                                                                            |
| apples, 15, 3.24                                                                              | apples, 15, 3.24                                                                                  |
| oranges, 12, 2.22                                                                             | oranges, 12, 2.22                                                                                 |
| ```                                                                                           | ```                                                                                               |
| ~~~                                                                                           |                                                                                                   |
+-----------------------------------------------------------------------------------------------+---------------------------------------------------------------------------------------------------+

Makefile dependencies
=====================

It is sometimes useful to build a dependency list on the fly.
`abp` can generate a dependency list for make, in the same vein than the gcc `-M` option.
The environment variable `ABP_TARGET` must be defined with the target name.
`abp` will generate a file named `${ABP_TARGET}.d`{.sh} containing the dependencies of `${ABP_TARGET}`{.sh}.

E.g.:

``` sh
ABP_TARGET=index.html pandoc -F ABP index.md -o index.html
```

This will produce a file named `index.html.d` containing `index.html: ...`.

Licenses
========

## ABP

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

## PP

Some portions of code are based on [PP] released under the [GPL license version 3](http://cdelord.fr/pp/#pp).

## Pandoc csv2table filter

[Pandoc csv2table filter] is written by Wasif Hasan Baig
and has been released under the [MIT license](http://hackage.haskell.org/package/pandoc-csv2table-1.0.7/src/LICENSE).

Feedback
========

Your feedback and contributions are welcome.
You can contact me at [cdelord.fr].
