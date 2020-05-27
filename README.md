# Abstract Processor (for Pandoc)

ABP is a [Pandoc filter](https://pandoc.org/filters.html) that works on internal Pandoc's AST.

It provides several interesting features:

- variable expansion (minimalistic templating)
- conditional blocks
- file inclusion (e.g. for source code examples)
- script execution (e.g. to include the result of a command)
- diagrams (Graphviz, PlantUML, Asymptote, blockdiag, mermaid...)
- CSV tables

# Installation

## Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
- [Pandoc](https://pandoc.org/installing.html)

## Installation from source

``` sh
$ git clone https://github.com/CDSoft/abp.git
$ cd abp
$ stack install         # install abp in ~/.local/bin
$ stack install pandoc  # if you need to install pandoc
```

## Test

``` sh
$ stack test
```

## Usage

``` sh
$ pandoc -F abp ...
```

# Documentation

The full documentation is in [doc/adp.md](doc/abp.md).
The rendered version of the documentation is here: <http://cdelord.fr/abp>.

# License

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
