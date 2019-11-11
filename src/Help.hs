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

{-# LANGUAGE TemplateHaskell #-}

module Help
    ( name
    , version
    , copyright
    , help
    )
where

import Config
import Package
import Tools

name :: String
name = $(abpName)

version :: String
version = $(abpName) ++ " " ++ $(abpVersion)

license :: String
license = $(abpLicense)

description :: String
description = $(abpDescription)

upstream :: String
upstream = "https://cdsoft.fr/" ++ name

copyright :: String
copyright = unlines [
        version,
        description,
        "",
        "Copyright (C) " ++ $(abpCopyright),
        "This is free software; see the source for copying conditions.  There is NO",
        "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.",
        "Released under the " ++ license ++ " license.",
        "",
        "See " ++ upstream ++ " for further information."
    ]

help :: String
help = unlines [
        "abp is a pandoc filter",
        "Usage: pandoc --filter=abp",
        "abp can be configured with environment variables:",
        "  "++ljust 20 kABPQuiet++" disable stderr on script execution",
        "  ABP_PLANTUML         plantuml.jar full path",
        "",
        "abp can be called on the command line to get additional information:",
        "Usage: " ++ name ++ " option",
        "Options:",
        "  -v                   Display the current version",
        "  -h                   Display this help message",
        "",
        "More information here: " ++ upstream
    ]
