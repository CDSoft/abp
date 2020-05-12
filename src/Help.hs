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
{-# LANGUAGE OverloadedStrings #-}

module Help
    ( name
    , version
    , copyright
    , help
    , emojis
    , emojisMarkdown
    )
where

import Config
import Package
import Tools

import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Pandoc
import qualified Text.Pandoc.Emoji as E

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
        "",
        "Usage: pandoc --filter=abp",
        "",
        "abp can be configured with environment variables:",
        "  "++ljust 12 (T.unpack kAbpQuiet)++" disable stderr on script execution",
        "  "++ljust 12 (T.unpack kAbpTarget)++" target name for dependency file generation",
        "  "++ljust 12 (T.unpack kAbpPlantuml)++" plantuml.jar full path",
        "  "++ljust 12 (T.unpack kAbpDitaa)++" ditaa.jar full path",
        "",
        "abp can be called on the command line to get additional information:",
        "Usage: " ++ name ++ " option",
        "Options:",
        "  -v           Display the current version",
        "  -h           Display this help message",
        "  emojis       Show available Pandoc emojis",
        "",
        "More information here: " ++ upstream
    ]

emojis :: Block
emojis = table
    where
        table = Table [] [AlignLeft, AlignLeft] [0.0, 0.0] header body
        header = [[Plain [Str "Code"]], [Plain [Str "Emoji"]]]
        body = [ [[Plain [Code ("",[],[]) (T.concat [":", code, ":"])]], [Plain [Str value]]]
               | (code, value) <- emojiList
               ]
        emojiList = sort $ M.toList E.emojis

emojisMarkdown :: IO T.Text
emojisMarkdown = do
    let writerOptions = def { writerExtensions = pandocExtensions
                            }
    let writer = runIOorExplode . writeMarkdown writerOptions
    writer (Pandoc nullMeta [emojis])
