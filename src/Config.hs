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

{-# LANGUAGE OverloadedStrings #-}

module Config
where

import Data.Bifunctor
import Data.List.Extra
import qualified Data.Text as T
import Text.Pandoc.JSON

kAbpQuiet :: T.Text
kAbpQuiet = "ABP_QUIET"

kAbpPath :: T.Text
kAbpPath = "ABP_PATH"

kAbpTarget :: T.Text
kAbpTarget = "ABP_TARGET"

kAbpPlantuml, kPlantumlJar :: T.Text
kAbpPlantuml = "PLANTUML"
kPlantumlJar = "plantuml.jar"

kAbpDitaa, kDitaaJar :: T.Text
kAbpDitaa = "DITAA"
kDitaaJar = "ditaa.jar"

kDiagramRenderers :: Maybe Format -> [(T.Text, T.Text)]
kDiagramRenderers fmt = map (bimap T.pack T.pack) $ concatMap mkEngine engines
    where
        engines = [ (name, "svg png pdf", \exe ext -> unwords [exe, "-T"++ext, "-o %o", "%i"])
                  | name <- words "dot neato twopi circo fdp sfdp patchwork osage"
                  ]
                  ++
                  [ ("plantuml", "svg png pdf", \_exe ext -> unwords ["java -jar {{PLANTUML}} -pipe -charset UTF-8", "-t"++ext, "< %i", "> %o"])
                  , ("asy", "svg png pdf", \exe ext -> unwords [exe, "-f", ext, "-o %o", "%i"])
                  , ("mmdc", "svg png pdf", \exe _ext -> unwords [exe, "-i %i", "-o %o"])
                  ]
                  ++
                  [ (name, "svg png pdf", \exe ext -> unwords [exe, "-a", "-T"++ext, "-o %o", "%i"])
                  | name <- words "actdiag  blockdiag  nwdiag  packetdiag  rackdiag  seqdiag"
                  ]
                  ++
                  [ ("ditaa", "svg png", \_exe ext -> unwords $ "java -jar {{DITAA}}" : [ "--svg" | ext == "svg" ] ++ ["-o", "-e UTF-8", "%i", "%o"])
                  ]
        mkEngine (exe, exts, cmd) =
            let exts'@(defaultHTML:defaultLaTeX:_) = words exts
                defaultExt = case fmt of
                    Just (Format "html") -> defaultHTML
                    Just (Format "latex") -> defaultLaTeX
                    Just (Format "beamer") -> defaultLaTeX
                    _ -> defaultHTML
            in (exe, replace "%o" ("%o."++defaultExt) (cmd exe defaultExt)) : [ (exe++"."++ext, replace "%o" ("%o."++ext) (cmd exe ext)) | ext <- exts' ]

kMeta, kIfdef, kValue, kIfndef :: T.Text
kMeta = "meta"
kIfdef = "ifdef"
kValue = "value"
kIfndef = "ifndef"

kVarOpen, kVarClose :: String
kVarOpen = "{{"
kVarClose = "}}"

kInclude, kFromLine, kToLine, kShift :: T.Text
kInclude = "include"
kFromLine = "fromline"
kToLine = "toline"
kShift = "shift"

kCmd :: T.Text
kCmd = "cmd"

kScriptArg :: Char
kScriptArg = '%'

kComment :: T.Text
kComment = "comment"

kRaw :: T.Text
kRaw = "raw"

kRender, kImg, kOut, kDiagArgIn, kDiagArgOut, kTitle, kTarget :: T.Text
kRender = "render"
kImg = "img"
kOut = "out"
kDiagArgIn = "%i"
kDiagArgOut = "%o"
kTitle = "title"
kTarget = "target"
