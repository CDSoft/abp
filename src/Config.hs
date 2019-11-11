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

module Config
where

kABPQuiet :: String
kABPQuiet = "ABP_QUIET"

kAbpPath :: String
kAbpPath = "ABP_PATH"

kAbpPlantuml, kPlantumlJar :: String
kAbpPlantuml = "ABP_PLANTUML"
kPlantumlJar = "plantuml.jar"

kDiagramRenderers :: [(String, String)]
kDiagramRenderers = concatMap mkEngine engines
    where
        engines = [ ("dot", "svg png pdf", \exe ext -> unwords [exe, "-T"++ext, "-o %o %i"])
                  , ("plantuml", "svg png pdf", \_exe ext -> unwords ["java -jar {{ABP_PLANTUML}} -pipe -charset UTF-8", "-t"++ext, "< %i", "> %o"])
                  , ("asy", "svg png pdf", \exe ext -> unwords [exe, "-f", ext, "-o %o %i"])
                  , ("mmdc", "svg png pdf", \exe _ext -> unwords [exe, "-i %i -o %o"])
                  ]
                  ++ [ (name, "svg png pdf", \exe ext -> unwords [exe, "-a", "-T"++ext, "-o %o %i"])
                     | name <- words "actdiag  blockdiag  nwdiag  packetdiag  rackdiag  seqdiag"
                     ]
        mkEngine (exe, exts, cmd) =
            let exts'@(defaultExt:_) = words exts
            in (exe, cmd exe defaultExt) : [ (exe++"."++ext, cmd exe ext) | ext <- exts' ]

kMeta, kIfdef, kValue, kIfndef :: String
kMeta = "meta"
kIfdef = "ifdef"
kValue = "value"
kIfndef = "ifndef"

kVarOpen, kVarClose :: String
kVarOpen = "{{"
kVarClose = "}}"

kInclude, kFromLine, kToLine :: String
kInclude = "include"
kFromLine = "fromline"
kToLine = "toline"

kCmd, kClasses :: String
kCmd = "cmd"
kClasses = "classes"

kScriptArg :: Char
kScriptArg = '%'

kComment :: String
kComment = "comment"

kRaw :: String
kRaw = "raw"

kRender, kImg, kOut, kDiagArgIn, kDiagArgOut, kTitle, kTarget :: String
kRender = "render"
kImg = "img"
kOut = "out"
kDiagArgIn = "%i"
kDiagArgOut = "%o"
kTitle = "title"
kTarget = "target"
