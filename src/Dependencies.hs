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

module Dependencies
    ( trackFile
    , writeDependencies
    )
where

import Config
import Environment
import Tools
import UTF8

import Data.List

trackFile :: Env -> FilePath -> IO (FilePath, String)
trackFile e name = do
    addDep e name
    name' <- expandPath name
    contents <- readFileUTF8 name'
    return (name', contents)

writeDependencies :: Env -> IO ()
writeDependencies e = do
    ds <- getDeps e
    maybeTarget <- getVar e kAbpTarget
    case maybeTarget of
        Just target ->
            writeFile (target++".d") $ target++": "++unwords (sort (nub ds))++"\n"
        Nothing -> return ()
