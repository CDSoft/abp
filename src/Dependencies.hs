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

module Dependencies
    ( trackFile
    , writeDependencies
    )
where

import Config
import qualified Data.Text as T
import Environment
import Patterns
import Tools
import UTF8

import Control.Concurrent.MVar
import Data.List

addDep :: EnvMVar -> FilePath -> IO ()
addDep mvar name = modifyMVar_ mvar (\e -> return e { deps = name : deps e })

trackFile :: EnvMVar -> FilePath -> IO (FilePath, T.Text)
trackFile e name = do
    addDep e name
    name' <- expandPath name
    contents <- readFileUTF8 name'
    return (name', contents)

writeDependencies :: EnvMVar -> IO ()
writeDependencies e = do
    Env { vars = vs, deps = ds } <- readMVar e
    whenJust (lookup kAbpTarget vs) $ \target -> do
        target' <- T.unpack <$> inlineToPlainText target
        writeFile (target'++".d") $ target'++": "++unwords (sort (nub ds))++"\n"
