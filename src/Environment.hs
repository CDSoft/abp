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

module Environment
    ( Env(..)
    , EnvMVar
    , newEnv
    , readEnv
    , setVar
    , getVar, getVarStr
    )
where

import Config
import Tools

import Control.Concurrent.MVar
import Data.Maybe
import System.Environment
import Text.Pandoc.JSON

data Env = Env { format :: Maybe Format
               , vars :: [(String, Inline)]
               , quiet :: Bool
               }

type EnvMVar = MVar Env

newEnv :: Maybe Format -> IO EnvMVar
newEnv maybeFormat = do
    envVars <- map (\(var, val) -> (var, Str val)) <$> getEnvironment
    abpPath <- getExecutablePath
    let vs = [ ("format", Str fmt) | Format fmt <- maybeToList maybeFormat ]
             ++ [ (kAbpPath, Str abpPath)
                ]
             ++ envVars
    let q = isJust (lookup kABPQuiet vs)
    newMVar $ Env { format = maybeFormat
                  , vars = vs
                  , quiet = q
                  }

readEnv :: EnvMVar -> IO Env
readEnv = readMVar

setVar :: EnvMVar -> String -> Inline -> IO ()
setVar mvar var val = modifyMVar_ mvar (\e -> return e { vars = (var, val) : vars e })

getVar :: EnvMVar -> String -> IO (Maybe Inline)
getVar e var = lookup var . vars <$> readMVar e

getVarStr :: EnvMVar -> String -> IO (Maybe String)
getVarStr e var = mapM inlineToPlainText =<< getVar e var
