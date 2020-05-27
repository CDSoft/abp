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
import Data.Bifunctor
import Data.Maybe
import qualified Data.Text as T
import System.Environment
import Text.Pandoc.JSON

data Env = Env { format :: Maybe Format
               , vars :: [(T.Text, Inline)]
               , quiet :: Bool
               , deps :: [FilePath]
               }

type EnvMVar = MVar Env

newEnv :: Maybe Format -> IO EnvMVar
newEnv maybeFormat = do
    envVars <- map (bimap T.pack (Str . T.pack)) <$> getEnvironment
    abpPath <- T.pack <$> getExecutablePath
    let vs = [ ("format", Str fmt) | Format fmt <- maybeToList maybeFormat ]
             ++ [ (kAbpPath, Str abpPath)
                ]
             ++ envVars
    let q = isJust (lookup kAbpQuiet vs)
    newMVar $ Env { format = maybeFormat
                  , vars = vs
                  , quiet = q
                  , deps = []
                  }

readEnv :: EnvMVar -> IO Env
readEnv = readMVar

setVar :: EnvMVar -> T.Text -> Inline -> IO ()
setVar mvar var val = modifyMVar_ mvar (\e -> return e { vars = (var, val) : vars e })

getVar :: EnvMVar -> T.Text -> IO (Maybe Inline)
getVar e var = lookup var . vars <$> readMVar e

getVarStr :: EnvMVar -> T.Text -> IO (Maybe T.Text)
getVarStr e var = mapM inlineToPlainText =<< getVar e var
