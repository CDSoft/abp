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
    ( Env
    , newEnv
    , setVar
    , getVar
    , quiet
    , getFormat
    , addDep
    , getDeps
    , runFile
    , runString
    , evalString
    , setRender
    )
where

import Config

import Control.Concurrent.MVar
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import Data.Maybe
import qualified Foreign.Lua as Lua
import Foreign.Lua.Core.Constants (multret)
import System.Environment
import Text.Pandoc.JSON

--import System.IO

data State = State
    { format :: Maybe Format
    , deps :: [FilePath]
    , luastate :: Lua.State
    }

type Env = MVar State

newEnv :: Maybe Format -> IO Env
newEnv maybeFormat = do
    --hPrint stderr "newstate"
    lua <- Lua.newstate
    --hPrint stderr "openlibs"
    Lua.runWith lua Lua.openlibs
    --hPrint stderr "openlibs done"
    mvar <- newMVar State { format = maybeFormat
                          , deps = []
                          , luastate = lua
                          }
    envVars <- getEnvironment
    forM_ envVars $ uncurry (setVar mvar)
    case maybeFormat of
        Just (Format fmt) -> setVar mvar "format" fmt
        Nothing -> return ()
    setVar mvar kAbpPath =<< getExecutablePath
    return mvar

setVar :: Env -> String -> String -> IO ()
setVar env var val = do
    state <- readMVar env
    --hPrint stderr ("setvar", var, val)
    Lua.runWith (luastate state) $ do
        Lua.push val
        Lua.setglobal' var
    --hPrint stderr "setvar done"

getVar :: Env -> String -> IO (Maybe String)
getVar env var = do
    state <- readMVar env
    --hPrint stderr ("getvar", var)
    val <- Lua.runWith (luastate state) $
        Lua.getglobal var *> Lua.peekEither (-1)
    --hPrint stderr "getvar done"
    return $ case val of
        Right s -> Just s
        Left _ -> Nothing

quiet :: Env -> IO Bool
quiet env = isJust <$> getVar env kAbpQuiet

getFormat :: Env -> IO (Maybe Format)
getFormat env = format <$> readMVar env

addDep :: Env -> FilePath -> IO ()
addDep env name = modifyMVar_ env (\s -> return s { deps = name : deps s })

getDeps :: Env -> IO [FilePath]
getDeps env = deps <$> readMVar env

runFile :: Env -> FilePath -> IO ()
runFile env name = do
    state <- readMVar env
    --hPrint stderr ("dofile", name)
    status <- Lua.runWith (luastate state) $ Lua.dofile name
    --hPrint stderr "dofile done"
    case status of
        Lua.OK -> return ()
        _ -> error $ "Can not execute Lua script: " ++ name

runString :: Env -> String -> IO ()
runString env s = do
    state <- readMVar env
    --hPrint stderr ("dostring", s)
    status <- Lua.runWith (luastate state) $ Lua.dostring (BS.pack s)
    --hPrint stderr "dostring done"
    case status of
        Lua.OK -> return ()
        _ -> error $ unlines ["Can not execute Lua script:", s]

evalString :: Env -> String -> IO String
evalString env s = do
    state <- readMVar env
    --hPrint stderr ("evalstring", s)
    value <- Lua.runWith (luastate state) $ do
        res <- Lua.loadstring (BS.pack ("return tostring(" ++ s ++ ")"))
        case res of
            Lua.OK -> Lua.pcall 0 multret Nothing *> Lua.peekEither (-1)
            _ -> return $ Left ("Can not compile Lua expression:" ++ s)
    --hPrint stderr "evalstring done"
    case value of
        Right value' -> return value'
        Left err -> error $ "Can not evaluate Lua expression:" ++ s ++ ": " ++ show err

setRender :: Env -> String -> String -> [(String, String)] -> IO ()
setRender env name render extrenders = do
    --hPrint stderr ("setRender", name, render, extrenders)
    state <- readMVar env
    Lua.runWith (luastate state) $ do
        Lua.newtable                -- render table

        Lua.createtable 0 2         -- metatable

        Lua.push "__tostring"
        Lua.pushHaskellFunction (luaConstantFunction (BS.pack render))
        Lua.rawset (-3)

        Lua.push "__index"
        Lua.push (M.fromList extrenders)
        Lua.rawset (-3)

        Lua.setmetatable (-2)               -- setmetatable(t, mt)
        Lua.setglobal name
    --hPrint stderr "setRender done"

luaConstantFunction :: BS.ByteString -> M.Map String String -> Lua.Lua BS.ByteString
luaConstantFunction value _ = return value
