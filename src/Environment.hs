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

data State = State
    { format :: Maybe Format
    , deps :: [FilePath]
    , luastate :: Lua.State
    }

type Env = MVar State

newEnv :: Maybe Format -> IO Env
newEnv maybeFormat = do
    lua <- Lua.newstate
    Lua.runWith lua Lua.openlibs
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
    Lua.runWith (luastate state) $ do
        Lua.push val
        Lua.setglobal' var

getVar :: Env -> String -> IO (Maybe String)
getVar = evalString

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
    status <- Lua.runWith (luastate state) $ Lua.dofile name
    case status of
        Lua.OK -> return ()
        _ -> error $ "Can not execute Lua script: " ++ name

runString :: Env -> String -> IO ()
runString env s = do
    state <- readMVar env
    status <- Lua.runWith (luastate state) $ Lua.dostring (BS.pack s)
    case status of
        Lua.OK -> return ()
        _ -> error $ unlines ["Can not execute Lua script:", s]

evalString :: Env -> String -> IO (Maybe String)
evalString env s = do
    state <- readMVar env
    value <- Lua.runWith (luastate state) $ do
        res <- Lua.loadstring (BS.pack ("return tostring(" ++ s ++ ")"))
        case res of
            Lua.OK -> Lua.pcall 0 multret Nothing *> Lua.peekEither (-1)
            _ -> return $ Left ("Can not compile Lua expression:" ++ s)
    return $ case value of
        Right "nil" -> Nothing -- I don't like this trick. How to detect nil values???
        Right value' -> Just value'
        Left err -> error $ "Can not evaluate Lua expression:" ++ s ++ ": " ++ show err

setRender :: Env -> String -> String -> [(String, String)] -> IO ()
setRender env name render extrenders = do
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

luaConstantFunction :: BS.ByteString -> M.Map String String -> Lua.Lua BS.ByteString
luaConstantFunction value _ = return value
