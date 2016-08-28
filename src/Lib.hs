-- SalasUSACH - Aplicación para buscar salas en la Universidad de Santiago
--
-- Copyright (C) 2016 CGL USACH and Authors
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings  #-}
module Lib
    ( startApp
    , generateDocs
    , createAssets
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.JS
import API.API
import Config
import Control.Monad.Except
import Control.Monad.Reader 
import System.Environment (lookupEnv, getEnv)
import Text.Printf
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Database.Persist.Postgresql
import Control.Monad.Logger
import Network.Wai.Middleware.RequestLogger
import System.Directory
import Servant.Docs
import System.Remote.Monitoring (forkServer)
import qualified Data.Text as T

-- Defaults

-- | El puerto en el cual se va a ejecutar el servidor de la API
defaultPort :: Int
defaultPort = 3000

defaultPGPort :: Int
defaultPGPort = 5432

defaultPGHost :: String
defaultPGHost = "localhost"

defaultPGDBName :: String
defaultPGDBName = "salasusach"

defaultPGUser :: String
defaultPGUser = "salasusach"

defaultPGConnNumber :: Int
defaultPGConnNumber = 10

--------------------------------------------------------------------------------
-- APP
--------------------------------------------------------------------------------


-- | Inicia las conexiones a la DB, el servidor de la API y además escribe el JS
-- de la API en la carpeta de assets
startApp :: IO ()
startApp = do
    -- Leemos las variables de entorno
    port         <- liftM (fromMaybe defaultPort . fmap read) $ lookupEnv "PORT"
    pgPort       <- liftM (fromMaybe defaultPGPort . fmap read) $ lookupEnv "PGPORT"
    pgHost       <- liftM (fromMaybe defaultPGHost) $ lookupEnv "PGHOST"
    pgDBName     <- liftM (fromMaybe defaultPGDBName) $ lookupEnv "PGDBNAME"
    pgUser       <- liftM (fromMaybe defaultPGUser) $ lookupEnv "PGUSER"
    pgConnNumber <- liftM (fromMaybe defaultPGConnNumber . fmap read) $ lookupEnv "PGCONNNUMBER"
    pgPasswd     <- getEnv "PGPASS"
    secret       <- liftM (T.splitOn ":" . T.pack) $ getEnv "SECRET"
    -- Fin Envs
    createAssets
    generateJS
    generateDocs
    pool <- makePool pgHost pgPort pgUser pgPasswd pgDBName pgConnNumber
    let cfg = Config pool secret
        logger = logStdout
        {-logger = logStdoutDev-}
    _ <- forkServer "localhost" 8000
    run port $ logger $ salasUSACHApp cfg


salasUSACHApp :: Config -> Application
salasUSACHApp cfg = serve api (appToServer cfg)

appToServer :: Config -> Server SalasUSACHAPI
appToServer cfg = enter (convertApp cfg) apiServer

convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

type AppAPI = SalasUSACHAPI :<|> Raw

app :: Config -> Application
app cfg = serve appApi (appToServer cfg :<|> files)
    where
        files = serveDirectory "assets"

        appApi :: Proxy AppAPI
        appApi = Proxy


-- | Genera código JS automático para conectarse a la API
generateJS :: IO ()
generateJS = writeJSForAPI api (angular angularOps) "./assets/salas-api.js"
    where
        angularOps :: AngularOptions
        angularOps = defAngularOptions

createAssets :: IO ()
createAssets = do
    createDirectoryIfMissing False "assets"
    createDirectoryIfMissing False "docs"


generateDocs :: IO ()
generateDocs = writeFile "docs/docs.md" . markdown $ docs api
    

--------------------------------------------------------------------------------
-- DB
--------------------------------------------------------------------------------


-- | Crea una pool de conexiones a la DB que van a ser utilizadas para
-- conectarse a postgres
makePool :: String -- ^ El host donde se encuentra la DB de postgres
         -> Int    -- ^ El puerto donde se encuentra escuchando la DB
         -> String -- ^ El usuario con el cual conectarse a la DB
         -> String -- ^ La clave del usuario de la DB
         -> String -- ^ El nombre de la base de datos
         -> Int    -- ^ La cantidad de conexiones a crear
         -> IO ConnectionPool
makePool host port user passwd dbname connNumber = do
    let connStr = BS.pack $ printf "host=%s port=%d user=%s dbname=%s password=%s" host port user dbname passwd
    runStdoutLoggingT $ createPostgresqlPool connStr connNumber
