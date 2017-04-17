-- SalasUSACH - Aplicación para buscar salas en la Universidad de Santiago
--
-- Copyright (C) 2016-2017 CGL USACH and Authors
-- Copyright (C) 2011-2016 Felipe Garay
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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module DB.Models where

import Control.Monad.Reader
import Database.Persist.Sql
import Database.Persist.TH
import Database.Persist.Quasi
import Data.Text (Text)
import DB.Types
import Servant.Docs
import Data.Proxy

import Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "src/DB/models")


-- | Ejecuta las migraciones de la DB
doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll


-- | Ejecuta una consulta a la base de datos
runDB :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDB query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

--------------------------------------------------------------------------------
-- Ejemplos de datos
--------------------------------------------------------------------------------

instance ToSample Reporte where
    toSamples _ = singleSample r
        where
            r = Reporte 
                "felipe@usach.cl"
                "La sala no se encuentra en la EAO, está en el frontis de Estación Central"
                (toSqlKey 1)


instance ToSample Lugar where
    toSamples _ = singleSample l
        where
            l = Lugar "517" "1" True (toSqlKey 1) TipoSala


instance ToSample Coordenada where
    toSamples _ = singleSample c
        where
            c = Coordenada (-33.212) (-71.3122)


instance (ToBackendKey SqlBackend a) => ToSample (Key a) where
    toSamples _ = [("El id de la entidad", toSqlKey 1)]


instance (ToSample (Key a), PersistEntity a, ToSample a) => ToSample (Entity a) where
    toSamples _ = map (\((_, i), (s, x)) -> (s, Entity i x)) $ zip (toSamples (Proxy :: Proxy (Key a))) (toSamples (Proxy :: Proxy a)) 



instance Distancia Coordenada where
    coordenadas l = (coordenadaLatitud l, coordenadaLongitud l)


instance (Distancia b) => Distancia (a, b) where
    coordenadas (_, x) = coordenadas x
