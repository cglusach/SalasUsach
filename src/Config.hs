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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Config where

import Database.Persist.Postgresql
import Servant
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Data.Text (Text)

-- | La aplicación en general utilizada
newtype App a = App
    { runApp :: ReaderT Config (ExceptT ServantErr IO) a
    } deriving ( Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr, MonadIO)

-- | Datos del entorno de ejecución
data Config = Config
    { getPool :: ConnectionPool     -- ^ Pool de conexiones a la DB
    , getSecret :: [Text]           -- ^ Posibles secretos válidos
    }


-- | En que ambiente estamos ejecutando la aplicación
data Environment = Development
                 | Test
                 | Production
                 deriving (Eq, Show, Read)
