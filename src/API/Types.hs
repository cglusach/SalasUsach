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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell  #-}
module API.Types where

import Data.Aeson
import Data.Text (Text)
import Data.Aeson.TH
import DB.Models
import Servant
import Servant.Docs
import Config


-- | Petición de búsqueda del nombre de un lugar
newtype LugarNombreQuery = LugarNombreQuery Text
    deriving (FromJSON, ToJSON, Show, Read)

-- | El secreto a comprobar
newtype Secret = Secret { unSecret :: Text }
    deriving (FromJSON, ToJSON, Show, Read, FromHttpApiData)


-- | El camino a seguir por la persona
data Camino = Camino
    { caminoMinimo :: [Coordenada]
    , caminoLargo  :: [Coordenada]
    }

$(deriveJSON defaultOptions ''Camino)

-- | Posibles errores a retornar
data ErrorRespuesta =
      SecretIncorrecto -- ^ El secreto dado es incorrecto
  | LugarNoExiste      -- ^ El lugar no existe en la DB
    deriving (Show, Read)

$(deriveJSON defaultOptions ''ErrorRespuesta)


-- | La respuesta del servidor puede ser correcta (Right) o un ErrorRespuesta
newtype Respuesta a = Respuesta (Either ErrorRespuesta a)
    deriving (FromJSON, ToJSON, Show, Read)

returnRespuesta :: App a -> App (Respuesta a)
returnRespuesta m = m >>= \x -> return . Respuesta . Right $ x


--------------------------------------------------------------------------------
-- Ejemplos
--------------------------------------------------------------------------------

instance (ToSample a) => ToSample (Respuesta a) where
    toSamples _ = [("Posible error", malo)] ++ xs
        where
            malo = Respuesta . Left $ SecretIncorrecto
            xs = map (\(x, y) -> (x, Respuesta . Right $ y)) $ toSamples (Proxy :: Proxy a)


instance ToSample LugarNombreQuery where
    toSamples _ = singleSample $ LugarNombreQuery "517"


instance ToSample Camino where
    toSamples _ = singleSample $ Camino minimo largo
        where
            minimo = [Coordenada (-71.213) (31.32), Coordenada (-70.23) (30.2)]
            largo  = [Coordenada (-71.213) (31.32), Coordenada (-72.1) (31.32)]

instance ToSample Text where
    toSamples _ = singleSample $ "text"
