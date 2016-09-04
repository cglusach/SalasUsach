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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module API.API where


import API.Types
import DB.Models
import Servant
import Config
import API.Common
import Servant.Docs
import qualified DB.Consultas as DB
import Database.Persist
import Control.Monad (liftM, void)

type WithSecret = Capture "secret" Secret 


type SalasUSACHAPI =
         "lugar" :> "buscar" :> ReqBody '[JSON] LugarNombreQuery :>  Get '[JSON] (Respuesta [(Entity Lugar, Entity Coordenada)])
    :<|> "lugar" :> "agregar" :> ReqBody '[JSON] Lugar :> ReqBody '[JSON] Coordenada :> WithSecret :> Post '[JSON] (Respuesta LugarId)
    :<|> "lugar" :> "actualizar" :> ReqBody '[JSON] Lugar :> Capture "lugar-id" LugarId :> WithSecret :> Put '[JSON] (Respuesta Lugar)
    :<|> "lugar" :> "caminos" :> Capture "lugar-id" LugarId :> Get '[JSON] (Respuesta Camino)
    :<|> "lugar" :> "reportar" :> ReqBody '[JSON] Reporte :> WithSecret :> Post '[JSON] (Respuesta ReporteId)


instance ToCapture (Capture "lugar-id" LugarId) where
    toCapture _ = DocCapture "lugar-id" "El ID del lugar que se quiere modificar"

instance ToCapture (Capture "secret" Secret) where
    toCapture _ = DocCapture "secret" "Un token de acceso para evitar que hagan spam a la API"

--------------------------------------------------------------------------------

api :: Proxy SalasUSACHAPI
api = Proxy

-- | Este es el conjunto de funciones que sirven la API de SalasUSACHAPI
apiServer :: ServerT SalasUSACHAPI App
apiServer =
         buscarLugar
    :<|> agregarLugar
    :<|> actualizarLugar
    :<|> obtenerCaminosLugar
    :<|> reportarLugar

--------------------------------------------------------------------------------
-- API Functions
--------------------------------------------------------------------------------


buscarLugar :: LugarNombreQuery -> App (Respuesta [(Entity Lugar, Entity Coordenada)])
buscarLugar lugar = returnRespuesta . runDB $ DB.buscarLugar lugar


-- | Agrega un nuevo lugar al sistema. Dado que no sabemos si está correcto o
-- no, debemos guardarlo inidcando que no está validado el lugar.
agregarLugar :: Lugar -> Coordenada -> Secret -> App (Respuesta LugarId)
agregarLugar lugar coordenada = withSecret $ do
    runDB $ do
        coordenadaId <- insert coordenada
        let lugar' = lugar { lugarValido = False, lugarCoordenada =  coordenadaId}
        liftM (Respuesta . Right) $ insert lugar'


actualizarLugar :: Lugar -> LugarId -> Secret -> App (Respuesta Lugar)
actualizarLugar lugar lugarId = withSecret . runDB $ do
    -- Verificamos primero si el lugar existe
    existe <- get lugarId
    case existe of
      Nothing -> return . Respuesta $ Left LugarNoExiste
      Just _ -> do
          void $ replace lugarId lugar
          return . Respuesta $ Right lugar

obtenerCaminosLugar :: LugarId -> App (Respuesta Camino)
obtenerCaminosLugar = undefined

reportarLugar :: Reporte -> Secret -> App (Respuesta ReporteId)
reportarLugar _ = withSecret $ undefined
