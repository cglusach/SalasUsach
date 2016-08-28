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

type WithSecret = Capture "secret" Secret 


type SalasUSACHAPI =
         "lugar" :> "buscar" :> ReqBody '[JSON] LugarNombreQuery :>  Get '[JSON] (Respuesta [Lugar])
    :<|> "lugar" :> "agregar" :> ReqBody '[JSON] Lugar :> WithSecret :> Post '[JSON] (Respuesta LugarId)
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


buscarLugar :: LugarNombreQuery -> App (Respuesta [Lugar])
buscarLugar = undefined

agregarLugar :: Lugar -> Secret -> App (Respuesta LugarId)
agregarLugar _ = withSecret $ undefined

actualizarLugar :: Lugar -> LugarId -> Secret -> App (Respuesta Lugar)
actualizarLugar _ _ = withSecret $ undefined

obtenerCaminosLugar :: LugarId -> App (Respuesta Camino)
obtenerCaminosLugar = undefined

reportarLugar :: Reporte -> Secret -> App (Respuesta ReporteId)
reportarLugar _ = withSecret $ undefined
