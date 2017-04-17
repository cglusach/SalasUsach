-- SalasUSACH - Aplicación para buscar salas en la Universidad de Santiago
--
-- Copyright (C) 2016-2017 CGL USACH and Authors
-- Copyright (C) 2011-2015 Felipe Garay
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
module API.Common where


import API.Types
import Config
import Control.Monad.Reader
import Data.List (find)

-- | Combinator que permite consultar si existe el token en la DB, en caso de
-- que no exista retorna como respuesta "Token Incorrecto", en caso que exista
-- ejecuta la función dada como argumento.
withSecret :: App (Respuesta a) -> Secret -> App (Respuesta a)
withSecret fn secret = do
    config <- ask
    case find (==(unSecret secret)) (getSecret config) of
      Nothing -> return . Respuesta . Left $ SecretIncorrecto
      Just _  -> fn
