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
{-# LANGUAGE OverloadedStrings #-}
module DB.Consultas where

import DB.Models
import API.Types
import Database.Persist.Sql hiding ((==.))
import DB.Common
import Prelude hiding ((||))
import Database.Esqueleto
import Data.Text (Text)

-- | Busca un lugar con el nombre exacto o por nombres parecidos. Retorna una
-- lista de máximo 6 elementos con nombres parecidos junto con la coordenada
-- donde se ubica. La lista retornada está ordenada de acuerdo a lo parecido que
-- es el nombre con respecto a la consulta.
--
-- Dado que el usuario puede escribir con tildes o sin tildes algunas consultas
-- es que para la comparación se eliminan las tildes y se comparan los nombres
-- asi no más.
--
-- Esta consulta depende de dos extensiones de Postgres: fuzzystrmatch y unaccent
--
buscarLugar :: LugarNombreQuery -> SqlPersistT IO [(Entity Lugar, Entity Coordenada)]
buscarLugar (LugarNombreQuery busqueda) =
    select $
    from $ \(l, c) -> do
    where_ ((lower . unaccent $ l ^. LugarNombre) `like` (lower . unaccent $ likeStr))
    where_ (l ^. LugarCoordenada ==. c ^. CoordenadaId)
    orderBy [ asc (orderQuery l)]
    limit 6
    return (l, c)
    where
        likeStr :: SqlExpr (Value Text)
        likeStr = val "%" || val busqueda || val "%"

        orderQuery l = levenshtein (lower . unaccent $ l ^. LugarNombre, lower . unaccent $ likeStr)
