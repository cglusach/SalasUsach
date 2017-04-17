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
module DB.Types where


import Database.Persist.TH
import Data.Aeson
import Data.Aeson.TH


-- | Identifica el tipo de lugar
data Tipo = TipoSala    -- ^ El lugar es una sala
          | TipoLugar   -- ^ Es un lugar que no es sala
          deriving (Eq, Read, Show)

$(derivePersistField "Tipo")
$(deriveJSON defaultOptions ''Tipo)


data NombreMetro = 
     MetroUSACH
   | EstacionCentral

data Metro = Metro
    { nombre        :: !NombreMetro
    , metroLatitud  :: !Double
    , metroLongitud :: !Double
    }



class Distancia a where
    coordenadas :: a -> (Double, Double)


instance Distancia Metro where
    coordenadas (Metro _ lat lng) = (lat, lng)

