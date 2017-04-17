module Graph.Path where
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

import Data.Graph.Inductive hiding ((><))
import Data.Graph.Inductive.Query.SP
import Data.Maybe (fromJust)
import Data.List (find)
import Database.Persist.Sql (fromSqlKey)
import Data.Angle
import DB.Models
import DB.Types


fst' :: (a, b, c) -> a
fst' (x, _, _) = x

snd' :: (a, b, c) -> b
snd' (_, x, _) = x

thr' :: (a, b, c) -> c
thr' (_, _, x) = x


unzipX :: ([a], [b]) -> [(a, b)]
unzipX (xs, ys) = zip xs ys


type Grafo = Gr Int Double

type Nodos = [(Int, Coordenada, [(Int, Coordenada)])]

crearGrafo :: Nodos -> Grafo
crearGrafo xs = mkGraph (genLNodes xs) (genLEdges xs)


genLNodes :: Nodos -> [LNode Int]
genLNodes = map (\x -> (fst' x, fst' x))


genLEdges :: Nodos -> [LEdge Double]
genLEdges = concat . map (\nodo -> map (\sig -> (fst' nodo, fst sig, distanciaMetros (snd' nodo) (snd sig))) (thr' nodo))


caminoMinimo :: NodoId -> NodoId -> [(NodoId, Coordenada, [(NodoId, Coordenada)])] -> [(Int, Coordenada)]
caminoMinimo init end nodos = map (\t -> (fst' t, snd' t))
                            . map fromJust 
                            . map (\i -> find (\n -> i == fst' n) allToInt) $ camino
    where
        camino = sp (toInt init) (toInt end) . crearGrafo $  allToInt

        allToInt = map (\(nId, c, xs) -> (toInt nId, c, map (\(x, y) -> (toInt x, y)) xs)) $ nodos

        toInt = fromIntegral . fromSqlKey




distanciaMetros :: (Distancia a, Distancia b) => a -> b -> Double
distanciaMetros coor1 coor2 = haversine (Degrees . fst . coordenadas $ coor1, Degrees . snd . coordenadas $ coor1)
                                        (Degrees . fst . coordenadas $ coor2, Degrees . snd . coordenadas $ coor2)

metros :: [Metro]
metros = [estacionCentral, usach]
    where
        estacionCentral :: Metro
        estacionCentral = Metro EstacionCentral (-33.4506668219) (-70.6792564259)

        usach :: Metro
        usach = Metro MetroUSACH (-33.4525153323) (-70.6860531435)


haversine :: (Degrees Double, Degrees Double) -> (Degrees Double, Degrees Double) -> Double
haversine coor1 coor2 =  (\x -> x * 2 * 6367 * 1000) . asin . sqrt $ haversine' (tupleMap radians coor1) (tupleMap radians coor2)
    where
        tupleMap :: (a -> b) -> (a, a) -> (b, b)
        tupleMap f (x, y) = (f x, f y)


        haversine' :: (Radians Double, Radians Double) -> (Radians Double, Radians Double) -> Double
        haversine' (Radians lat1, Radians lng1) (Radians lat2, Radians lng2) =
            sin ((lat2 - lat1) / 2) ^ 2 + cos lat1 * cos lat2 * sin ((lng2 - lng1) / 2) ^ 2
