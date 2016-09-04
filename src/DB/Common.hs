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
module DB.Common where

import Database.Esqueleto
import Data.Text (Text)
import Database.Esqueleto.Internal.Sql




unaccent :: SqlExpr (Value Text) -> SqlExpr (Value Text)
unaccent = unsafeSqlFunction "unaccent"

lower :: SqlExpr (Value Text) -> SqlExpr (Value Text)
lower = unsafeSqlFunction "lower"

levenshtein :: (SqlExpr (Value Text), SqlExpr (Value Text)) -> SqlExpr (Value Int)
levenshtein = unsafeSqlFunction "levenshtein"


infixl 5 ||
(||) :: SqlExpr (Value Text) -> SqlExpr (Value Text) -> SqlExpr (Value Text)
(||) = fn
    where
        fn :: SqlExpr (Value Text) -> SqlExpr (Value Text) -> SqlExpr (Value Text)
        fn = unsafeSqlBinOp " || "
