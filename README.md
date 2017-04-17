Este programa fue realizado con la esperanza que le resulte útil pero sin
ninguna garantía.

# Servidor de API de Salas USACH

Este es el servidor de la API de Salas USACH que unifica los esfuerzos de la
página web y la aplicación móvil en un solo motor. Utiliza las siguientes
tecnologías:


* [Haskell](https://www.haskell.org/): Lenguaje funcional de tipado estático.
* [GHC](https://www.haskell.org/ghc/): Compilador de Haskell libre.
* [Stack](https://github.com/commercialhaskell/stack): Programa multiplaforma para el desarrollo de programas en Haskell.
* [PostgreSQL](https://www.postgresql.org/): Base de datos.
* [Warp](https://hackage.haskell.org/package/warp): Servidor HTTP 1.0, 1.1 y 2.0.
* [Esqueleto](https://hackage.haskell.org/package/esqueleto): EDSL para consultas SQL.
* [Servant](https://github.com/haskell-servant/servant): DSL a nivel de tipado para definir APIs.


# Requisitos

* [Stack](https://github.com/commercialhaskell/stack)
* libpq-dev

# Compilación

Se puede generar un binario del proyecto con:

```
stack build
```

# Ejecución

```
stack exec SalasUSACHAPI-exe
```

El programa utiliza variables de entorno para poder conectarse a la base de
datos y definir el puerto en el cual se está ejecutando:

* PORT: El puerto en el cual va a escuchar peticiones el servidor.
* PGPORT: El puerto de la base de datos PostgreSQL.
* PGHOST: La dirección donde se encuentra la base de datos PostgreSQL.
* PGDBNAME: El nombre de la base de datos.
* PGUSER: El nombre de usuario de la base de datos.
* PGCONNNUMBER: Cantidad de conexiones a utilizar en el pool.
* PGPASS: La clave de la base de datos.
* SECRET: Uno o varios strings separados por ":" que permiten el acceso a
  algunas rutas del sistema.

# Documentación de la API


La documentación se encuentra en la carpeta docs/ en el repositorio.


# Licencia Programa

Este programa, SalasUSACH, se encuentra bajo la licencia pública general GPL
versión 3 o versión posterior y se comparte con la esperanza que le resulte útil
pero sin ninguna garantía.


```
SalasUSACH - Aplicación para buscar salas en la Universidad de Santiago

Copyright (C) 2016 CGL USACH and Authors

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
```

# Licencia datos

Los datos origianles migrados de la versión original de SalasUSACH son de
propiedad de Felipe Garay con todos los derechos reservados.

Los nuevos datos agregados en esta versión de Salas USACH así como
modificaciones a los datos originales son de propiedad de las personas listadas
en AUTHORS quienes se reservan todos los derechos.

Queda prohibida toda copia o uso de los datos utilizados en esta aplicación.


