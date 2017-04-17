## PUT /lugar/actualizar/:lugar-id/:secret

#### Authentication



Clients must supply the following data


#### Captures:

- *lugar-id*: El ID del lugar que se quiere modificar
- *secret*: Un token de acceso para evitar que hagan spam a la API

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"coordenada":1,"piso":"1","nombre":"517","tipo":"TipoSala","valido":true}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Posible error

```javascript
{"Left":"SecretIncorrecto"}
```

- 

```javascript
{"Right":{"coordenada":1,"piso":"1","nombre":"517","tipo":"TipoSala","valido":true}}
```

## POST /lugar/agregar/:secret

#### Authentication



Clients must supply the following data


#### Captures:

- *secret*: Un token de acceso para evitar que hagan spam a la API

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"latitud":-33.212,"longitud":-71.3122}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Posible error

```javascript
{"Left":"SecretIncorrecto"}
```

- El id de la entidad

```javascript
{"Right":1}
```

## GET /lugar/buscar

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
"517"
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Posible error

```javascript
{"Left":"SecretIncorrecto"}
```

- 

```javascript
{"Right":[]}
```

- 

```javascript
{"Right":[[{"coordenada":1,"piso":"1","nombre":"517","tipo":"TipoSala","id":1,"valido":true},{"latitud":-33.212,"id":1,"longitud":-71.3122}]]}
```

- 

```javascript
{"Right":[[{"coordenada":1,"piso":"1","nombre":"517","tipo":"TipoSala","id":1,"valido":true},{"latitud":-33.212,"id":1,"longitud":-71.3122}],[{"coordenada":1,"piso":"1","nombre":"517","tipo":"TipoSala","id":1,"valido":true},{"latitud":-33.212,"id":1,"longitud":-71.3122}]]}
```

- 

```javascript
{"Right":[[{"coordenada":1,"piso":"1","nombre":"517","tipo":"TipoSala","id":1,"valido":true},{"latitud":-33.212,"id":1,"longitud":-71.3122}],[{"coordenada":1,"piso":"1","nombre":"517","tipo":"TipoSala","id":1,"valido":true},{"latitud":-33.212,"id":1,"longitud":-71.3122}],[{"coordenada":1,"piso":"1","nombre":"517","tipo":"TipoSala","id":1,"valido":true},{"latitud":-33.212,"id":1,"longitud":-71.3122}]]}
```

## GET /lugar/caminos/:lugar-id/:secret

#### Authentication



Clients must supply the following data


#### Captures:

- *lugar-id*: El ID del lugar que se quiere modificar
- *secret*: Un token de acceso para evitar que hagan spam a la API

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Posible error

```javascript
{"Left":"SecretIncorrecto"}
```

- 

```javascript
{"Right":{"caminoMinimo":[{"latitud":-71.213,"longitud":31.32},{"latitud":-70.23,"longitud":30.2}],"caminoLargo":[{"latitud":-71.213,"longitud":31.32},{"latitud":-72.1,"longitud":31.32}]}}
```

## POST /lugar/reportar/:secret

#### Authentication



Clients must supply the following data


#### Captures:

- *secret*: Un token de acceso para evitar que hagan spam a la API

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"lugar":1,"reporter":"felipe@usach.cl","causa":"La sala no se encuentra en la EAO, está en el frontis de Estación Central"}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Posible error

```javascript
{"Left":"SecretIncorrecto"}
```

- El id de la entidad

```javascript
{"Right":1}
```

## GET /test

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
"text"
```

