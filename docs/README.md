## GET /boards

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
[]
```

- 

```javascript
[{"boardName":"Sample Board","boardOwnerId":1}]
```

- 

```javascript
[{"boardName":"Sample Board","boardOwnerId":1},{"boardName":"Sample Board","boardOwnerId":1}]
```

- 

```javascript
[{"boardName":"Sample Board","boardOwnerId":1},{"boardName":"Sample Board","boardOwnerId":1},{"boardName":"Sample Board","boardOwnerId":1}]
```

- 

```javascript
[{"boardName":"Sample Board","boardOwnerId":1},{"boardName":"Sample Board","boardOwnerId":1},{"boardName":"Sample Board","boardOwnerId":1},{"boardName":"Sample Board","boardOwnerId":1}]
```

## POST /boards

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"boardName":"Sample Board","boardOwnerId":1}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"boardName":"Sample Board","boardOwnerId":1}
```

## DELETE /boards/:id

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript

```

## GET /boards/:id

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"boardName":"Sample Board","boardOwnerId":1}
```

## POST /boards/:id

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: (integer) ID of the Resource

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"boardName":"Sample Board","boardOwnerId":1}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"boardName":"Sample Board","boardOwnerId":1}
```

## GET /boards/:id/cards

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
[]
```

- 

```javascript
[{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}]
```

- 

```javascript
[{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1},{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}]
```

- 

```javascript
[{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1},{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1},{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}]
```

- 

```javascript
[{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1},{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1},{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1},{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}]
```

## GET /boards/:id/lists

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
[]
```

- 

```javascript
[{"listBoardId":1,"listName":"Sample List"}]
```

- 

```javascript
[{"listBoardId":1,"listName":"Sample List"},{"listBoardId":1,"listName":"Sample List"}]
```

- 

```javascript
[{"listBoardId":1,"listName":"Sample List"},{"listBoardId":1,"listName":"Sample List"},{"listBoardId":1,"listName":"Sample List"}]
```

- 

```javascript
[{"listBoardId":1,"listName":"Sample List"},{"listBoardId":1,"listName":"Sample List"},{"listBoardId":1,"listName":"Sample List"},{"listBoardId":1,"listName":"Sample List"}]
```

## GET /cards

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
[]
```

- 

```javascript
[{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}]
```

- 

```javascript
[{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1},{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}]
```

- 

```javascript
[{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1},{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1},{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}]
```

- 

```javascript
[{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1},{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1},{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1},{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}]
```

## POST /cards

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}
```

## DELETE /cards/:id

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript

```

## GET /cards/:id

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}
```

## POST /cards/:id

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: (integer) ID of the Resource

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}
```

## GET /lists

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
[]
```

- 

```javascript
[{"listBoardId":1,"listName":"Sample List"}]
```

- 

```javascript
[{"listBoardId":1,"listName":"Sample List"},{"listBoardId":1,"listName":"Sample List"}]
```

- 

```javascript
[{"listBoardId":1,"listName":"Sample List"},{"listBoardId":1,"listName":"Sample List"},{"listBoardId":1,"listName":"Sample List"}]
```

- 

```javascript
[{"listBoardId":1,"listName":"Sample List"},{"listBoardId":1,"listName":"Sample List"},{"listBoardId":1,"listName":"Sample List"},{"listBoardId":1,"listName":"Sample List"}]
```

## POST /lists

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"listBoardId":1,"listName":"Sample List"}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"listBoardId":1,"listName":"Sample List"}
```

## DELETE /lists/:id

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript

```

## GET /lists/:id

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"listBoardId":1,"listName":"Sample List"}
```

## POST /lists/:id

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: (integer) ID of the Resource

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"listBoardId":1,"listName":"Sample List"}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"listBoardId":1,"listName":"Sample List"}
```

## GET /lists/:id/cards

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
[]
```

- 

```javascript
[{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}]
```

- 

```javascript
[{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1},{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}]
```

- 

```javascript
[{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1},{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1},{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}]
```

- 

```javascript
[{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1},{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1},{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1},{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}]
```

## GET /users

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
[]
```

- 

```javascript
[{"userFullName":"John Doe","userPassword":"password","userEmail":"john.doe@example.com"}]
```

- 

```javascript
[{"userFullName":"John Doe","userPassword":"password","userEmail":"john.doe@example.com"},{"userFullName":"John Doe","userPassword":"password","userEmail":"john.doe@example.com"}]
```

- 

```javascript
[{"userFullName":"John Doe","userPassword":"password","userEmail":"john.doe@example.com"},{"userFullName":"John Doe","userPassword":"password","userEmail":"john.doe@example.com"},{"userFullName":"John Doe","userPassword":"password","userEmail":"john.doe@example.com"}]
```

- 

```javascript
[{"userFullName":"John Doe","userPassword":"password","userEmail":"john.doe@example.com"},{"userFullName":"John Doe","userPassword":"password","userEmail":"john.doe@example.com"},{"userFullName":"John Doe","userPassword":"password","userEmail":"john.doe@example.com"},{"userFullName":"John Doe","userPassword":"password","userEmail":"john.doe@example.com"}]
```

## POST /users

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"userFullName":"John Doe","userPassword":"password","userEmail":"john.doe@example.com"}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"userFullName":"John Doe","userPassword":"password","userEmail":"john.doe@example.com"}
```

## DELETE /users/:id

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript

```

## GET /users/:id

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"userFullName":"John Doe","userPassword":"password","userEmail":"john.doe@example.com"}
```

## POST /users/:id

#### Authentication



Clients must supply the following data


#### Captures:

- *id*: (integer) ID of the Resource

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"userFullName":"John Doe","userPassword":"password","userEmail":"john.doe@example.com"}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"userFullName":"John Doe","userPassword":"password","userEmail":"john.doe@example.com"}
```


