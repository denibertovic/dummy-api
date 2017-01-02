## GET /boards

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[{"boardName":"Sample Board","boardOwnerId":1}]
```

## POST /boards

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"boardName":"Board Name","boardOwnerId":1}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"boardName":"Board Name","boardOwnerId":1}
```

## DELETE /boards/:id

#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## GET /boards/:id

#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"boardName":"Board Name","boardOwnerId":1}
```

## POST /boards/:id

#### Captures:

- *id*: (integer) ID of the Resource

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"boardName":"Board Name","boardOwnerId":1}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"boardName":"Board Name","boardOwnerId":1}
```

## GET /boards/:id/cards

#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}]
```

## GET /boards/:id/lists

#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[{"listBoardId":1,"listName":"Sample List"}]
```

## GET /cards

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}]
```

## POST /cards

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}
```

## DELETE /cards/:id

#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## GET /cards/:id

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

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}
```

## GET /lists

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[{"listBoardId":1,"listName":"Sample List"}]
```

## POST /lists

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"listBoardId":1,"listName":"Sample List"}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"listBoardId":1,"listName":"Sample List"}
```

## DELETE /lists/:id

#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## GET /lists/:id

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

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"listBoardId":1,"listName":"Sample List"}
```

## GET /lists/:id/cards

#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[{"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title","cardListId":1}]
```

## GET /users

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[{"userFullName":"John Doe","userEmail":"john.doe@example.com"},{"userFullName":"Jane Doe","userEmail":"jane.doe@example.com"}]
```

## POST /users

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"userFullName":"John Doe","userEmail":"john.doe@example.com"}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"userFullName":"John Doe","userEmail":"john.doe@example.com"}
```

## DELETE /users/:id

#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## GET /users/:id

#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"userFullName":"John Doe","userEmail":"john.doe@example.com"}
```

## POST /users/:id

#### Captures:

- *id*: (integer) ID of the Resource

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"userFullName":"John Doe","userEmail":"john.doe@example.com"}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"userFullName":"John Doe","userEmail":"john.doe@example.com"}
```


