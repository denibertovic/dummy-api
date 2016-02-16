## GET /boards

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[{"boardName":"Sample Board"}]
```

## POST /boards

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"boardName":"Board Name"}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"boardName":"Board Name"}
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
{"boardName":"Board Name"}
```

## POST /boards/:id

#### Captures:

- *id*: (integer) ID of the Resource

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"boardName":"Board Name"}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"boardName":"Board Name"}
```

## GET /boards/:id/scrolls

#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[{"scrollName":"Sample Scroll","scrollBoardId":1}]
```

## GET /cards

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[{"cardScrollId":1,"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title"}]
```

## POST /cards

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"cardScrollId":1,"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title"}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"cardScrollId":1,"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title"}
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
{"cardScrollId":1,"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title"}
```

## POST /cards/:id

#### Captures:

- *id*: (integer) ID of the Resource

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"cardScrollId":1,"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title"}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"cardScrollId":1,"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title"}
```

## GET /scrolls

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[{"scrollName":"Sample Scroll","scrollBoardId":1}]
```

## POST /scrolls

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"scrollName":"Sample Scroll","scrollBoardId":1}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"scrollName":"Sample Scroll","scrollBoardId":1}
```

## DELETE /scrolls/:id

#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## GET /scrolls/:id

#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"scrollName":"Sample Scroll","scrollBoardId":1}
```

## POST /scrolls/:id

#### Captures:

- *id*: (integer) ID of the Resource

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"scrollName":"Sample Scroll","scrollBoardId":1}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"scrollName":"Sample Scroll","scrollBoardId":1}
```

## GET /scrolls/:id/cards

#### Captures:

- *id*: (integer) ID of the Resource

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[{"cardScrollId":1,"cardAssignedTo":1,"cardDescription":"Sample description","cardTitle":"Sample title"}]
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


