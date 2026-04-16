# parsecos

`parsecos` parses a small optimization DSL or JSON model into ECOS-compatible data.

It now supports two related outputs:

- `Ecos_data`: fully numeric solver input
- `Ecos_template`: reusable symbolic solver input with parameter placeholders

## Numeric DSL

The existing numeric DSL path substitutes parameter values before building solver data.

```text
params:
  demand = [3, 5]

vars:
  continuous x[2]

minimize:
  x[0] + x[1]

subject to:
  x[0] = demand[0]
  x[1] = demand[1]
```

Use:

- library: `Parsecos.ecos_data_of_text`
- HTTP: `POST /v1/dsl`

## Template DSL

The template path keeps params symbolic and returns `Ecos_template` instead of `Ecos_data`.

Declare params by shape only:

```text
params:
  alpha
  price[2]
  demand[2]
  cap[1]

vars:
  continuous x[2]

minimize:
  (2 * price[0] - alpha + 3) * x[0] + price[1] * x[1] + demand[0]

subject to:
  x[0] + x[1] = demand[1]
  price[0] * x[0] <= cap[0]
  x[1] >= 0
```

Use:

- library: `Parsecos.ecos_template_of_text`
- HTTP: `POST /v1/template/dsl`

## Template Semantics

`Ecos_template` is a symbolic version of `Ecos_data`.

Every numeric field that may depend on params is represented as a formula:

```json
{
  "constant": 3.0,
  "terms": [
    { "param": "alpha", "indices": [], "coefficient": -1.0 },
    { "param": "price", "indices": [0], "coefficient": 2.0 }
  ]
}
```

Interpretation:

```text
3.0 + (-1.0 * alpha) + (2.0 * price[0])
```

To turn an `Ecos_template` into concrete ECOS data, evaluate every formula after supplying all parameter values.

## Supported Param Arithmetic

Allowed:

- `price[t] * x[t]`
- `(2 * price[t] - alpha + 3) * x[t]`
- `demand[t] + 5`
- `x[t] <= cap[t]`

Rejected:

- `alpha * beta`
- `x * y`
- `price[t] * demand[t]`

The intent is that the model remains linear/SOCP once params are filled in.

## HTTP Example

Request:

```http
POST /v1/template/dsl
Content-Type: text/plain
```

```text
params:
  alpha
  price[2]
  demand[2]
  cap[1]

vars:
  continuous x[2]

minimize:
  (2 * price[0] - alpha + 3) * x[0] + price[1] * x[1] + demand[0]

subject to:
  x[0] + x[1] = demand[1]
  price[0] * x[0] <= cap[0]
  x[1] >= 0
```

Response shape:

```json
{
  "params": [
    { "name": "alpha", "dimensions": [] },
    { "name": "price", "dimensions": [2] },
    { "name": "demand", "dimensions": [2] },
    { "name": "cap", "dimensions": [1] }
  ],
  "c": [
    {
      "constant": 3.0,
      "terms": [
        { "param": "alpha", "indices": [], "coefficient": -1.0 },
        { "param": "price", "indices": [0], "coefficient": 2.0 }
      ]
    },
    {
      "constant": 0.0,
      "terms": [
        { "param": "price", "indices": [1], "coefficient": 1.0 }
      ]
    }
  ],
  "a": {
    "pr": [
      { "constant": 1.0, "terms": [] },
      { "constant": 1.0, "terms": [] }
    ],
    "jc": [0, 1, 2],
    "ir": [0, 0]
  },
  "g": {
    "pr": [
      {
        "constant": 0.0,
        "terms": [
          { "param": "price", "indices": [0], "coefficient": 1.0 }
        ]
      },
      { "constant": -1.0, "terms": [] }
    ],
    "jc": [0, 1, 2],
    "ir": [0, 1]
  },
  "b": [
    {
      "constant": 0.0,
      "terms": [
        { "param": "demand", "indices": [1], "coefficient": 1.0 }
      ]
    }
  ],
  "h": [
    {
      "constant": 0.0,
      "terms": [
        { "param": "cap", "indices": [0], "coefficient": 1.0 }
      ]
    },
    { "constant": 0.0, "terms": [] }
  ],
  "objective_offset": {
    "constant": 0.0,
    "terms": [
      { "param": "demand", "indices": [0], "coefficient": 1.0 }
    ]
  }
}
```

## Manual Filling Example

Given parameter values:

```json
{
  "alpha": 4.0,
  "price": [10.0, 11.0],
  "demand": [7.0, 8.0],
  "cap": [12.0]
}
```

You evaluate formulas entry-by-entry:

- `c[0] = 3 - alpha + 2 * price[0] = 3 - 4 + 20 = 19`
- `c[1] = price[1] = 11`
- `A.pr = [1, 1]`
- `G.pr[0] = price[0] = 10`
- `G.pr[1] = -1`
- `b[0] = demand[1] = 8`
- `h[0] = cap[0] = 12`
- `h[1] = 0`
- `objective_offset = demand[0] = 7`

Resulting concrete ECOS data:

```json
{
  "c": [19.0, 11.0],
  "a": {
    "pr": [1.0, 1.0],
    "jc": [0, 1, 2],
    "ir": [0, 0]
  },
  "g": {
    "pr": [10.0, -1.0],
    "jc": [0, 1, 2],
    "ir": [0, 1]
  },
  "b": [8.0],
  "h": [12.0, 0.0],
  "objective_offset": 7.0
}
```

The remaining metadata fields like `n`, `m`, `p`, `q`, `column_names`, `bool_vars_idx`, and `int_vars_idx` are copied directly from the template.

## Library API

Main entry points:

```ocaml
val ecos_data_of_text : string -> (Ecos_data.t, Error.t) result
val ecos_data_of_json_string : string -> (Ecos_data.t, Error.t) result

val ecos_template_of_text : string -> (Ecos_template.t, Error.t) result
val ecos_template_of_json_string : string -> (Ecos_template.t, Error.t) result
```
