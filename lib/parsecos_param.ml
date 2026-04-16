open! Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t =
  { name : string
  ; dimensions : int array
  }
[@@deriving compare, sexp_of, yojson]

let scalar name = { name; dimensions = [||] }
let array1 ~name ~length = { name; dimensions = [| length |] }
let array2 ~name ~dim1 ~dim2 = { name; dimensions = [| dim1; dim2 |] }
let rank t = Array.length t.dimensions
