open! Core

type t =
  { name : string
  ; dimensions : int array
  }
[@@deriving compare, sexp_of, yojson]

val scalar : string -> t
val array1 : name:string -> length:int -> t
val array2 : name:string -> dim1:int -> dim2:int -> t
val rank : t -> int
