open! Core

type t =
  { pr : float array
  ; jc : int array
  ; ir : int array
  }
[@@deriving sexp_of]

type triplet =
  { row : int
  ; col : int
  ; value : float
  }

val of_triplets : nrows:int -> ncols:int -> triplet list -> t
val t_of_yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> t
val yojson_of_t : t -> Ppx_yojson_conv_lib.Yojson.Safe.t
