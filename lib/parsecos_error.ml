open! Core

type t =
  | Json_parse of string
  | Json_decode of string
  | Dsl_parse of string
  | Duplicate_variable of string
  | Unknown_variable of string
  | Expected_scalar of string
  | Expected_array1 of string
  | Expected_array2 of string
  | Negative_length of
      { name : string
      ; length : int
      }
  | Negative_dimension of
      { name : string
      ; axis : int
      ; length : int
      }
  | Index_out_of_bounds of
      { name : string
      ; index : int
      ; length : int
      }
  | Index2_out_of_bounds of
      { name : string
      ; axis : int
      ; index : int
      ; length : int
      }
[@@deriving sexp_of]
