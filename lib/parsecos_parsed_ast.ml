open! Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type domain =
  | Continuous
  | Boolean
  | Integer
[@@deriving sexp_of, yojson]

type var_decl =
  | Scalar of
      { name : string
      ; domain : domain
      }
  | Array1 of
      { name : string
      ; domain : domain
      ; length : int
      }
  | Array2 of
      { name : string
      ; domain : domain
      ; dim1 : int
      ; dim2 : int
      }
[@@deriving sexp_of, yojson]

type reference =
  | Scalar_ref of string
  | Array1_ref of
      { name : string
      ; index : int
      }
  | Array2_ref of
      { name : string
      ; index1 : int
      ; index2 : int
      }
[@@deriving sexp_of, yojson]

type expr =
  | Constant of float
  | Var of reference
  | Sum of expr list
  | Sub of expr * expr
  | Scale of
      { coefficient : float
      ; expr : expr
      }
[@@deriving sexp_of, yojson]

type constraint_ =
  | Eq of
      { lhs : expr
      ; rhs : expr
      }
  | Le of
      { lhs : expr
      ; rhs : expr
      }
  | Ge of
      { lhs : expr
      ; rhs : expr
      }
  | Soc of
      { t : expr
      ; xs : expr list
      }
[@@deriving sexp_of, yojson]

type objective = Minimize of expr [@@deriving sexp_of, yojson]

type model =
  { vars : var_decl list
  ; objective : objective
  ; constraints : constraint_ list
  }
[@@deriving sexp_of, yojson]
