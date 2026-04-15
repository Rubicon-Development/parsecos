open! Core

module Domain : sig
  type t =
    | Continuous
    | Boolean
    | Integer
  [@@deriving compare, sexp_of]
end

module Scalar : sig
  type t =
    { id : int
    ; name : string
    ; domain : Domain.t
    }
  [@@deriving compare, sexp_of]

  include Comparator.S with type t := t
end

type t = Scalar.t [@@deriving sexp_of]

type array1 =
  { name : string
  ; domain : Domain.t
  ; vars : t array
  }

type array2 =
  { name : string
  ; domain : Domain.t
  ; vars : t array array
  }

val create : name:string -> domain:Domain.t -> t
val continuous : string -> t
val boolean : string -> t
val integer : string -> t
val name : t -> string
val domain : t -> Domain.t
val array1 : name:string -> domain:Domain.t -> length:int -> array1
val array2 : name:string -> domain:Domain.t -> dim1:int -> dim2:int -> array2
val get1 : array1 -> int -> t
val get2 : array2 -> int -> int -> t
