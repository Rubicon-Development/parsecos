open! Core

module Term : sig
  type t =
    { param : string
    ; indices : int list
    ; coefficient : float
    }
  [@@deriving sexp_of, yojson]
end

type t =
  { constant : float
  ; terms : Term.t list
  }
[@@deriving sexp_of, yojson]

val zero : t
val constant : float -> t
val param : name:string -> indices:int list -> t
val add : t -> t -> t
val sub : t -> t -> t
val scale : float -> t -> t
val multiply : t -> t -> (t, Parsecos_error.t) result
val is_constant : t -> bool
val to_float : t -> float option
val unresolved_params : t -> string list
