open! Core

type t =
  | Eq of
      { expr : Parsecos_affine.t
      ; rhs : float
      }
  | Le of
      { expr : Parsecos_affine.t
      ; rhs : float
      }
  | Soc of
      { t : Parsecos_affine.t
      ; xs : Parsecos_affine.t list
      }

val eq : Parsecos_affine.t -> float -> t
val le : Parsecos_affine.t -> float -> t
val ge : Parsecos_affine.t -> float -> t
val soc : t:Parsecos_affine.t -> xs:Parsecos_affine.t list -> t
val vars : t -> (Parsecos_var.t, Parsecos_var.Scalar.comparator_witness) Set.t
