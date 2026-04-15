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

let eq expr rhs = Eq { expr; rhs }
let le expr rhs = Le { expr; rhs }
let ge expr rhs = Le { expr = Parsecos_affine.scale (-1.0) expr; rhs = -.rhs }

let soc ~t ~xs =
  if List.is_empty xs then failwith "soc constraints require at least one x component";
  Soc { t; xs }
;;

let vars = function
  | Eq { expr; _ } | Le { expr; _ } -> Parsecos_affine.vars expr
  | Soc { t; xs } ->
    List.fold xs ~init:(Parsecos_affine.vars t) ~f:(fun acc expr ->
      Set.union acc (Parsecos_affine.vars expr))
;;
