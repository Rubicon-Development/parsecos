open! Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Term = struct
  type t =
    { param : string
    ; indices : int list
    ; coefficient : float
    }
  [@@deriving sexp_of, yojson]
end

module Key = struct
  module T = struct
    type t = string * int list [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

type t =
  { constant : float
  ; terms : Term.t list
  }
[@@deriving sexp_of, yojson]

let map_of_terms terms =
  List.fold
    terms
    ~init:(Map.empty (module Key))
    ~f:(fun acc term ->
      if Float.equal term.Term.coefficient 0.0
      then acc
      else
        Map.update acc (term.param, term.indices) ~f:(function
          | None -> term.coefficient
          | Some coefficient -> coefficient +. term.coefficient))
  |> Map.filter ~f:(fun coefficient -> not (Float.equal coefficient 0.0))
;;

let terms_of_map map =
  Map.to_alist map
  |> List.map ~f:(fun ((param, indices), coefficient) ->
    { Term.param; indices; coefficient })
;;

let normalize t = { constant = t.constant; terms = terms_of_map (map_of_terms t.terms) }
let zero = { constant = 0.0; terms = [] }
let constant constant = normalize { constant; terms = [] }

let param ~name ~indices =
  { constant = 0.0; terms = [ { Term.param = name; indices; coefficient = 1.0 } ] }
;;

let add left right =
  let terms =
    Map.merge
      (map_of_terms left.terms)
      (map_of_terms right.terms)
      ~f:(fun ~key:_ -> function
      | `Left coefficient | `Right coefficient ->
        if Float.equal coefficient 0.0 then None else Some coefficient
      | `Both (left_coefficient, right_coefficient) ->
        let coefficient = left_coefficient +. right_coefficient in
        if Float.equal coefficient 0.0 then None else Some coefficient)
  in
  { constant = left.constant +. right.constant; terms = terms_of_map terms }
;;

let scale factor t =
  if Float.equal factor 0.0
  then zero
  else
    normalize
      { constant = factor *. t.constant
      ; terms =
          List.map t.terms ~f:(fun term ->
            { term with coefficient = factor *. term.coefficient })
      }
;;

let sub left right = add left (scale (-1.0) right)
let is_constant t = List.is_empty t.terms
let to_float t = if is_constant t then Some t.constant else None

let unresolved_params t =
  List.map t.terms ~f:(fun term -> term.Term.param)
  |> List.dedup_and_sort ~compare:String.compare
;;

let multiply left right =
  if is_constant left
  then Ok (scale left.constant right)
  else if is_constant right
  then Ok (scale right.constant left)
  else
    Error
      (Parsecos_error.Nonlinear_expression
         "products of parameter expressions are not supported")
;;
