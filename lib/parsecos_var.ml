open! Core

module Domain = struct
  type t =
    | Continuous
    | Boolean
    | Integer
  [@@deriving compare, sexp_of]
end

module Scalar = struct
  module T = struct
    type t =
      { id : int
      ; name : string
      ; domain : Domain.t
      }
    [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
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

let next_id =
  let id = ref 0 in
  fun () ->
    let current = !id in
    id := current + 1;
    current

let create ~name ~domain = { Scalar.id = next_id (); name; domain }
let continuous name = create ~name ~domain:Domain.Continuous
let boolean name = create ~name ~domain:Domain.Boolean
let integer name = create ~name ~domain:Domain.Integer
let name (t : t) = t.name
let domain (t : t) = t.domain

let array1 ~name ~domain ~length : array1 =
  if length < 0 then failwith (Printf.sprintf "array1 %s length must be non-negative" name);
  { name
  ; domain
  ; vars = Array.init length ~f:(fun index -> create ~name:(Printf.sprintf "%s[%d]" name index) ~domain)
  }

let array2 ~name ~domain ~dim1 ~dim2 : array2 =
  if dim1 < 0 then failwith (Printf.sprintf "array2 %s dim1 must be non-negative" name);
  if dim2 < 0 then failwith (Printf.sprintf "array2 %s dim2 must be non-negative" name);
  { name
  ; domain
  ; vars =
      Array.init dim1 ~f:(fun i ->
        Array.init dim2 ~f:(fun j -> create ~name:(Printf.sprintf "%s[%d,%d]" name i j) ~domain))
  }

let get1 (family : array1) index =
  if index < 0 || index >= Array.length family.vars
  then failwith (Printf.sprintf "index %d out of bounds for %s" index family.name)
  else family.vars.(index)

let get2 (family : array2) i j =
  if i < 0 || i >= Array.length family.vars
  then failwith (Printf.sprintf "index %d out of bounds for %s dimension 1" i family.name)
  else
    let row = family.vars.(i) in
    if j < 0 || j >= Array.length row
    then failwith (Printf.sprintf "index %d out of bounds for %s dimension 2" j family.name)
    else row.(j)
