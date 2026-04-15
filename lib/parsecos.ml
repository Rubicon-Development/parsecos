open! Core

module Var = struct
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
end

module Affine = struct
  type t =
    { constant : float
    ; terms : float Map.M(Var.Scalar).t
    }

  let zero = { constant = 0.0; terms = Map.empty (module Var.Scalar) }
  let constant constant = { zero with constant }
  let of_var var = { constant = 0.0; terms = Map.singleton (module Var.Scalar) var 1.0 }
  let term coefficient var = if Float.equal coefficient 0.0 then zero else { constant = 0.0; terms = Map.singleton (module Var.Scalar) var coefficient }

  let add_term terms var coefficient =
    if Float.equal coefficient 0.0
    then terms
    else
      Map.change terms var ~f:(function
        | None -> Some coefficient
        | Some existing ->
          let updated = existing +. coefficient in
          if Float.equal updated 0.0 then None else Some updated)

  let add left right =
    let terms =
      Map.fold right.terms ~init:left.terms ~f:(fun ~key ~data acc -> add_term acc key data)
    in
    { constant = left.constant +. right.constant; terms }

  let sub left right = add left { constant = -.right.constant; terms = Map.map right.terms ~f:Float.neg }

  let scale factor expr =
    if Float.equal factor 0.0
    then zero
    else
      { constant = factor *. expr.constant
      ; terms = Map.filter_map expr.terms ~f:(fun coefficient ->
          let scaled = factor *. coefficient in
          if Float.equal scaled 0.0 then None else Some scaled)
      }

  let sum exprs = List.fold exprs ~init:zero ~f:add
  let terms expr = expr.terms
  let constant_term expr = expr.constant
  let vars expr = Map.key_set expr.terms
end

module Constraint = struct
  type t =
    | Eq of
        { expr : Affine.t
        ; rhs : float
        }
    | Le of
        { expr : Affine.t
        ; rhs : float
        }
    | Soc of
        { t : Affine.t
        ; xs : Affine.t list
        }

  let eq expr rhs = Eq { expr; rhs }
  let le expr rhs = Le { expr; rhs }
  let ge expr rhs = Le { expr = Affine.scale (-1.0) expr; rhs = -.rhs }

  let soc ~t ~xs =
    if List.is_empty xs then failwith "soc constraints require at least one x component";
    Soc { t; xs }

  let vars = function
    | Eq { expr; _ }
    | Le { expr; _ } -> Affine.vars expr
    | Soc { t; xs } ->
      List.fold xs ~init:(Affine.vars t) ~f:(fun acc expr -> Set.union acc (Affine.vars expr))
end

module Csc_matrix = struct
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

  let of_triplets ~nrows:_ ~ncols triplets =
    let by_col = Array.init ncols ~f:(fun _ -> []) in
    List.iter triplets ~f:(fun { row; col; value } -> by_col.(col) <- (row, value) :: by_col.(col));
    let nnz = List.length triplets in
    let pr = Array.create ~len:nnz 0.0 in
    let ir = Array.create ~len:nnz 0 in
    let jc = Array.create ~len:(ncols + 1) 0 in
    let position = ref 0 in
    for col = 0 to ncols - 1 do
      jc.(col) <- !position;
      by_col.(col)
      |> List.sort ~compare:(fun (left, _) (right, _) -> Int.compare left right)
      |> List.iter ~f:(fun (row, value) ->
        pr.(!position) <- value;
        ir.(!position) <- row;
        Int.incr position)
    done;
    jc.(ncols) <- !position;
    { pr; jc; ir }
end

module Ecos_data = struct
  type t =
    { n : int
    ; m : int
    ; p : int
    ; l : int
    ; q : int array
    ; e : int
    ; g : Csc_matrix.t
    ; a : Csc_matrix.t option
    ; c : float array
    ; h : float array
    ; b : float array option
    ; bool_vars_idx : int array
    ; int_vars_idx : int array
    ; column_names : string array
    ; objective_offset : float
    }
  [@@deriving sexp_of]
end

module Problem = struct
  type t =
    { objective : Affine.t
    ; constraints : Constraint.t list
    }

  let minimize objective = { objective; constraints = [] }
  let subject_to problem constraints = { problem with constraints = problem.constraints @ constraints }

  module Row = struct
    type t =
      { coeffs : float Map.M(Var.Scalar).t
      ; rhs : float
      }
  end

  let used_variables problem =
    let objective_vars = Affine.vars problem.objective in
    List.fold problem.constraints ~init:objective_vars ~f:(fun acc constraint_ -> Set.union acc (Constraint.vars constraint_))
    |> Set.to_list

  let dense_vector ~columns coeffs =
    let vector = Array.create ~len:(Map.length columns) 0.0 in
    Map.iteri coeffs ~f:(fun ~key:var ~data:coefficient ->
      let column = Map.find_exn columns var in
      vector.(column) <- coefficient);
    vector

  let eq_row expr rhs = { Row.coeffs = Affine.terms expr; rhs = rhs -. Affine.constant_term expr }
  let le_row expr rhs = { Row.coeffs = Affine.terms expr; rhs = rhs -. Affine.constant_term expr }

  let soc_rows exprs =
    List.map exprs ~f:(fun expr ->
      { Row.coeffs = Map.map (Affine.terms expr) ~f:Float.neg; rhs = Affine.constant_term expr })

  let triplets_of_rows ~columns rows =
    List.concat_mapi rows ~f:(fun row_index row ->
      Map.to_alist row.Row.coeffs
      |> List.map ~f:(fun (var, coefficient) ->
        { Csc_matrix.row = row_index; col = Map.find_exn columns var; value = coefficient }))

  let to_ecos_data problem =
    let variables = used_variables problem in
    let columns =
      List.mapi variables ~f:(fun index var -> var, index)
      |> Map.of_alist_exn (module Var.Scalar)
    in
    let equality_rows, linear_rows, soc_blocks =
      List.fold problem.constraints ~init:([], [], []) ~f:(fun (eqs, les, socs) constraint_ ->
        match constraint_ with
        | Constraint.Eq { expr; rhs } -> eq_row expr rhs :: eqs, les, socs
        | Constraint.Le { expr; rhs } -> eqs, le_row expr rhs :: les, socs
        | Constraint.Soc { t; xs } -> eqs, les, soc_rows (t :: xs) :: socs)
    in
    let equality_rows = List.rev equality_rows in
    let linear_rows = List.rev linear_rows in
    let soc_blocks = List.rev soc_blocks in
    let g_rows = linear_rows @ List.concat soc_blocks in
    let q = soc_blocks |> List.map ~f:List.length |> Array.of_list in
    let column_names = variables |> List.map ~f:Var.name |> Array.of_list in
    let c = dense_vector ~columns (Affine.terms problem.objective) in
    let h = Array.of_list (List.map g_rows ~f:(fun row -> row.Row.rhs)) in
    let b = Array.of_list (List.map equality_rows ~f:(fun row -> row.Row.rhs)) in
    let a =
      if List.is_empty equality_rows
      then None
      else Some (Csc_matrix.of_triplets ~nrows:(List.length equality_rows) ~ncols:(List.length variables) (triplets_of_rows ~columns equality_rows))
    in
    let g =
      Csc_matrix.of_triplets ~nrows:(List.length g_rows) ~ncols:(List.length variables) (triplets_of_rows ~columns g_rows)
    in
    let bool_vars_idx, int_vars_idx =
      variables
      |> List.filter_mapi ~f:(fun index var ->
        match Var.domain var with
        | Var.Domain.Continuous -> None
        | Var.Domain.Boolean -> Some (`Bool index)
        | Var.Domain.Integer -> Some (`Int index))
      |> List.fold ~init:([], []) ~f:(fun (bools, ints) tag ->
        match tag with
        | `Bool index -> index :: bools, ints
        | `Int index -> bools, index :: ints)
      |> fun (bools, ints) -> Array.of_list (List.rev bools), Array.of_list (List.rev ints)
    in
    { Ecos_data.n = List.length variables
    ; m = List.length g_rows
    ; p = List.length equality_rows
    ; l = List.length linear_rows
    ; q
    ; e = 0
    ; g
    ; a
    ; c
    ; h
    ; b = if List.is_empty equality_rows then None else Some b
    ; bool_vars_idx
    ; int_vars_idx
    ; column_names
    ; objective_offset = Affine.constant_term problem.objective
    }
end
