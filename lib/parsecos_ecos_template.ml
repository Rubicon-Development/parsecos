open! Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Matrix = struct
  type t =
    { pr : Parsecos_formula.t array
    ; jc : int array
    ; ir : int array
    }
  [@@deriving sexp_of, yojson]

  type triplet =
    { row : int
    ; col : int
    ; value : Parsecos_formula.t
    }

  let of_triplets ~nrows:_ ~ncols triplets =
    let by_col = Array.init ncols ~f:(fun _ -> []) in
    List.iter triplets ~f:(fun { row; col; value } ->
      by_col.(col) <- (row, value) :: by_col.(col));
    let nnz = List.length triplets in
    let pr = Array.create ~len:nnz Parsecos_formula.zero in
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
  ;;
end

type t =
  { params : Parsecos_param.t list
  ; n : int
  ; m : int
  ; p : int
  ; l : int
  ; q : int array
  ; e : int
  ; g : Matrix.t
  ; a : Matrix.t option
  ; c : Parsecos_formula.t array
  ; h : Parsecos_formula.t array
  ; b : Parsecos_formula.t array option
  ; bool_vars_idx : int array
  ; int_vars_idx : int array
  ; column_names : string array
  ; objective_offset : Parsecos_formula.t
  }
[@@deriving sexp_of, yojson]
