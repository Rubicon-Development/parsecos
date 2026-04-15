open! Core

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
