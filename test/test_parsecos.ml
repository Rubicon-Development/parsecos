open! Core
open Parsecos

let print_sexp sexp_of value = print_s (sexp_of value)

let print_metadata data =
  let payload =
    [%sexp
      { column_names = (data.Ecos_data.column_names : string array)
      ; c = (data.c : float array)
      ; bool_vars_idx = (data.bool_vars_idx : int array)
      ; int_vars_idx = (data.int_vars_idx : int array)
      }]
  in
  print_s payload

let print_linear_data data =
  let payload =
    [%sexp
      { p = (data.Ecos_data.p : int)
      ; m = (data.m : int)
      ; l = (data.l : int)
      ; a = (data.a : Csc_matrix.t option)
      ; b = (data.b : float array option)
      ; g = (data.g : Csc_matrix.t)
      ; h = (data.h : float array)
      }]
  in
  print_s payload

let print_soc_data data =
  let payload =
    [%sexp
      { l = (data.Ecos_data.l : int)
      ; q = (data.q : int array)
      ; g = (data.g : Csc_matrix.t)
      ; h = (data.h : float array)
      }]
  in
  print_s payload

let print_full_data data =
  let payload =
    [%sexp
      { column_names = (data.Ecos_data.column_names : string array)
      ; c = (data.c : float array)
      ; p = (data.p : int)
      ; m = (data.m : int)
      ; l = (data.l : int)
      ; q = (data.q : int array)
      ; a = (data.a : Csc_matrix.t option)
      ; b = (data.b : float array option)
      ; g = (data.g : Csc_matrix.t)
      ; h = (data.h : float array)
      ; bool_vars_idx = (data.bool_vars_idx : int array)
      ; int_vars_idx = (data.int_vars_idx : int array)
      }]
  in
  print_s payload

let print_parse_result = function
  | Ok data ->
    print_s
      [%sexp
        { column_names = (data.Ecos_data.column_names : string array)
        ; c = (data.c : float array)
        ; p = (data.p : int)
        ; m = (data.m : int)
        ; l = (data.l : int)
        ; q = (data.q : int array)
        ; a = (data.a : Csc_matrix.t option)
        ; b = (data.b : float array option)
        ; g = (data.g : Csc_matrix.t)
        ; h = (data.h : float array)
        ; bool_vars_idx = (data.bool_vars_idx : int array)
        ; int_vars_idx = (data.int_vars_idx : int array)
         ; objective_offset = (data.objective_offset : float)
         }]
  | Error error -> print_s [%sexp (error : Parsecos.Error.t)]

let%expect_test "variable ordering and integrality metadata" =
  let x = Var.continuous "x" in
  let b = Var.boolean "b" in
  let u = Var.array1 ~name:"u" ~domain:Var.Domain.Integer ~length:3 in
  let y = Var.array2 ~name:"y" ~domain:Var.Domain.Continuous ~dim1:2 ~dim2:2 in
  let objective =
    Affine.sum
      [ Affine.term 1.0 x
      ; Affine.term 2.0 b
      ; Affine.term 10.0 (Var.get1 u 0)
      ; Affine.term 11.0 (Var.get1 u 1)
      ; Affine.term 12.0 (Var.get1 u 2)
      ; Affine.term 20.0 (Var.get2 y 0 0)
      ; Affine.term 21.0 (Var.get2 y 0 1)
      ; Affine.term 22.0 (Var.get2 y 1 0)
      ; Affine.term 23.0 (Var.get2 y 1 1)
      ]
  in
  let data = Problem.minimize objective |> Problem.to_ecos_data in
  print_metadata data;
  [%expect
    {|
    ((column_names (x b u[0] u[1] u[2] y[0,0] y[0,1] y[1,0] y[1,1]))
     (c (1 2 10 11 12 20 21 22 23)) (bool_vars_idx (1)) (int_vars_idx (2 3 4)))
    |}]

let%expect_test "objective constants and coefficient merging" =
  let x = Var.continuous "x_merge" in
  let y = Var.continuous "y_cancel" in
  let objective =
    Affine.sum
      [ Affine.term 2.0 x
      ; Affine.term 3.0 x
      ; Affine.constant 7.0
      ; Affine.term 4.0 y
      ; Affine.scale (-1.0) (Affine.term 4.0 y)
      ]
  in
  let data = Problem.minimize objective |> Problem.to_ecos_data in
  print_s [%sexp { column_names = (data.column_names : string array); c = (data.c : float array); objective_offset = (data.objective_offset : float) }];
  [%expect
    {|
    ((column_names (x_merge)) (c (5)) (objective_offset 7))
    |}]

let%expect_test "linear equalities and inequalities canonicalize into A b G h" =
  let x = Var.continuous "x_lin" in
  let y = Var.continuous "y_lin" in
  let constraints =
    [ Constraint.eq
        (Affine.sum [ Affine.term 1.0 x; Affine.term 2.0 y; Affine.constant 3.0 ])
        10.0
    ; Constraint.le
        (Affine.sum [ Affine.term 1.0 x; Affine.term (-1.0) y; Affine.constant 1.0 ])
        4.0
    ; Constraint.ge
        (Affine.sum [ Affine.term 2.0 x; Affine.constant 5.0 ])
        1.0
    ]
  in
  let data =
    Problem.minimize Affine.zero
    |> fun problem -> Problem.subject_to problem constraints
    |> Problem.to_ecos_data
  in
  print_linear_data data;
  [%expect
    {|
    ((p 1) (m 2) (l 2) (a (((pr (1 2)) (jc (0 1 2)) (ir (0 0))))) (b ((7)))
     (g ((pr (1 -2 -1)) (jc (0 2 3)) (ir (0 1 0)))) (h (3 4)))
    |}]

let%expect_test "soc constraints become cone blocks in G and h" =
  let t = Var.continuous "t_soc" in
  let x = Var.continuous "x_soc" in
  let y = Var.continuous "y_soc" in
  let constraint_ =
    Constraint.soc
      ~t:(Affine.sum [ Affine.of_var t; Affine.constant 1.0 ])
      ~xs:
        [ Affine.sum [ Affine.of_var x; Affine.constant (-2.0) ]
        ; Affine.term 3.0 y
        ]
  in
  let data =
    Problem.minimize Affine.zero
    |> fun problem -> Problem.subject_to problem [ constraint_ ]
    |> Problem.to_ecos_data
  in
  print_soc_data data;
  [%expect
    {|
    ((l 0) (q (3)) (g ((pr (-1 -1 -3)) (jc (0 1 2 3)) (ir (0 1 2))))
     (h (1 -2 0)))
    |}]

let%expect_test "mixed integer metadata includes indexed families" =
  let x = Var.continuous "x_meta" in
  let b = Var.array1 ~name:"b_meta" ~domain:Var.Domain.Boolean ~length:2 in
  let k = Var.array2 ~name:"k_meta" ~domain:Var.Domain.Integer ~dim1:2 ~dim2:2 in
  let objective =
    Affine.sum
      [ Affine.of_var x
      ; Affine.of_var (Var.get1 b 0)
      ; Affine.of_var (Var.get1 b 1)
      ; Affine.of_var (Var.get2 k 0 0)
      ; Affine.of_var (Var.get2 k 0 1)
      ; Affine.of_var (Var.get2 k 1 0)
      ; Affine.of_var (Var.get2 k 1 1)
      ]
  in
  let data = Problem.minimize objective |> Problem.to_ecos_data in
  print_metadata data;
  [%expect
    {|
    ((column_names
      (x_meta b_meta[0] b_meta[1] k_meta[0,0] k_meta[0,1] k_meta[1,0]
       k_meta[1,1]))
     (c (1 1 1 1 1 1 1)) (bool_vars_idx (1 2)) (int_vars_idx (3 4 5 6)))
    |}]

let%expect_test "time indexed equalities flatten deterministically" =
  let x = Var.array1 ~name:"x_dyn" ~domain:Var.Domain.Continuous ~length:3 in
  let u = Var.array1 ~name:"u_dyn" ~domain:Var.Domain.Boolean ~length:2 in
  let dynamics =
    List.init 2 ~f:(fun step ->
      Constraint.eq
        (Affine.sum
           [ Affine.of_var (Var.get1 x (step + 1))
           ; Affine.scale (-1.0) (Affine.of_var (Var.get1 x step))
           ; Affine.scale (-2.0) (Affine.of_var (Var.get1 u step))
           ])
        0.0)
  in
  let data =
    Problem.minimize Affine.zero
    |> fun problem -> Problem.subject_to problem dynamics
    |> Problem.to_ecos_data
  in
  print_s [%sexp { column_names = (data.column_names : string array); a = (data.a : Csc_matrix.t option); b = (data.b : float array option); bool_vars_idx = (data.bool_vars_idx : int array) }];
  [%expect
    {|
    ((column_names (x_dyn[0] x_dyn[1] x_dyn[2] u_dyn[0] u_dyn[1]))
     (a (((pr (-1 1 -1 1 -2 -2)) (jc (0 1 3 4 5 6)) (ir (0 0 1 1 0 1)))))
     (b ((0 0))) (bool_vars_idx (3 4)))
    |}]

let%expect_test "time indexed mixed integer socp example" =
  let x = Var.array1 ~name:"x_misocp" ~domain:Var.Domain.Continuous ~length:3 in
  let on = Var.array1 ~name:"on_misocp" ~domain:Var.Domain.Boolean ~length:2 in
  let level = Var.array1 ~name:"level_misocp" ~domain:Var.Domain.Integer ~length:2 in
  let objective =
    Affine.sum
      [ Affine.term 1.0 (Var.get1 x 2)
      ; Affine.term 1.0 (Var.get1 on 0)
      ; Affine.term 1.0 (Var.get1 on 1)
      ; Affine.term 2.0 (Var.get1 level 0)
      ; Affine.term 2.0 (Var.get1 level 1)
      ]
  in
  let dynamics =
    List.init 2 ~f:(fun step ->
      Constraint.eq
        (Affine.sum
           [ Affine.of_var (Var.get1 x (step + 1))
           ; Affine.scale (-1.0) (Affine.of_var (Var.get1 x step))
           ; Affine.scale (-2.0) (Affine.of_var (Var.get1 on step))
           ; Affine.scale (-1.0) (Affine.of_var (Var.get1 level step))
           ])
        0.0)
  in
  let bounds = [ Constraint.le (Affine.of_var (Var.get1 x 0)) 10.0 ] in
  let cones =
    [ Constraint.soc
        ~t:(Affine.sum [ Affine.of_var (Var.get1 x 1); Affine.constant 3.0 ])
        ~xs:
          [ Affine.sum [ Affine.of_var (Var.get1 x 0); Affine.constant (-1.0) ]
          ; Affine.of_var (Var.get1 level 0)
          ]
    ; Constraint.soc
        ~t:(Affine.sum [ Affine.of_var (Var.get1 x 2); Affine.constant 3.0 ])
        ~xs:
          [ Affine.sum [ Affine.of_var (Var.get1 x 1); Affine.constant (-1.0) ]
          ; Affine.of_var (Var.get1 level 1)
          ]
    ]
  in
  let data =
    Problem.minimize objective
    |> fun problem -> Problem.subject_to problem (dynamics @ bounds @ cones)
    |> Problem.to_ecos_data
  in
  print_full_data data;
   [%expect
     {|
     ((column_names
       (x_misocp[0] x_misocp[1] x_misocp[2] on_misocp[0] on_misocp[1]
        level_misocp[0] level_misocp[1]))
      (c (0 0 1 1 1 2 2)) (p 2) (m 7) (l 1) (q (3 3))
      (a
       (((pr (-1 1 -1 1 -2 -2 -1 -1)) (jc (0 1 3 4 5 6 7 8))
         (ir (0 0 1 1 0 1 0 1)))))
      (b ((0 0)))
      (g ((pr (1 -1 -1 -1 -1 -1 -1)) (jc (0 2 4 5 5 5 6 7)) (ir (0 2 1 5 4 3 6))))
      (h (10 3 -1 0 3 -1 0)) (bool_vars_idx (3 4)) (int_vars_idx (5 6)))
     |}]

let%expect_test "json input elaborates with declared unused variables preserved" =
  let model : Parsecos.Parsed_ast.model =
    { vars =
        [ Scalar { name = "x_json"; domain = Continuous }
        ; Array1 { name = "on_json"; domain = Boolean; length = 2 }
        ; Scalar { name = "unused_json"; domain = Integer }
        ]
    ; objective =
        Minimize
          (Sum
             [ Var (Scalar_ref "x_json")
             ; Scale
                 { coefficient = 2.0
                 ; expr = Var (Array1_ref { name = "on_json"; index = 0 })
                 }
             ])
    ; constraints =
        [ Le
            { lhs = Var (Array1_ref { name = "on_json"; index = 1 })
            ; rhs = Constant 1.0
            }
        ]
    }
  in
  let data =
    model
    |> Parsecos.Parsed_ast.yojson_of_model
    |> Yojson.Safe.to_string
    |> Parsecos.ecos_data_of_json_string
  in
  print_parse_result data;
  [%expect
    {|
    ((column_names (x_json on_json[0] on_json[1] unused_json)) (c (1 2 0 0))
     (p 0) (m 1) (l 1) (q ()) (a ()) (b ())
     (g ((pr (1)) (jc (0 0 0 1 1)) (ir (0)))) (h (1)) (bool_vars_idx (1 2))
     (int_vars_idx (3)) (objective_offset 0))
    |}]

let%expect_test "dsl input elaborates with linear and soc constraints" =
  let text =
    {|vars:
  continuous x_dsl
  boolean on_dsl[2]
  integer unused_dsl

minimize:
  x_dsl + on_dsl[0] + 3

subject to:
  x_dsl - on_dsl[1] = 2
  norm(x_dsl - 1, on_dsl[0]) <= x_dsl + 4
|}
  in
  let data = Parsecos.ecos_data_of_text text in
  print_parse_result data;
  [%expect
    {|
    ((column_names (x_dsl on_dsl[0] on_dsl[1] unused_dsl)) (c (1 1 0 0))
     (p 1) (m 3) (l 0) (q (3)) (a (((pr (1 -1)) (jc (0 1 1 2 2)) (ir (0 0)))))
     (b ((2))) (g ((pr (-1 -1 -1)) (jc (0 2 3 3 3)) (ir (0 1 2)))) (h (4 -1 0))
     (bool_vars_idx (1 2)) (int_vars_idx (3)) (objective_offset 3))
    |}]

let%expect_test "dsl example for a day-scale bes site" =
  let loads =
    [| 28; 26; 24; 23; 22; 24; 30; 38; 44; 48; 52; 55; 57; 56; 54; 50; 46; 44; 42; 40; 38; 36; 34; 31 |]
  in
  let pv_available =
    [| 0; 0; 0; 0; 0; 2; 8; 16; 24; 32; 36; 38; 40; 36; 30; 22; 12; 4; 0; 0; 0; 0; 0; 0 |]
  in
  let horizon = Array.length loads in
  let bounds_for_time step pv_cap =
    [ Printf.sprintf "grid[%d] >= 0" step
    ; Printf.sprintf "pv[%d] >= 0" step
    ; Printf.sprintf "pv[%d] <= %d" step pv_cap
    ; Printf.sprintf "charge[%d] >= 0" step
    ; Printf.sprintf "charge[%d] <= 20" step
    ; Printf.sprintf "discharge[%d] >= 0" step
    ; Printf.sprintf "discharge[%d] <= 20" step
    ; Printf.sprintf "gen[%d] >= 0" step
    ; Printf.sprintf "gen[%d] <= 40" step
    ]
  in
  let power_balance_constraints =
    Array.to_list loads
    |> List.mapi ~f:(fun step load ->
      Printf.sprintf "grid[%d] + pv[%d] + discharge[%d] + gen[%d] - charge[%d] = %d" step step step step step load)
  in
  let soc_dynamics =
    List.init horizon ~f:(fun step ->
      Printf.sprintf "soc[%d] - soc[%d] - charge[%d] + discharge[%d] = 0" (step + 1) step step step)
  in
  let operating_bounds =
    Array.to_list pv_available
    |> List.concat_mapi ~f:(fun step pv_cap -> bounds_for_time step pv_cap)
  in
  let soc_bounds =
    List.init (horizon + 1) ~f:(fun step ->
      [ Printf.sprintf "soc[%d] >= 0" step; Printf.sprintf "soc[%d] <= 80" step ])
    |> List.concat
  in
  let objective_terms =
    List.init horizon ~f:(fun step -> Printf.sprintf "grid[%d]" step)
    @ List.init horizon ~f:(fun step -> Printf.sprintf "5 * gen[%d]" step)
  in
  let text =
    String.concat
      ~sep:"\n"
      ([ "vars:"
       ; "  continuous grid[24]"
       ; "  continuous pv[24]"
       ; "  continuous charge[24]"
       ; "  continuous discharge[24]"
       ; "  continuous soc[25]"
       ; "  continuous gen[24]"
       ; ""
       ; "minimize:"
       ; "  " ^ String.concat ~sep:" + " objective_terms
       ; ""
       ; "subject to:"
       ; "  # Serve fixed site load with grid, PV, battery, and generator"
       ]
       @ List.map power_balance_constraints ~f:(fun line -> "  " ^ line)
       @ [ ""; "  # Battery state dynamics and end-of-day return" ]
       @ List.map soc_dynamics ~f:(fun line -> "  " ^ line)
       @ [ "  soc[0] = 40"; "  soc[24] = 40"; ""; "  # Device limits" ]
       @ List.map operating_bounds ~f:(fun line -> "  " ^ line)
       @ List.map soc_bounds ~f:(fun line -> "  " ^ line))
  in
  print_endline text;
  let data = Parsecos.ecos_data_of_text text in
  print_parse_result data;
  [%expect
    {|
    vars:
      continuous grid[24]
      continuous pv[24]
      continuous charge[24]
      continuous discharge[24]
      continuous soc[25]
      continuous gen[24]

    minimize:
      grid[0] + grid[1] + grid[2] + grid[3] + grid[4] + grid[5] + grid[6] + grid[7] + grid[8] + grid[9] + grid[10] + grid[11] + grid[12] + grid[13] + grid[14] + grid[15] + grid[16] + grid[17] + grid[18] + grid[19] + grid[20] + grid[21] + grid[22] + grid[23] + 5 * gen[0] + 5 * gen[1] + 5 * gen[2] + 5 * gen[3] + 5 * gen[4] + 5 * gen[5] + 5 * gen[6] + 5 * gen[7] + 5 * gen[8] + 5 * gen[9] + 5 * gen[10] + 5 * gen[11] + 5 * gen[12] + 5 * gen[13] + 5 * gen[14] + 5 * gen[15] + 5 * gen[16] + 5 * gen[17] + 5 * gen[18] + 5 * gen[19] + 5 * gen[20] + 5 * gen[21] + 5 * gen[22] + 5 * gen[23]

    subject to:
      # Serve fixed site load with grid, PV, battery, and generator
      grid[0] + pv[0] + discharge[0] + gen[0] - charge[0] = 28
      grid[1] + pv[1] + discharge[1] + gen[1] - charge[1] = 26
      grid[2] + pv[2] + discharge[2] + gen[2] - charge[2] = 24
      grid[3] + pv[3] + discharge[3] + gen[3] - charge[3] = 23
      grid[4] + pv[4] + discharge[4] + gen[4] - charge[4] = 22
      grid[5] + pv[5] + discharge[5] + gen[5] - charge[5] = 24
      grid[6] + pv[6] + discharge[6] + gen[6] - charge[6] = 30
      grid[7] + pv[7] + discharge[7] + gen[7] - charge[7] = 38
      grid[8] + pv[8] + discharge[8] + gen[8] - charge[8] = 44
      grid[9] + pv[9] + discharge[9] + gen[9] - charge[9] = 48
      grid[10] + pv[10] + discharge[10] + gen[10] - charge[10] = 52
      grid[11] + pv[11] + discharge[11] + gen[11] - charge[11] = 55
      grid[12] + pv[12] + discharge[12] + gen[12] - charge[12] = 57
      grid[13] + pv[13] + discharge[13] + gen[13] - charge[13] = 56
      grid[14] + pv[14] + discharge[14] + gen[14] - charge[14] = 54
      grid[15] + pv[15] + discharge[15] + gen[15] - charge[15] = 50
      grid[16] + pv[16] + discharge[16] + gen[16] - charge[16] = 46
      grid[17] + pv[17] + discharge[17] + gen[17] - charge[17] = 44
      grid[18] + pv[18] + discharge[18] + gen[18] - charge[18] = 42
      grid[19] + pv[19] + discharge[19] + gen[19] - charge[19] = 40
      grid[20] + pv[20] + discharge[20] + gen[20] - charge[20] = 38
      grid[21] + pv[21] + discharge[21] + gen[21] - charge[21] = 36
      grid[22] + pv[22] + discharge[22] + gen[22] - charge[22] = 34
      grid[23] + pv[23] + discharge[23] + gen[23] - charge[23] = 31

      # Battery state dynamics and end-of-day return
      soc[1] - soc[0] - charge[0] + discharge[0] = 0
      soc[2] - soc[1] - charge[1] + discharge[1] = 0
      soc[3] - soc[2] - charge[2] + discharge[2] = 0
      soc[4] - soc[3] - charge[3] + discharge[3] = 0
      soc[5] - soc[4] - charge[4] + discharge[4] = 0
      soc[6] - soc[5] - charge[5] + discharge[5] = 0
      soc[7] - soc[6] - charge[6] + discharge[6] = 0
      soc[8] - soc[7] - charge[7] + discharge[7] = 0
      soc[9] - soc[8] - charge[8] + discharge[8] = 0
      soc[10] - soc[9] - charge[9] + discharge[9] = 0
      soc[11] - soc[10] - charge[10] + discharge[10] = 0
      soc[12] - soc[11] - charge[11] + discharge[11] = 0
      soc[13] - soc[12] - charge[12] + discharge[12] = 0
      soc[14] - soc[13] - charge[13] + discharge[13] = 0
      soc[15] - soc[14] - charge[14] + discharge[14] = 0
      soc[16] - soc[15] - charge[15] + discharge[15] = 0
      soc[17] - soc[16] - charge[16] + discharge[16] = 0
      soc[18] - soc[17] - charge[17] + discharge[17] = 0
      soc[19] - soc[18] - charge[18] + discharge[18] = 0
      soc[20] - soc[19] - charge[19] + discharge[19] = 0
      soc[21] - soc[20] - charge[20] + discharge[20] = 0
      soc[22] - soc[21] - charge[21] + discharge[21] = 0
      soc[23] - soc[22] - charge[22] + discharge[22] = 0
      soc[24] - soc[23] - charge[23] + discharge[23] = 0
      soc[0] = 40
      soc[24] = 40

      # Device limits
      grid[0] >= 0
      pv[0] >= 0
      pv[0] <= 0
      charge[0] >= 0
      charge[0] <= 20
      discharge[0] >= 0
      discharge[0] <= 20
      gen[0] >= 0
      gen[0] <= 40
      grid[1] >= 0
      pv[1] >= 0
      pv[1] <= 0
      charge[1] >= 0
      charge[1] <= 20
      discharge[1] >= 0
      discharge[1] <= 20
      gen[1] >= 0
      gen[1] <= 40
      grid[2] >= 0
      pv[2] >= 0
      pv[2] <= 0
      charge[2] >= 0
      charge[2] <= 20
      discharge[2] >= 0
      discharge[2] <= 20
      gen[2] >= 0
      gen[2] <= 40
      grid[3] >= 0
      pv[3] >= 0
      pv[3] <= 0
      charge[3] >= 0
      charge[3] <= 20
      discharge[3] >= 0
      discharge[3] <= 20
      gen[3] >= 0
      gen[3] <= 40
      grid[4] >= 0
      pv[4] >= 0
      pv[4] <= 0
      charge[4] >= 0
      charge[4] <= 20
      discharge[4] >= 0
      discharge[4] <= 20
      gen[4] >= 0
      gen[4] <= 40
      grid[5] >= 0
      pv[5] >= 0
      pv[5] <= 2
      charge[5] >= 0
      charge[5] <= 20
      discharge[5] >= 0
      discharge[5] <= 20
      gen[5] >= 0
      gen[5] <= 40
      grid[6] >= 0
      pv[6] >= 0
      pv[6] <= 8
      charge[6] >= 0
      charge[6] <= 20
      discharge[6] >= 0
      discharge[6] <= 20
      gen[6] >= 0
      gen[6] <= 40
      grid[7] >= 0
      pv[7] >= 0
      pv[7] <= 16
      charge[7] >= 0
      charge[7] <= 20
      discharge[7] >= 0
      discharge[7] <= 20
      gen[7] >= 0
      gen[7] <= 40
      grid[8] >= 0
      pv[8] >= 0
      pv[8] <= 24
      charge[8] >= 0
      charge[8] <= 20
      discharge[8] >= 0
      discharge[8] <= 20
      gen[8] >= 0
      gen[8] <= 40
      grid[9] >= 0
      pv[9] >= 0
      pv[9] <= 32
      charge[9] >= 0
      charge[9] <= 20
      discharge[9] >= 0
      discharge[9] <= 20
      gen[9] >= 0
      gen[9] <= 40
      grid[10] >= 0
      pv[10] >= 0
      pv[10] <= 36
      charge[10] >= 0
      charge[10] <= 20
      discharge[10] >= 0
      discharge[10] <= 20
      gen[10] >= 0
      gen[10] <= 40
      grid[11] >= 0
      pv[11] >= 0
      pv[11] <= 38
      charge[11] >= 0
      charge[11] <= 20
      discharge[11] >= 0
      discharge[11] <= 20
      gen[11] >= 0
      gen[11] <= 40
      grid[12] >= 0
      pv[12] >= 0
      pv[12] <= 40
      charge[12] >= 0
      charge[12] <= 20
      discharge[12] >= 0
      discharge[12] <= 20
      gen[12] >= 0
      gen[12] <= 40
      grid[13] >= 0
      pv[13] >= 0
      pv[13] <= 36
      charge[13] >= 0
      charge[13] <= 20
      discharge[13] >= 0
      discharge[13] <= 20
      gen[13] >= 0
      gen[13] <= 40
      grid[14] >= 0
      pv[14] >= 0
      pv[14] <= 30
      charge[14] >= 0
      charge[14] <= 20
      discharge[14] >= 0
      discharge[14] <= 20
      gen[14] >= 0
      gen[14] <= 40
      grid[15] >= 0
      pv[15] >= 0
      pv[15] <= 22
      charge[15] >= 0
      charge[15] <= 20
      discharge[15] >= 0
      discharge[15] <= 20
      gen[15] >= 0
      gen[15] <= 40
      grid[16] >= 0
      pv[16] >= 0
      pv[16] <= 12
      charge[16] >= 0
      charge[16] <= 20
      discharge[16] >= 0
      discharge[16] <= 20
      gen[16] >= 0
      gen[16] <= 40
      grid[17] >= 0
      pv[17] >= 0
      pv[17] <= 4
      charge[17] >= 0
      charge[17] <= 20
      discharge[17] >= 0
      discharge[17] <= 20
      gen[17] >= 0
      gen[17] <= 40
      grid[18] >= 0
      pv[18] >= 0
      pv[18] <= 0
      charge[18] >= 0
      charge[18] <= 20
      discharge[18] >= 0
      discharge[18] <= 20
      gen[18] >= 0
      gen[18] <= 40
      grid[19] >= 0
      pv[19] >= 0
      pv[19] <= 0
      charge[19] >= 0
      charge[19] <= 20
      discharge[19] >= 0
      discharge[19] <= 20
      gen[19] >= 0
      gen[19] <= 40
      grid[20] >= 0
      pv[20] >= 0
      pv[20] <= 0
      charge[20] >= 0
      charge[20] <= 20
      discharge[20] >= 0
      discharge[20] <= 20
      gen[20] >= 0
      gen[20] <= 40
      grid[21] >= 0
      pv[21] >= 0
      pv[21] <= 0
      charge[21] >= 0
      charge[21] <= 20
      discharge[21] >= 0
      discharge[21] <= 20
      gen[21] >= 0
      gen[21] <= 40
      grid[22] >= 0
      pv[22] >= 0
      pv[22] <= 0
      charge[22] >= 0
      charge[22] <= 20
      discharge[22] >= 0
      discharge[22] <= 20
      gen[22] >= 0
      gen[22] <= 40
      grid[23] >= 0
      pv[23] >= 0
      pv[23] <= 0
      charge[23] >= 0
      charge[23] <= 20
      discharge[23] >= 0
      discharge[23] <= 20
      gen[23] >= 0
      gen[23] <= 40
      soc[0] >= 0
      soc[0] <= 80
      soc[1] >= 0
      soc[1] <= 80
      soc[2] >= 0
      soc[2] <= 80
      soc[3] >= 0
      soc[3] <= 80
      soc[4] >= 0
      soc[4] <= 80
      soc[5] >= 0
      soc[5] <= 80
      soc[6] >= 0
      soc[6] <= 80
      soc[7] >= 0
      soc[7] <= 80
      soc[8] >= 0
      soc[8] <= 80
      soc[9] >= 0
      soc[9] <= 80
      soc[10] >= 0
      soc[10] <= 80
      soc[11] >= 0
      soc[11] <= 80
      soc[12] >= 0
      soc[12] <= 80
      soc[13] >= 0
      soc[13] <= 80
      soc[14] >= 0
      soc[14] <= 80
      soc[15] >= 0
      soc[15] <= 80
      soc[16] >= 0
      soc[16] <= 80
      soc[17] >= 0
      soc[17] <= 80
      soc[18] >= 0
      soc[18] <= 80
      soc[19] >= 0
      soc[19] <= 80
      soc[20] >= 0
      soc[20] <= 80
      soc[21] >= 0
      soc[21] <= 80
      soc[22] >= 0
      soc[22] <= 80
      soc[23] >= 0
      soc[23] <= 80
      soc[24] >= 0
      soc[24] <= 80
    ((column_names
      (grid[0] grid[1] grid[2] grid[3] grid[4] grid[5] grid[6] grid[7] grid[8]
       grid[9] grid[10] grid[11] grid[12] grid[13] grid[14] grid[15] grid[16]
       grid[17] grid[18] grid[19] grid[20] grid[21] grid[22] grid[23] pv[0] pv[1]
       pv[2] pv[3] pv[4] pv[5] pv[6] pv[7] pv[8] pv[9] pv[10] pv[11] pv[12]
       pv[13] pv[14] pv[15] pv[16] pv[17] pv[18] pv[19] pv[20] pv[21] pv[22]
       pv[23] charge[0] charge[1] charge[2] charge[3] charge[4] charge[5]
       charge[6] charge[7] charge[8] charge[9] charge[10] charge[11] charge[12]
       charge[13] charge[14] charge[15] charge[16] charge[17] charge[18]
       charge[19] charge[20] charge[21] charge[22] charge[23] discharge[0]
       discharge[1] discharge[2] discharge[3] discharge[4] discharge[5]
       discharge[6] discharge[7] discharge[8] discharge[9] discharge[10]
       discharge[11] discharge[12] discharge[13] discharge[14] discharge[15]
       discharge[16] discharge[17] discharge[18] discharge[19] discharge[20]
       discharge[21] discharge[22] discharge[23] soc[0] soc[1] soc[2] soc[3]
       soc[4] soc[5] soc[6] soc[7] soc[8] soc[9] soc[10] soc[11] soc[12] soc[13]
       soc[14] soc[15] soc[16] soc[17] soc[18] soc[19] soc[20] soc[21] soc[22]
       soc[23] soc[24] gen[0] gen[1] gen[2] gen[3] gen[4] gen[5] gen[6] gen[7]
       gen[8] gen[9] gen[10] gen[11] gen[12] gen[13] gen[14] gen[15] gen[16]
       gen[17] gen[18] gen[19] gen[20] gen[21] gen[22] gen[23]))
     (c
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5))
     (p 50) (m 266) (l 266) (q ())
     (a
      (((pr
         (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
          1 1 1 1 1 1 1 1 1 1 1 1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
          -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
          -1 -1 -1 -1 -1 -1 -1 -1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
          1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 -1 1 1 -1 1 -1 1 -1 1
          -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1
          -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
          1 1 1 1))
        (jc
         (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
          27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 50 52
          54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90 92 94 96 98
          100 102 104 106 108 110 112 114 116 118 120 122 124 126 128 130 132 134
          136 138 140 142 144 146 148 150 152 154 156 158 160 162 164 166 168 170
          172 174 176 178 180 182 184 186 188 190 192 194 195 196 197 198 199 200
          201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217
          218))
        (ir
         (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 0 1 2 3 4
          5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 0 24 1 25 2 26 3 27
          4 28 5 29 6 30 7 31 8 32 9 33 10 34 11 35 12 36 13 37 14 38 15 39 16 40
          17 41 18 42 19 43 20 44 21 45 22 46 23 47 0 24 1 25 2 26 3 27 4 28 5 29
          6 30 7 31 8 32 9 33 10 34 11 35 12 36 13 37 14 38 15 39 16 40 17 41 18
          42 19 43 20 44 21 45 22 46 23 47 24 48 24 25 25 26 26 27 27 28 28 29 29
          30 30 31 31 32 32 33 33 34 34 35 35 36 36 37 37 38 38 39 39 40 40 41 41
          42 42 43 43 44 44 45 45 46 46 47 47 49 0 1 2 3 4 5 6 7 8 9 10 11 12 13
          14 15 16 17 18 19 20 21 22 23)))))
     (b
      ((28 26 24 23 22 24 30 38 44 48 52 55 57 56 54 50 46 44 42 40 38 36 34 31 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 40 40)))
     (g
      ((pr
        (-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
         -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1
         1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1
         -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1
         1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1
         -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1
         1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1
         -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1
         1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1 -1 1
         -1 1 -1 1 -1 1 -1 1 -1 1))
       (jc
        (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 26 28
         30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76
         78 80 82 84 86 88 90 92 94 96 98 100 102 104 106 108 110 112 114 116 118
         120 122 124 126 128 130 132 134 136 138 140 142 144 146 148 150 152 154
         156 158 160 162 164 166 168 170 172 174 176 178 180 182 184 186 188 190
         192 194 196 198 200 202 204 206 208 210 212 214 216 218 220 222 224 226
         228 230 232 234 236 238 240 242 244 246 248 250 252 254 256 258 260 262
         264 266))
       (ir
        (0 9 18 27 36 45 54 63 72 81 90 99 108 117 126 135 144 153 162 171 180
         189 198 207 1 2 10 11 19 20 28 29 37 38 46 47 55 56 64 65 73 74 82 83 91
         92 100 101 109 110 118 119 127 128 136 137 145 146 154 155 163 164 172
         173 181 182 190 191 199 200 208 209 3 4 12 13 21 22 30 31 39 40 48 49 57
         58 66 67 75 76 84 85 93 94 102 103 111 112 120 121 129 130 138 139 147
         148 156 157 165 166 174 175 183 184 192 193 201 202 210 211 5 6 14 15 23
         24 32 33 41 42 50 51 59 60 68 69 77 78 86 87 95 96 104 105 113 114 122
         123 131 132 140 141 149 150 158 159 167 168 176 177 185 186 194 195 203
         204 212 213 216 217 218 219 220 221 222 223 224 225 226 227 228 229 230
         231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248
         249 250 251 252 253 254 255 256 257 258 259 260 261 262 263 264 265 7 8
         16 17 25 26 34 35 43 44 52 53 61 62 70 71 79 80 88 89 97 98 106 107 115
         116 124 125 133 134 142 143 151 152 160 161 169 170 178 179 187 188 196
         197 205 206 214 215))))
     (h
      (0 0 0 0 20 0 20 0 40 0 0 0 0 20 0 20 0 40 0 0 0 0 20 0 20 0 40 0 0 0 0 20
       0 20 0 40 0 0 0 0 20 0 20 0 40 0 0 2 0 20 0 20 0 40 0 0 8 0 20 0 20 0 40 0
       0 16 0 20 0 20 0 40 0 0 24 0 20 0 20 0 40 0 0 32 0 20 0 20 0 40 0 0 36 0
       20 0 20 0 40 0 0 38 0 20 0 20 0 40 0 0 40 0 20 0 20 0 40 0 0 36 0 20 0 20
       0 40 0 0 30 0 20 0 20 0 40 0 0 22 0 20 0 20 0 40 0 0 12 0 20 0 20 0 40 0 0
       4 0 20 0 20 0 40 0 0 0 0 20 0 20 0 40 0 0 0 0 20 0 20 0 40 0 0 0 0 20 0 20
       0 40 0 0 0 0 20 0 20 0 40 0 0 0 0 20 0 20 0 40 0 0 0 0 20 0 20 0 40 0 80 0
       80 0 80 0 80 0 80 0 80 0 80 0 80 0 80 0 80 0 80 0 80 0 80 0 80 0 80 0 80 0
       80 0 80 0 80 0 80 0 80 0 80 0 80 0 80 0 80))
     (bool_vars_idx ()) (int_vars_idx ()) (objective_offset 0))
    |}]
