open Fp_lab_3
open Common

let float_eps = 1e-9

let check_float_list name expected actual =
  Alcotest.(check (list (float float_eps))) name expected actual

let check_pair_list name expected actual =
  let pair_testable =
    Alcotest.pair (Alcotest.float float_eps) (Alcotest.float float_eps)
  in
  Alcotest.check (Alcotest.list pair_testable) name expected actual

(* unit tests *)
let test_gen_xs_basic () =
  let xs = Engine.gen_xs ~step:0.5 ~from_x:0.0 ~to_x:1.2 in
  check_float_list "gen_xs 0..1.2 step 0.5" [ 0.0; 0.5; 1.0 ] xs

let test_linear_eval () =
  let a = { x = 0.0; y = 0.0 } in
  let b = { x = 2.0; y = 4.0 } in
  let y = Linear.eval a b 0.5 in
  Alcotest.(check (float float_eps)) "linear eval" 1.0 y

let test_linear_stream () =
  let e0 = Linear.create ~step:1.0 in
  let e1, outs1 = Engine.push e0 { x = 0.0; y = 0.0 } in
  Alcotest.(check int) "no output after first point" 0 (List.length outs1);
  let _e2, outs2 = Engine.push e1 { x = 2.0; y = 2.0 } in
  let pairs = List.map (fun o -> (o.x, o.y)) outs2 in
  check_pair_list "linear outputs 0..2 step 1" [ (0.0, 0.0); (1.0, 1.0); (2.0, 2.0) ] pairs

let test_newton_eval () =
  let pts = [| { x = 0.0; y = 0.0 }; { x = 1.0; y = 1.0 }; { x = 2.0; y = 4.0 } |] in
  let coeffs = Newton.divided_differences_coeffs pts in
  let y = Newton.eval_newton pts coeffs 1.5 in
  Alcotest.(check (float float_eps)) "newton eval" 2.25 y

let test_newton_stream () =
  let e0 = Newton.create ~step:1.0 ~n:3 in
  let e1, outs1 = Engine.push e0 { x = 0.0; y = 0.0 } in
  let e2, outs2 = Engine.push e1 { x = 1.0; y = 1.0 } in
  Alcotest.(check int) "no output after first point" 0 (List.length outs1);
  Alcotest.(check int) "no output after second point" 0 (List.length outs2);
  let _e3, outs3 = Engine.push e2 { x = 2.0; y = 4.0 } in
  let pairs = List.map (fun o -> (o.x, o.y)) outs3 in
  check_pair_list "newton outputs 0..2 step 1" [ (0.0, 0.0); (1.0, 1.0); (2.0, 4.0) ] pairs

let test_newton_invalid_n () =
  Alcotest.check_raises
    "newton n < 3"
    (Invalid_argument "--newton -n must be >= 3")
    (fun () -> ignore (Newton.create ~step:1.0 ~n:2))

let unit_tests =
  let open Alcotest in
  [ test_case "gen_xs basic" `Quick test_gen_xs_basic
  ; test_case "linear eval" `Quick test_linear_eval
  ; test_case "linear stream" `Quick test_linear_stream
  ; test_case "newton eval" `Quick test_newton_eval
  ; test_case "newton stream" `Quick test_newton_stream
  ; test_case "newton invalid n" `Quick test_newton_invalid_n
  ]

(* property tests *)
let qcheck_tests =
  let open QCheck in
  let gen_small_float = Gen.float_range (-1000.0) 1000.0 in
  let arb_point =
    make ~print:(fun (x, y) -> Printf.sprintf "(%g,%g)" x y) Gen.(pair gen_small_float gen_small_float)
  in
  let prop_linear_eval_at_a =
    Test.make
      ~name:"linear eval at a.x equals a.y"
      (pair arb_point arb_point)
      (fun ((x0, y0), (x1, y1)) ->
        let a = { x = x0; y = y0 } in
        let b = { x = x1; y = y1 } in
        abs_float (Linear.eval a b x0 -. y0) < 1e-7)
  in
  let prop_linear_eval_at_b =
    Test.make
      ~name:"linear eval at b.x equals b.y when x distinct"
      (pair arb_point arb_point)
      (fun ((x0, y0), (x1, y1)) ->
        if abs_float (x1 -. x0) < 1e-6 then true
        else
          let a = { x = x0; y = y0 } in
          let b = { x = x1; y = y1 } in
          abs_float (Linear.eval a b x1 -. y1) < 1e-7)
  in
  [ prop_linear_eval_at_a; prop_linear_eval_at_b ]

let () =
  let open Alcotest in
  run
    "fp-lab-3 – interpolation"
    [ "unit", unit_tests
    ; "properties", List.map QCheck_alcotest.to_alcotest qcheck_tests
    ]
