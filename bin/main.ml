open Fp_lab_3
open Types

type config = {
  step : float;
  use_linear : bool;
  use_newton : bool;
  newton_n : int;
}

let default_config =
  { step = 1.0; use_linear = false; use_newton = false; newton_n = 4 }

let () =
  let cfg = ref default_config in

  let set_step s = cfg := { !cfg with step = s } in
  let set_linear () = cfg := { !cfg with use_linear = true } in
  let set_newton () = cfg := { !cfg with use_newton = true } in
  let set_newton_n n = cfg := { !cfg with newton_n = n } in

  let specs =
    [
      ("--step", Arg.Float set_step, "Sampling step (e.g. 0.5)");
      ("--linear", Arg.Unit set_linear, "Enable linear interpolation");
      ("--newton", Arg.Unit set_newton, "Enable Newton interpolation");
      ("-n", Arg.Int set_newton_n, "Newton window size (>=3)");
    ]
  in

  let usage = "my_lab3 [--linear] [--newton -n N] --step S < input" in
  Arg.parse specs (fun _ -> ()) usage;

  if (not (!cfg).use_linear) && (not (!cfg).use_newton) then (
    prerr_endline "Error: choose at least one algorithm: --linear and/or --newton";
    exit 2);

  (* Build engines list *)
  let engines =
    let acc = ref [] in
    if (!cfg).use_linear then acc := Linear.create ~step:(!cfg).step :: !acc;
    if (!cfg).use_newton then
      acc := Newton.create ~step:(!cfg).step ~n:(!cfg).newton_n :: !acc;
    List.rev !acc
  in

  (* Streaming loop:
     For each incoming point, push it into every engine and print what they emit *)
  let points = Io.read_points_seq () in

  let rec consume (engs : Engine.t list) (seq : point Seq.t) =
    match seq () with
    | Seq.Nil ->
        (* EOF: flush all engines *)
        List.iter (fun e -> Io.print_many (e.Engine.flush ())) engs
    | Seq.Cons (p, tl) ->
        let engs', outs =
          List.fold_left
            (fun (es, all_outs) e ->
              let e', out = e.Engine.push p in
              (es @ [ e' ], all_outs @ out))
            ([], []) engs
        in
        Io.print_many outs;
        consume engs' tl
  in

  consume engines points
