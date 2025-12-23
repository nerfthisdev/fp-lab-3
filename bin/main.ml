open Fp_lab_3

type method_kind =
  | Linear
  | Newton

type config = { step : float; methods : method_kind list; newton_n : int }

let default_config =
  { step = 1.0; methods = []; newton_n = 4 }

let () =
  let cfg = ref default_config in

  let add_method m cfg =
    if List.mem m cfg.methods then cfg
    else { cfg with methods = cfg.methods @ [ m ] }
  in

  let set_step s = cfg := { !cfg with step = s } in
  let set_linear () = cfg := add_method Linear !cfg in
  let set_newton () = cfg := add_method Newton !cfg in
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

  if (!cfg).methods = [] then (
    prerr_endline "Error: choose at least one algorithm: --linear and/or --newton";
    exit 2);

  (* Build engines list *)
  let engines =
    List.map
      (function
        | Linear -> Linear.create ~step:(!cfg).step
        | Newton -> Newton.create ~step:(!cfg).step ~n:(!cfg).newton_n)
      (!cfg).methods
  in

  (* Streaming loop:
     For each incoming point, push it into every engine and print what they emit *)
  let points = Io.read_points_seq () in

  let engines = ref engines in
  Seq.iter
    (fun p ->
      let engs', outs_rev =
        List.fold_left
          (fun (rev_es, rev_outs) e ->
            let e', out = Engine.push e p in
            (e' :: rev_es, List.rev_append out rev_outs))
          ([], []) !engines
      in
      engines := List.rev engs';
      Io.print_many (List.rev outs_rev))
    points;

  List.iter (fun e -> Io.print_many (Engine.flush e)) !engines
