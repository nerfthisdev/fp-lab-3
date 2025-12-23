open Types
open Engine

type state = {
  step : float;
  n : int;
  window : point list;
  cursor : float option;
  last_emittable : float option;
}

let take_last_n (n : int) (xs : 'a list) : 'a list =
  let len = List.length xs in
  if len <= n then xs else List.drop (len - n) xs

let divided_differences_coeffs (pts : point array) : float array =

  let n = Array.length pts in
  let dd = Array.init n (fun i -> pts.(i).y) in
  let coeffs = Array.make n 0.0 in
  coeffs.(0) <- dd.(0);
  for level = 1 to n - 1 do
    for i = 0 to n - level - 1 do
      let x0 = pts.(i).x in
      let x1 = pts.(i + level).x in
      let denom = x1 -. x0 in
      let v =
        if abs_float denom < eps then 0.0
        else (dd.(i + 1) -. dd.(i)) /. denom
      in
      dd.(i) <- v
    done;
    coeffs.(level) <- dd.(0)
  done;
  coeffs

let eval_newton (pts : point array) (coeffs : float array) (x : float) : float =
  let n = Array.length pts in
  let acc = ref coeffs.(0) in
  let prod = ref 1.0 in
  for k = 1 to n - 1 do
    prod := !prod *. (x -. pts.(k - 1).x);
    acc := !acc +. coeffs.(k) *. !prod
  done;
  !acc

let interval_responsibility (pts : point array) : float * float =
  (* "centered" responsibility for streaming windows:
     for n>=4: use [x_{mid-1}, x_{mid}] as main interval
     for n=3:  use [x0, x2] (no strong center)
     for n=2:  degenerates (but we won't use newton n=2 usually)
  *)
  let n = Array.length pts in
  if n <= 2 then (pts.(0).x, pts.(n - 1).x)
  else if n = 3 then (pts.(0).x, pts.(2).x)
  else
    let mid_right = n / 2 in
    let mid_left = mid_right - 1 in
    (pts.(mid_left).x, pts.(mid_right).x)

let outputs_for_window (st : state) : state * out_point list =
  match st.window with
  | [] | [ _ ] -> (st, [])
  | _ ->
      if List.length st.window < st.n then (st, [])
      else
        let pts = Array.of_list st.window in
        let coeffs = divided_differences_coeffs pts in
        let (l, r) = interval_responsibility pts in
        let cursor =
          match st.cursor with
          | None -> pts.(0).x
          | Some c -> c
        in
        (* We only emit inside [cursor, r] if cursor is within current responsibility.
           For the very first full window, cursor starts at x0 and r might be "center",
           so we will naturally emit the first chunk. *)
        let from_x = max cursor l in
        let to_x = r in
        if from_x > to_x +. eps then

          ({ st with last_emittable = Some r }, [])
        else
          let xs = Engine.gen_xs ~step:st.step ~from_x ~to_x in
          let outs =
            xs
            |> List.map (fun x ->
                   { algo = "newton"; x; y = eval_newton pts coeffs x })
          in
          let next_cursor =
            from_x +. (float_of_int (List.length xs)) *. st.step
          in
          let st' =
            { st with cursor = Some next_cursor; last_emittable = Some r }
          in
          (st', outs)

let create ~(step : float) ~(n : int) : Engine.t =
  if n < 3 then invalid_arg "--newton -n must be >= 3";
  let rec make (st : state) : Engine.t =
    {
      name = "newton";
      push =
        (fun p ->
          let w = st.window @ [ p ] in
          let w = if List.length w > n then take_last_n n w else w in
          let st1 = { st with window = w } in
          let st2, outs = outputs_for_window st1 in
          (make st2, outs));
      flush =
        (fun () ->
          (* On EOF: output remaining tail using the last available window:
             we emit from current cursor to the last x in the window. *)
          if List.length st.window < n then []
          else
            let pts = Array.of_list st.window in
            let coeffs = divided_differences_coeffs pts in
            let last_x = pts.(Array.length pts - 1).x in
            let cursor =
              match st.cursor with
              | None -> pts.(0).x
              | Some c -> c
            in
            if cursor > last_x +. eps then []
            else
              let xs = Engine.gen_xs ~step:st.step ~from_x:cursor ~to_x:last_x in
              xs
              |> List.map (fun x ->
                     { algo = "newton"; x; y = eval_newton pts coeffs x }));
    }
  in
  make { step; n; window = []; cursor = None; last_emittable = None }
