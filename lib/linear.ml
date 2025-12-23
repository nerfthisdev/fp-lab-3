open Common
open Engine

let eval (a : point) (b: point) (x: float) : float =
  let dx = b.x -. a.x in
  if abs_float dx < eps then a.y
  else a.y +. (b.y -. a.y) *. (x -. a.x) /. dx

let segment_outputs (st : state) (a : point) (b : point) : state * out_point list =
  let start_x =
  match st.cursor with
  | None -> a.x
  | Some cx -> cx
  in
  let xs = Engine.gen_xs ~step: st.step ~from_x: start_x ~to_x: b.x in
  let outs =
  xs
  |> List.map (fun x -> {algo = "linear"; x; y = eval a b x})
  in
  let next_cursor = start_x +. (float_of_int (List.length xs)) *. st.step in
  let st' = {st with cursor = Some next_cursor} in
  (st', outs)

let flush_outputs (st : state) : out_point list =
  match st.prev with
  | None -> []
  | Some p ->
      let needs_last =
        match st.cursor with
        | None -> true
        | Some c -> c <= p.x +. eps
      in
      if needs_last then [ { algo = "linear"; x = p.x; y = p.y } ] else []

let create ~(step : float) : Engine.t =
  let push st p =
    match st.prev with
    | None -> ({ st with prev = Some p; cursor = None }, [])
    | Some a ->
        let st1 = { st with prev = Some p } in
        let st2, outs = segment_outputs st1 a p in
        (st2, outs)
  in
  let st0 =
    {
      step;
      prev = None;
      cursor = None;
      n = None;
      window = [];
      last_emittable = None;
    }
  in
  Engine.E { name = "linear"; state = st0; push; flush = flush_outputs }
