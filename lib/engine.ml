open Common

type 's engine = {
  name : string;
  state : 's;
  push : 's -> point -> 's * out_point list;
  flush : 's -> out_point list;
}

type t = E : 's engine -> t

let name (E e) = e.name

let push (E e) p =
  let state', outs = e.push e.state p in
  (E { e with state = state' }, outs)

let flush (E e) = e.flush e.state

(*for x from a to b step s*)
let rec gen_xs ~step ~from_x ~to_x : float list =
  if step <= 0.0 then invalid_arg "--step must be > 0";
  if from_x > to_x +. eps then []
  else from_x :: gen_xs ~step ~from_x:(from_x +. step) ~to_x
