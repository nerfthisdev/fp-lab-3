open Types

let normalize_seps (s : string) =
  let b = Bytes.of_string s in
  for i = 0 to Bytes.length b - 1 do
    match Bytes.get b i with
    | ';' | '\t' -> Bytes.set b i ' '
    | _ -> ()
  done;
  Bytes.to_string b

let split_ws (s : string) : string list =
  s
  |> String.split_on_char ' '
  |> List.filter (fun t -> t <> "")

let parse_point_line (line: string) : point option =
  let line = String.trim line in
  if line = "" then None
else
let line = normalize_seps line in
match split_ws line with
| [xs; ys] -> (
  try
  let x = float_of_string xs in
  let y = float_of_string ys in
  Some {x; y}
  with _ -> None)
| _ -> None

let read_points_seq () : point Seq.t =
  let rec next () =
  match input_line stdin with
  | line -> (
    match parse_point_line line with
    | Some p -> Seq.Cons (p, next)
    | None -> next ())
  | exception End_of_file -> Seq.Nil
  in
  next

let print_out (o : out_point): unit =
  Printf.printf "%s: %.10g %.10g\n%!" o.algo o.x o.y


let print_many (xs: out_point list): unit =
  List.iter print_out xs
