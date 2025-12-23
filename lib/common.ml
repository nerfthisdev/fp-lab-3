type point = {x : float; y : float}
type out_point = {
  algo : string;
  x : float;
  y : float;
}

let eps = 1e-12

type state = {
  step : float;
  prev : point option;
  cursor : float option;
  n : int option;
  window : point list;
  last_emittable : float option;
}
