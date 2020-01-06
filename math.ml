(* contains trigonometrics function *)

let pi = 4. *. atan 1.

let degrees = fun rad ->
  rad *. 180. /. pi

let radians = fun deg ->
  deg *. pi /. 180.
