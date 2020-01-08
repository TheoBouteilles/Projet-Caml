(* contains trigonometrics function *)

let pi = 4. *. atan 1.

let degrees = fun rad ->
  rad *. 180. /. pi

let radians = fun deg ->
  deg *. pi /. 180.

let normalize = fun deg ->
  let n = floor (deg /. 360.) in
  deg -. 360. *. n

type 'a intervalle = Empty | Intervalle of 'a*'a


let intersection = fun intervalle1 intervalle2 ->
  match intervalle1, intervalle2 with
  | _,Empty -> Empty
  | Empty,_ -> Empty
  | Intervalle(i,j), Intervalle(i',j') -> if max i i' <= min j j' then
      Intervalle (max i i', min j j')
    else Empty
