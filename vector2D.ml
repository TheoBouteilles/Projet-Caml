(* contains functions on vector2D *)

type vector2D = float*float;;


let add: vector2D -> vector2D -> vector2D = fun v1 v2 ->
  let (vx, vy) = v1 in
  let (vx', vy') = v2 in
  (vx +. vx'), (vy +. vy')


let mul: vector2D -> float -> vector2D = fun v f ->
  let (vx, vy) = v in
  (vx *. f), (vy *. f)


let sub: vector2D -> vector2D -> vector2D = fun v v' ->
  add v (mul v' (-1.))


let isZero: vector2D -> bool = fun v ->
  let (vx, vy) = v in
  (vx < 1e-6) && (vy < 1e-6)


let prod_scal: vector2D -> vector2D -> float = fun v v' ->
  let (vx, vy) = v in
  let (vx', vy') = v' in
  vx *. vx' +. vy *. vy'


let vect_angle: vector2D -> float = fun vect->
  (* calcul l angle du vecteur en radian*)
  let (vx, vy) = vect in
  atan2 vy vx


let dist: vector2D -> vector2D -> float = fun p1 p2 ->
  let (x1,y1)=p1 in
  let (x2,y2)=p2 in
  sqrt ( (x2-.x1)**2. +. (y2-.y1)**2.);;

let norm: vector2D -> float = fun v ->
  let (vx, vy)=v in
  sqrt (vx ** 2. +. vy **2.);;

let normalized: vector2D -> vector2D = fun v ->
  let norme = norm v in
  let vx, vy = v in
  (vx /. norme), (vy /. norme);;

let setNorm: vector2D -> float -> vector2D = fun v norme->
  mul (normalized v) norme;;


