type aircraft = {id : int; mutable pos : float*float; mutable speed_vector: float*float; mutable neighbors: aircraft list; route: (float*float) list};;

let creer_avion id position vitesse =
  {id = id; pos = position; speed_vector = vitesse ; neighbors = []};;

let add_pos a (p1,p2) =
	let (x, y) = a.pos in
	a.pos <- (x +. p1, y +. p2);;

let modif_vit a speed_vect =
  a.speed_vector <- speed_vect;;

let getDistHeading = fun a1, a2 ->
  let px1,py1 = a1.pos in
  let px2,py2 = a2.pos in
  let vx1,vy1 = get_speed_vect(a1) in
  let vx2,vy2 = get_speed_vect(a2) in
  let px = px1 -. px2 in
  let py = py1 -. py2 in
  let vx = vx1 -. vx2 in
  let vy = vy1 -. vy2 in
  if (vx < 1e-6) && (vy < 1e-6)
  then sqrt((px)**2. +. (py)**2.)
  else let t = (px*.vx +. py*.vy) /. (vx ** 2. +. vy ** 2.) in
    (px +. vx *. t)**2. +. (py +. vy *. t)**2.
;;

let getDistHeadingMatrix = fun a1, a2 ->
  let matrix = Array.make 36 (Array.make 36 0.) in
  let i = ref 0 in
  
  
