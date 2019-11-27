type aircraft = {id : int; mutable pos : float*float; mutable speed_vector: float*float; mutable neighbors: aircraft list; route: (float*float) list};;

let creer_avion id position vitesse =
	{id = id; pos = position; speed_vector = vitesse ; neighbors = []};;

let add_pos a (p1,p2) =
	let (x, y) = a.pos in
	a.pos <- (x +. p1, y +. p2);;

let modif_vit a speed_vect =
  a.speed_vector <- speed_vect;;

  

