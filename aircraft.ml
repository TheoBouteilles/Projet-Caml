type aircraft = {id : int; mutable pos : float*float; mutable speed_vector: float*float; mutable neigbors_list: aircraft list; route: (float*float) list};;

let creer_avion position vitesse =
	incr id_ref;
	{id = !id_ref; pos = position; vit = vitesse ; voisins = []};;

let add_pos a p1 p2 =
	let (x, y) = a.pos in
	a.pos <- (x +. p1, y +. p2);;

let modif_vit a v1 v2 =
	a.vit <- (v1, v2);;

