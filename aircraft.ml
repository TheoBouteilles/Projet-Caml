type aircraft = { id : int; mutable pos : float*float ; mutable vit : float*float ; mutable
voisins : aircraft list}
let id_ref = ref 0;;

let creer_avion position vitesse =
	incr id_ref;
	{id = !id_ref; pos = position; vit = vitesse ; voisins = []};;

let a1 = creer_avion (5., 4.) (2., 1.);;
let a2 = creer_avion (7., 4.) (-2., 1.);;

let add_pos a p1 p2 =
	let (x, y) = a.pos in
	a.pos <- (x +. p1, y +. p2);;

let modif_vit a v1 v2 =
	a.vit <- (v1, v2);;
type aircraft = {id : int; mutable pos : float*float; mutable vit : float*float};;
let id_ref = ref 0;;

let creer_avion pos_init v_init =
  incr id_ref;
  {id = !id_ref; pos = pos_init; vit = v_init};;

let a1 = creer_avion (2.,3.) (4.,5.);;
