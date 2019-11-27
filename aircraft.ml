type aircraft = {id : int; mutable pos : float*float; mutable heading: float;mutable speed: float; mutable neighbors: aircraft list; route: (float*float) list};;

let creer_avion id position heading speed =
	{id = id; pos = position; heading=heading; speed=speed; neighbors = []; route=[]};;

let add_pos a (p1,p2) =
	let (x, y) = a.pos in
	a.pos <- (x +. p1, y +. p2);;

let modif_vit a speed_vect =
  a.speed_vector <- speed_vect;;

let avion1= creer_avion 0 (1.,2.) 10. 15.;;

let avion2= creer_avion 1 (2.,3.) 180. 25.;;


let heading_to_speed_vector = fun aircraft ->
  let (x0,y0) =aircraft.pos in
  (    aircraft.speed *. (sin (aircraft.heading *.3.14159265359/.180.))/.(tan (aircraft.heading *.3.14159265359/.180.))+.x0     ,
       aircraft.speed *. (tan (aircraft.heading *.3.14159265359/.180.))*.(cos (aircraft.heading *.3.14159265359/.180.))+.y0);;
  

let distance_aircrafts = fun a1 a2 ->
  let (x1,y1)=a1.pos in
  let (x2,y2)=a2.pos in
  sqrt ( (x2-.x1)**2. +. (y2-.y1)**2.);;


let rec discover_neighbors = fun distance a1 list_aircrafts ->
  let neighbors =[] in
  match list_aircrafts with
    |[]-> neighbors
    |t::q -> if t.id != a1.id && distance_aircrafts a1 t < distance
      then t::(discover_neighbors distance a1 q)
      else discover_neighbors distance a1 q;;

let move_aircraft = fun dt aircraft ->
  let (x,y) = aircraft.pos in
  let (vx,vy) = heading_to_speed_vector aircraft in
  aircraft.pos <- (x +. vx*.dt, y +. vy*.dt);;

let change_heading = fun heading aircraft ->
  aircraft.heading <- heading;;

  
