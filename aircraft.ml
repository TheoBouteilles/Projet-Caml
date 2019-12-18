type aircraft = {id : int;
		 mutable pos : float*float;
		 mutable heading: float;
		 mutable speed: float;
		 mutable neighbors: aircraft list;
		 mutable route: (float*float) list;
		 mutable previous_point: (float*float)};;

let creer_avion id position heading speed =
	{id = id; pos = position; heading=heading; speed=speed; neighbors = []; route=[]; previous_point=(0,0)};;

let add_pos a (p1,p2) =
	let (x, y) = a.pos in
	a.pos <- (x +. p1, y +. p2);;

let modif_vit a speed_vect =
  a.speed_vector <- speed_vect;;

let avion1= creer_avion 0 (1.,2.) 10. 15.;;

let avion2= creer_avion 1 (2.,3.) 180. 25.;;


let get_speed_vector = fun aircraft ->
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
  let (vx,vy) = get_speed_vector aircraft in
  aircraft.pos <- (x +. vx*.dt, y +. vy*.dt);;

let change_heading = fun heading aircraft ->
  aircraft.heading <- heading;;

let pos_extended = fun aircraft ->
  let (x,y) = aircraft.pos in
  let (vx,vy)=get_speed_vector aircraft in
  (x +. vx*.10.,y +. vy*.10.);;


exception Empty;;

let right = fun aircraft ->
  let (xa,ya) = aircraft.previous_point in
  match aircraft.route with
    |[] -> raise Empty
    |(xb,yb)::q -> ((xb-.xa),(yb-.ya));;

let right_ortho = fun aircraft ->
  let (a,b) = right aircraft in
  match aircraft.route with
    |[]-> raise Empty
    |(x,y)::q -> (-.a,b,a*.x+b*.y);;


let next_point_heading = fun aircraft ->
  let (x1,y1)=aircraft.pos in
  match aircraft.route with
    |[]-> raise Empty
    |(x,y)::q -> math.rad_to_deg(
      math.vect_angle(
	math.sub_vect (x,y) (x1,y1)
      )
    );;


let find_new_heading = fun aircraft1 aircraft2 matrix ->
  let weight_matrix = array.make_matrix 36 36 -1 in
  for i=0 to 35 do
    for j=0 to 35 do
      if matrix.(i).(j)==true then weight_matrix.(i).(j) <-
	(abs_float(
	  (   (  (next_point_heading aircraft1) -. i) + (next_point_heading aircraft2 -. j)) /2
	 )
	);;



		
	       
		

	       
  
  
