type aircraft = {id : int; mutable pos : Math.vector2D; mutable heading: float;mutable speed: float; mutable neighbors: aircraft list; route: Math.vector2D list};;

let creer_avion id position heading speed =
  {id = id; pos = position; heading=heading; speed=speed; neighbors = []; route=[]};;
  
  
let modif_vit a speed =
  a.speed <- speed;;


let avion1= creer_avion 0 (1.,2.) 10. 15.;;

let avion2= creer_avion 1 (2.,3.) 180. 25.;;


let get_speed_vector = fun aircraft ->
  Math.setNorm (Math.degToVect aircraft.heading) aircraft.speed;;


let distance_aircrafts = fun a1 a2 ->
  Math.dist a1.pos a2.pos;;


let rec discover_neighbors = fun distance a1 list_aircrafts ->
  let neighbors =[] in
  match list_aircrafts with
    |[]-> neighbors
    |t::q -> if t.id != a1.id && distance_aircrafts a1 t < distance
      then t::(discover_neighbors distance a1 q)
      else discover_neighbors distance a1 q;;


let move_aircraft = fun dt aircraft ->
  let pos = aircraft.pos in
  let v = get_speed_vector aircraft in
  aircraft.pos <- Math.add pos (Math.mul v dt);;


let change_heading = fun heading aircraft ->
  aircraft.heading <- heading;;

let pos_extended = fun aircraft ->
  let (x,y) = aircraft.pos in
  let (vx,vy)=get_speed_vector aircraft in
  (x +. vx*.10.,y +. vy*.10.);;

let getDistWithHeading = fun a1 a2 h1 h2 ->
  let p1 = a1.pos in
  let p2 = a2.pos in
  let v1 = Math.setNorm (Math.degToVect h1) a1.speed in
  let v2 = Math.setNorm (Math.degToVect h2) a2.speed in
  let p = Math.sub p1 p2 in
  let v = Math.sub v1 v2 in
  if (Math.isZero v) then distance_aircrafts a1 a2
  else let t = (Math.prod_scal p v) /. ((Math.norm v) ** 2.) in
    if t < 0. then distance_aircrafts a1 a2 (* cas où les avions s'éloignent *)
    else if t <= 180. then Math.norm (Math.add p (Math.mul v t)) (* cas où le croisement va se faire dans les 3 min *)
    else Math.norm (Math.add p (Math.mul v 180.)) (* cas où le croisement à lieu plus tard *)
;;

let getDistHeadingMatrix = fun a1 a2 ->
  let matrix = Array.make_matrix 36 36 0. in
  for i=0 to 35 do
    for j=0 to 35 do
      begin
        let h1 = (float i) *. 10. in
        let h2 = (float j) *. 10. in
        matrix.(i).(j) <- getDistWithHeading a1 a2 h1 h2
      end
    done;
  done;
  matrix
;;

exception Empty;;

let getConflictMatrix = fun a1 a2 ->
  let matrix = Array.make_matrix 36 36 true in
  let matrix' = getDistHeadingMatrix a1 a2 in
  for i=0 to 35 do
    for j=0 to 35 do
      matrix.(i).(j) <- matrix'.(i).(j) < 10000. (* meters *)
    done;
  done;
  matrix
;;


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



		
	       
		

	       
  
  
