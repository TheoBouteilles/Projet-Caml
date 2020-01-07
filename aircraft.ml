type aircraft = {id : int;
                 mutable pos : Vector2D.vector2D;
                 mutable heading: float;
                 mutable speed: float;
                 destination : Vector2D.vector2D}

let _n = 27

let _angleMax = 40. (* degrees *)

let _separationLimite = 9260. (* meters *)

let _conflictDetectionTime = 300. (* seconds *)

let _detectionRadius = 166680. (* meters = 90NM *)


let createAircraft = fun id position heading speed destination ->
  {id = id;
   pos = position;
   heading=heading;
   speed=speed;
   destination = destination
  }


let setSpeed = fun a speed ->
  a.speed <- speed
    

let setHeading = fun heading aircraft ->
  aircraft.heading <- heading


let getHeadingVector = fun aircraft ->
  let radh = Math.radians aircraft.heading in
  (cos radh, sin radh)
  

let getSpeedVector = fun aircraft ->
  let headingVector = getHeadingVector aircraft in
  Vector2D.setNorm headingVector aircraft.speed
    

let getDistanceAircrafts = fun a1 a2 ->
  Vector2D.dist a1.pos a2.pos
    

let moveAircraft = fun dt aircraft ->
  let pos = aircraft.pos in
  let v = getSpeedVector aircraft in
  aircraft.pos <- Vector2D.add pos (Vector2D.mul v dt)
      

let rec discoverNeighbors = fun a1 env ->
  match env with
  |[]-> []
  |a::q ->
      let dist = getDistanceAircrafts a1 a in
      Printf.printf "A%d - Distance with A%d is %.3f\n" a1.id a.id dist;
      if a.id != a1.id && dist < _detectionRadius then
        a::(discoverNeighbors a1 q)
      else discoverNeighbors a1 q


let getMinimalSeparationDistance = fun a1 a2 h1 h2 -> (* might be bugged *)
  let p1 = a1.pos in
  let p2 = a2.pos in
  let v1 = Vector2D.mul
      (cos (Math.radians h1), sin (Math.radians h1))
      a1.speed in
  let v2 = Vector2D.mul
      (cos (Math.radians h2), sin (Math.radians h2))
      a2.speed in
  let p = Vector2D.sub p1 p2 in
  let v = Vector2D.sub v1 v2 in
  if (Vector2D.isZero v) then
    (* cas où les avions ont des trajectoires parallèles *)
    getDistanceAircrafts a1 a2
  else let t = (Vector2D.prod_scal p v) /. ((Vector2D.norm v) ** 2.) in
    if t < 0. then
      (* cas où les avions s'éloignent *)
      getDistanceAircrafts a1 a2 
    else if t <= _conflictDetectionTime then
      (* cas où le croisement va se faire dans les 5min *)
      Vector2D.norm (Vector2D.add p (Vector2D.mul v t))
    else
      (* cas où le croisement à lieu plus tard : distance min = distance à 5 min*)
      Vector2D.norm (Vector2D.add p (Vector2D.mul v _conflictDetectionTime))  


let getHeadings = fun a ->
  let h = a.heading in
  let eps = 2. *. _angleMax /. (float _n) in
  Array.init _n (fun i -> h -. _angleMax +. (float i) *. eps)


let getConflictMatrix = fun a1 a2 ->
  let hs1 = getHeadings a1 in
  let hs2 = getHeadings a2 in
  let matrix = Array.make_matrix _n _n true in
  for i=0 to (_n-1) do
    for j=0 to (_n-1) do
      begin
        let h1 = hs1.(i) in
        let h2 = hs2.(j) in
        matrix.(i).(j) <- (getMinimalSeparationDistance a1 a2 h1 h2) >= _separationLimite
      end
    done;
  done;
  Printf.printf "A%d - Conflict Matrice with A%d :\n  " a1.id a2.id;
  Array.iter (fun a ->( Array.iter (fun b ->Printf.printf "%b " b) a );  print_newline ()) matrix;
  matrix


let cmp = fun a1 a2 ->
  compare a1.pos a2.pos


exception NoSolution

let getAvailableHeadings = fun a env->
  let getAvailableIndexes = fun a1 a2 ->
      let matrix = getConflictMatrix a1 a2 in
      let list = Rectangle.listRectangles _n _n matrix in
      let mbr = Rectangle.getMostBalancedRectangle list in
      match mbr with
      |None ->
          begin
            (*logs*)
            let (x,y),h = a1.pos, a1.heading in
            let (x',y'),h' = a2.pos, a2.heading in
            Printf.printf "A%d - No Solution found at (%.3f,%.3f, h=%.3f) in conflict with A%d at (%.3f,%.3f, h=%.3f) : distance between aircraft = %.3f \n"
              a1.id x y h a2.id x' y' h' (Vector2D.dist a1.pos a2.pos);
            (*/logs*)
            raise NoSolution;
        end
      |Some (j,i,width,height) ->
          Math.Intervalle(i, i+height), Math.Intervalle(j, j+width)
  in

  let neighbors = discoverNeighbors a env in

  let intervalles = (List.map (fun a' -> if cmp a a' < 0 then
                                let _,is = getAvailableIndexes a' a in is
                              else let is,_ = getAvailableIndexes a a' in is)
                       neighbors)
  in
  (*logs*)
  Printf.printf "A%d - Neighbors = " a.id;
  List.iter (fun a -> print_int a.id; print_char ' ') neighbors;
  print_newline ();
  List.iter (fun (Math.Intervalle (i,j)) -> Printf.printf "A%d - i = %d j=%d \n" a.id i j)
    intervalles;  (* raises a warning but missing case never happen *)
  (* \log *)
  List.fold_right Math.intersection intervalles (Math.Intervalle(0, _n))


let getNextHeading = fun a env ->
  match getAvailableHeadings a env with
  | Math.Empty ->
      begin
        (*logs*)
        let (x,y),h = a.pos, a.heading in
        Printf.printf "A%d - No solution found at (%.3f, %.3f, h=%.3f)\n"
          a.id x y h;
        (*/logs*)
        raise NoSolution;
    end
  | Math.Intervalle(i,j) ->
    let eps = 2. *. _angleMax /. (float _n) in
    let h = a.heading in
    let h1 = Math.normalize (h -. _angleMax +. (float i) *. eps) in
    let h2 = Math.normalize (h -. _angleMax +. (float j) *. eps) in
    (* logs *)
    Printf.printf "A%d - Can take heading between %.3f - %.3f \n"
      a.id h1 h2;
    (* /logs *)
    let goalh = Math.degrees (Vector2D.vect_angle (Vector2D.sub a.destination a.pos)) in
    if Math.normalize ( goalh -. h1) <= Math.normalize (h2 -. h1) then goalh
    else if Math.normalize(goalh -. h1) <= Math.normalize (goalh -. h2) then h1
    else h2
    

let rec updateEnv env dt table =
  let updateAircraft a env dt table =
    setHeading (getNextHeading a env) a;
    let (x,y),h = a.pos, a.heading in
    Printf.printf "A%d - Position (%.3f, %.3f, h=%.3f)\n" a.id x y h;
    let p=Printf.sprintf "%.3f,%.3f,%.3f" x y h in
    table.(a.id) <- p::table.(a.id);
    moveAircraft dt a;
  in
  List.iter (fun a -> updateAircraft a env dt table) env

let arrived a dt =
  (Vector2D.dist a.pos a.destination) < (a.speed *. dt)
