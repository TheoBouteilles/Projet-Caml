type aircraft = {id : int;
                 mutable pos : Vector2D.vector2D;
                 mutable heading: float;
                 mutable speed: float;
                 mutable neighbors: aircraft list;
                 route: Vector2D.vector2D array;
                 mutable activeWaypoint: int}
                

let createAircraft = fun id position heading speed route ->
  {id = id;
   pos = position;
   heading=heading;
   speed=speed;
   neighbors = [];
   route=route;
   activeWaypoint= 0}


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
      

let rec discoverNeighbors = fun distance a1 aircrafts_list ->
  let neighbors =[] in
  match aircrafts_list with
  |[]-> neighbors
  |t::q -> if t.id != a1.id && getDistanceAircrafts a1 t < distance
    then t::(discoverNeighbors distance a1 q)
    else discoverNeighbors distance a1 q


let getMinimalSeparationDistanceForHeading = fun a1 a2 h1 h2 ->
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
    else if t <= 180. then
      (* cas où le croisement va se faire dans les 3 min *)
      Vector2D.norm (Vector2D.add p (Vector2D.mul v t))
    else
      (* cas où le croisement à lieu plus tard : distance min = distance à 3 min*)
      Vector2D.norm (Vector2D.add p (Vector2D.mul v 180.))  


let getMinimalSeparationDistanceMatrix = fun a1 a2 headings ->
  let n = Array.length headings in
  let matrix = Array.make_matrix n n 0. in
  for i=0 to (n-1) do
    for j=0 to (n-1) do
      begin
        let h1 = headings.(i) in
        let h2 = headings.(j) in
        matrix.(i).(j) <- getMinimalSeparationDistanceForHeading a1 a2 h1 h2
      end
    done;
  done;
  matrix
  

let getConflictMatrix = fun a1 a2 headings ->
  let n = Array.length headings in
  let matrix = Array.make_matrix n n true in
  let matrix' = getMinimalSeparationDistanceMatrix a1 a2 headings in
  for i=0 to (n-1) do
    for j=0 to (n-1) do
      matrix.(i).(j) <- matrix'.(i).(j) < 10000. (* meters *)
    done;
  done;
  matrix


let isPrioOn = fun a1 a2 -> a1.id > a2.id


let getAvailableHeadings = fun a1 a2 headings ->
  let matrix = getConflictMatrix a1 a2 headings in
  if isPrioOn a1 a2 then headings
  else
    let f = fun b sum -> if b then 1+sum else sum in
    Math.allmax (fun a -> Array.fold_right f a 0) matrix


let getAvailableHeadings2 = fun a headings ->
  List.fold_right (fun a2 hs-> getAvailableHeadings a a2 hs) a.neighbors headings


module AircraftModel = struct
  type state = Vector2D.vector2D * float
  type user_param = aircraft

  let initial_state = fun (a:user_param) -> (a.pos , a.heading) 
                                            
  let is_goal = fun (a:user_param) u -> (u == a.route.(a.activeWaypoint))
                                        
  let k = fun _ u v ->
    let (p,_),(p',_) = u,v in
    Vector2D.dist p p'

  let h = fun (a:user_param) u ->
    let (p,_) = u in
    Vector2D.dist p a.route.(a.activeWaypoint)
      
  let next = fun (a:user_param) u ->
    if u == initial_state a then
      let hs = getAvailableHeadings2 a headings in
      
      let f = fun h l ->
        let p = Vector2D.add
            a.pos
            (Vector2D.mul
               (cos (Math.radians h), sin (Math.radians h))
               (a.speed*. dt))
        in
        (p,h)::l
      in

      Array.fold_right f hs []
    else
      let (p,h) = u in
      [truc; goal]

  let do_at_insertion _ _ _ = ()
  let do_at_extraction _ _ _ _ = ()
end
