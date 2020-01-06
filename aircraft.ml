module type HeadingsSpace = sig
  val n : int
  val headings : float array
end

module Make = functor (Space : HeadingsSpace) -> struct
  
  type aircraft = {id : int;
                   mutable pos : Vector2D.vector2D;
                   mutable heading: float;
                   mutable speed: float;
                   mutable neighbors: aircraft list;
                   route: Vector2D.vector2D array;
                   activeWaypoint: int ref}
                  
  
  let createAircraft = fun id position heading speed route ->
    {id = id;
     pos = position;
     heading=heading;
     speed=speed;
     neighbors = [];
     route=route;
     activeWaypoint= ref 0}
    
  
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
          
  
  let extendedPos = fun aircraft ->
    let (x,y) = aircraft.pos in
    let (vx,vy)=getSpeedVector aircraft in
    (x +. vx*.10.,y +. vy*.10.)
    
  
  let getDistWithHeading = fun a1 a2 h1 h2 ->
    let p1 = a1.pos in
    let p2 = a2.pos in
    let v1 = Vector2D.setNorm (getHeadingVector a1) a1.speed in
    let v2 = Vector2D.setNorm (getHeadingVector a2) a2.speed in
    let p = Vector2D.sub p1 p2 in
    let v = Vector2D.sub v1 v2 in
    if (Vector2D.isZero v) then getDistanceAircrafts a1 a2
    else let t = (Vector2D.prod_scal p v) /. ((Vector2D.norm v) ** 2.) in
      if t < 0. then
        (* cas où les avions s'éloignent *)
        getDistanceAircrafts a1 a2 
      else if t <= 180. then
        (* cas où le croisement va se faire dans les 3 min *)
        Vector2D.norm (Vector2D.add p (Vector2D.mul v t))
      else
        (* cas où le croisement à lieu plus tard *)
        Vector2D.norm (Vector2D.add p (Vector2D.mul v 180.))  
  
  let getDistHeadingMatrix = fun a1 a2 ->
    let matrix = Array.make_matrix Space.n Space.n 0. in
    for i=0 to Space.n do
      for j=0 to Space.n do
        begin
          let h1 = Space.headings.(i) in
          let h2 = Space.headings.(j) in
          matrix.(i).(j) <- getDistWithHeading a1 a2 h1 h2
        end
      done;
    done;
    matrix
    
  
  let getConflictMatrix = fun a1 a2 ->
    let matrix = Array.make_matrix Space.n Space.n true in
    let matrix' = getDistHeadingMatrix a1 a2 in
    for i=0 to Space.n do
      for j=0 to Space.n do
        matrix.(i).(j) <- matrix'.(i).(j) < 10000. (* meters *)
      done;
    done;
    matrix
end
