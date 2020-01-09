type aircraft = {
  id : int;
  mutable pos : Vector2D.vector2D;
  mutable heading : float;
  mutable speed : float;
  destination : Vector2D.vector2D;
}

type environment = aircraft list

val create :
  int -> Vector2D.vector2D -> float -> float -> Vector2D.vector2D -> aircraft
(* create id (x,y) h s (gx,gy) : create an aircraft item {id=id; pos=(x,y); heading=h; speed=s; destination=(gx,gy)} *)
val setSpeed : aircraft -> float -> unit
(* setSpeed a s : set the speed value of the aircraft item a to s *)
val setHeading : float -> aircraft -> unit
(* setHeading a h : set the heading of the aircraft item a to h *)
val getHeadingVector : aircraft -> float * float
(* getHeadingVector a : return the heading vector of the aircraft a *)
val getSpeedVector : aircraft -> Vector2D.vector2D
(* getSpeedVector a : return the speedvector of the aircraft a *)
val getDistanceAircrafts : aircraft -> aircraft -> float
(* getDistanceAircrafts a a' : return the cartesian distance between the aircraft a and a' *)
val moveAircraft : float -> aircraft -> unit
(* moveAircraft dt a : move the aircraft a along its speedvector on a period dt *)  
val discoverNeighbors : aircraft -> environment -> aircraft list
(* discoverNeighbors a env : return a list of the aircrafts close to the aircraft a in the environment env *)
val getMinimalSeparationDistance :
  aircraft -> aircraft -> float -> float -> float
(* getMinimalSeparationDistance a a' h h' : return an estimation of the  minimal separation distance between 
   aircraft a with heading h and aircraft a' with heading h' in a period of 5 minutes *)
val getHeadings : aircraft -> float array
(* getHeadings a -> return an array of the different headings that aircraft a could take *)
val getConflictMatrix : aircraft -> aircraft -> bool array array
(* getConflictMatrix a a' -> return a matrix of boolean M : Mij = true if there is no conflict estimated
   if aircraft a take the ith heading in its heading list and aircraft a' take the jth heading in its heading list *)
val cmp : aircraft -> aircraft -> int
(* cmp a a' -> return (-1) if aircraft a is ordenanced greater than a', 0 if ordenanced equals, else 1 *) 
val getAvailableHeadings : aircraft -> environment -> int Math.intervalle
(* getAvailableHeadings a env -> return an intervalle of indexes giving the available headings that aircraft a can take in its list of headings in the environment of aircraft env *)
val getNextHeading : aircraft -> environment -> float
(* getNextHeading a env -> return the selected heading for the next iteration of the simulation for the aircraft a in the environment of aircraft env *)
val updateEnv : environment -> float -> unit
(* updateEnv env dt -> move all the aircraft in the environment env along their speed vector on the period dt *)
val arrived : aircraft -> bool
(* arrived a -> return true if aircraft a is arrived at its goal *)
val string_state : aircraft -> string
(* return a string corresponding to the actual state (x,y,h) of the aircraft a *)
