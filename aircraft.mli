type aircraft = {
  id : int;
  mutable pos : Math.vector2D;
  mutable heading : float;
  mutable speed : float;
  mutable neighbors : aircraft list;
  route : Math.vector2D list;
}
val creer_avion : int -> Math.vector2D -> float -> float -> aircraft
val modif_vit : aircraft -> float -> unit
val avion1 : aircraft
val avion2 : aircraft
val get_speed_vector : aircraft -> Math.vector2D
val distance_aircrafts : aircraft -> aircraft -> float
val discover_neighbors : float -> aircraft -> aircraft list -> aircraft list
val move_aircraft : float -> aircraft -> unit
val change_heading : float -> aircraft -> unit
val getDistWithHeading : aircraft -> aircraft -> float -> float -> float
val getDistHeadingMatrix : aircraft -> aircraft -> float array array
val getConflictMatrix : aircraft -> aircraft -> bool array array
