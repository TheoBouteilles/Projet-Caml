type aircraft = {
  id : int;
  mutable pos : Vector2D.vector2D;
  mutable heading : float;
  mutable speed : float;
  mutable neighbors : aircraft list;
  route : Vector2D.vector2D array;
  mutable activeWaypoint : int;
}
val createAircraft :
  int ->
  Vector2D.vector2D -> float -> float -> Vector2D.vector2D array -> aircraft
val setSpeed : aircraft -> float -> unit
val setHeading : float -> aircraft -> unit
val getHeadingVector : aircraft -> float * float
val getSpeedVector : aircraft -> Vector2D.vector2D
val getDistanceAircrafts : aircraft -> aircraft -> float
val moveAircraft : float -> aircraft -> unit
val discoverNeighbors : float -> aircraft -> aircraft list -> aircraft list
val getMinimalSeparationDistanceForHeading :
  aircraft -> aircraft -> 'a -> 'b -> float
val getMinimalSeparationDistanceMatrix :
  aircraft -> aircraft -> 'a array -> float array array
val getConflictMatrix : aircraft -> aircraft -> 'a array -> bool array array
val isPrioOn : aircraft -> aircraft -> bool
val getAvailableHeadings : aircraft -> aircraft -> int array -> int array
module AircraftModel :
  sig
    type state = Vector2D.vector2D * float
    type user_param = aircraft
    val initial_state : aircraft -> Vector2D.vector2D * float
    val is_goal : aircraft -> Vector2D.vector2D -> bool
    val k : 'a -> Vector2D.vector2D -> Vector2D.vector2D -> float
    val h : aircraft -> Vector2D.vector2D -> float
    val next : 'a -> 'b -> 'c list
    val do_at_insertion : 'a -> 'b -> 'c -> unit
    val do_at_extraction : 'a -> 'b -> 'c -> 'd -> unit
  end
