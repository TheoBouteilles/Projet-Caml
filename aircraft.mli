type aircraft = {
  id : int;
  mutable pos : Vector2D.vector2D;
  mutable heading : float;
  mutable speed : float;
  destination : Vector2D.vector2D;
}
val _n : int
val _angleMax : float
val _separationLimite : float
val _conflictDetectionTime : float
val _detectionRadius : float
val create :
  int -> Vector2D.vector2D -> float -> float -> Vector2D.vector2D -> aircraft
val setSpeed : aircraft -> float -> unit
val setHeading : float -> aircraft -> unit
val getHeadingVector : aircraft -> float * float
val getSpeedVector : aircraft -> Vector2D.vector2D
val getDistanceAircrafts : aircraft -> aircraft -> float
val moveAircraft : float -> aircraft -> unit
val discoverNeighbors : aircraft -> aircraft list -> aircraft list
val getMinimalSeparationDistance :
  aircraft -> aircraft -> float -> float -> float
val getHeadings : aircraft -> float array
val getConflictMatrix : aircraft -> aircraft -> bool array array
val cmp : aircraft -> aircraft -> int
exception NoSolution
val getAvailableHeadings : aircraft -> aircraft list -> int Math.intervalle
val getNextHeading : aircraft -> aircraft list -> float
val updateEnv : aircraft list -> float -> unit
val arrived : aircraft -> float -> bool
val string_state : aircraft -> string
