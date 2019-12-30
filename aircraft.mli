module type HeadingsSpace = sig val n : int val headings : float array end
module Make :
  functor (Space : HeadingsSpace) ->
    sig
      type aircraft = {
        id : int;
        mutable pos : Vector2D.vector2D;
        mutable heading : float;
        mutable speed : float;
        mutable neighbors : aircraft list;
        route : Vector2D.vector2D array;
        activeWaypoint : int ref;
      }
      val createAircraft :
        int ->
        Vector2D.vector2D ->
        float -> float -> Vector2D.vector2D array -> aircraft
      val setSpeed : aircraft -> float -> unit
      val setHeading : float -> aircraft -> unit
      val getHeadingVector : aircraft -> float * float
      val getSpeedVector : aircraft -> Vector2D.vector2D
      val getDistanceAircrafts : aircraft -> aircraft -> float
      val moveAircraft : float -> aircraft -> unit
      val discoverNeighbors :
        float -> aircraft -> aircraft list -> aircraft list
      val extendedPos : aircraft -> float * float
      val getDistWithHeading : aircraft -> aircraft -> 'a -> 'b -> float
      val getDistHeadingMatrix : aircraft -> aircraft -> float array array
      val getConflictMatrix : aircraft -> aircraft -> bool array array
    end
