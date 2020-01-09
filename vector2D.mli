type vector2D = float * float
val add : vector2D -> vector2D -> vector2D
(* add v v' -> return the addition of v with v' *)
val mul : vector2D -> float -> vector2D
(* mul v f -> return v * f with f float *)
val sub : vector2D -> vector2D -> vector2D
(* sub v v' -> return v - v' *)
val isZero : vector2D -> bool
(* isZero v -> return true if v is equal to (0.,0.) *)
val prod_scal : vector2D -> vector2D -> float
(* prod_scal v v' :  return v.v' *)
val vect_angle : vector2D -> float
(* vect_angle v : return the angle between (0.,1.) and v *)
val dist : vector2D -> vector2D -> float
(*dist p p' -> return the carthesian distance between p and p' *)
val norm : vector2D -> float
(* norm v -> return the norm of the vector v *)
val normalized : vector2D -> vector2D
(* normalized v -> return v with a norm of 1 *)
val setNorm : vector2D -> float -> vector2D
(* setNorm v f -> set the norm of v to f *)
