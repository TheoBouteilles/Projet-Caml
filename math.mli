type vector2D = float * float
val pi : float
val add : vector2D -> vector2D -> vector2D
val mul : vector2D -> float -> vector2D
val sub : vector2D -> vector2D -> vector2D
val isZero : vector2D -> bool
val prod_scal : vector2D -> vector2D -> float
val rad_to_deg : float -> float
val deg_to_rad : float -> float
val vect_angle : vector2D -> float
val radToVect : float -> vector2D
val degToVect : float -> vector2D
val dist : vector2D -> vector2D -> float
val norm : vector2D -> float
val normalized : vector2D -> vector2D
val setNorm : vector2D -> float -> vector2D
