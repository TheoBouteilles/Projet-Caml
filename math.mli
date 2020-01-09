val degrees : float -> float
(* convert a angle in radians in degrees *)
val radians : float -> float
(* convert a angle in degrees in radians *)
val normalize : float -> float
(* normalize an angle in degree *)
type 'a intervalle = Empty | Intervalle of 'a * 'a
val intersection : 'a intervalle -> 'a intervalle -> 'a intervalle
(* return an intersection between two intervalles *)
