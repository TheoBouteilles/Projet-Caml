val pi : float
val degrees : float -> float
val radians : float -> float
val normalize : float -> float
type 'a intervalle = Empty | Intervalle of 'a * 'a
val intersection : 'a intervalle -> 'a intervalle -> 'a intervalle
