type rectangle = int * int * int * int
val add : int -> int -> int -> int -> rectangle list -> rectangle list
val listRectangles : int -> int -> bool array array -> rectangle list
val getMostBalancedRectangle : rectangle list -> rectangle option
