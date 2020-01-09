type rectangle = int * int * int * int
val add : int -> int -> int -> int -> rectangle list -> rectangle list
(* add x y width height rectangles -> create and add the rectangle (x,y,width,height) in the list of rectangles rectangles *)
val listRectangles : int -> int -> bool array array -> rectangle list
(* listRectangles height width matrix -> find and return all the not dominated rectangles in the matrix of dimension (height width)*)
val getMostBalancedRectangle : rectangle list -> rectangle option
(* return the most balanced rectangle in the list of rectangle *)
