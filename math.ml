(* contains trigonometrics function *)

let pi = 4. *. atan 1.

let degrees = fun rad ->
  rad *. 180. /. pi

let radians = fun deg ->
  deg *. pi /. 180.


let allmax = fun f array ->
  let n = Array.length array in
  if n == 0 then
    [||]
  else
    begin
      let l = ref [0] in
      let max = ref (f array.(0)) in
      for i=0 to (n-1) do
        if f array.(i) > !max then
          begin
            l:= [i];
            max := f array.(i)
          end
        else if f array.(i) == !max then
          l := i::!l
      done;
      Array.of_list !l
    end
