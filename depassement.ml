open Printf;;

let point_absc (px,py) (a,b) =
let x = if a <> 0. then (py -. b) /. a else px in
x;;

let angle_vect (v1, v2) (a1, a2) =
let angle_rad = acos ((v1 *. a1 +. v2 *. a2)/.(sqrt(v1*.v1 +. v2*.v2) *. sqrt(a1*.a1 +. a2*.a2))) in
180. *. angle_rad /. 3.141592653;;

let has_passed (px,py) (a,b) ?(x=1.) (a1,a2) =
  let rep = if (a == 0. && b <> 0.) then
      begin
        if a1 < 0. then
          begin
            if b > py then "gauche" else "droite"
          end
        else
          begin
            if b > py then "droite" else "gauche"
          end
      end
      else
      if ((a,b) == (0., 0.)) then
        begin
          if a2 > 0. then
            begin
              if px < x then "gauche" else "droite"
            end
          else
            begin
              if px < x then "droite" else "gauche"
            end
        end
      else
        begin
          let x1 = point_absc (px,py) (a,b) in
          let (v1,v2) = if px -. x1 > 0. then (1., 0.) else (-1., 0.) in
          let angle = angle_vect (v1, v2) (a1, a2) in
          print_float(angle);
          if angle > 90. then "gauche" else "droite"
        end in
  rep ;;
