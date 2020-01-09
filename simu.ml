let rec ending env dt =
  match env with
    [] -> true
  | tete::queue -> (Aircraft.arrived tete) && (ending queue dt);;

let write filename table =
  begin
    let file = open_out filename in
    for i = 0 to (Array.length table -1) do
      begin
        Printf.fprintf file "%d" i;
        Printf.fprintf file " 0";
        let rec write_line line=
          match line with
            [] -> ()
          | tete::queue -> begin
              Printf.fprintf file " %s" tete;
              write_line queue;
            end
        in write_line (List.rev table.(i));
        Printf.fprintf file "\n";
      end
    done;
    close_out file;
  end;;


let cluster_create n =
  let angle = 360. /. (float n) in
  let rec loop n =
    match n with
    | 0 -> []
    | i -> let goal = (150000. *. cos (Math.radians (float i) *. angle), 150000. *. sin (Math.radians (float i) *. angle)) in
      let start = Vector2D.mul goal (-1.) in
      (Aircraft.create (i-1) start (angle *. (float i)) 300. goal)::(loop (n-1))
  in
  loop n;;


let simu env dt =
  let n = List.length env in
  let table = Array.make n [] in
  List.iteri (fun i a -> table.(i) <- [Aircraft.string_state a]) env;
  try
    begin
      while not (ending env dt) do
        Aircraft.updateEnv env dt;
        List.iteri (fun i a -> table.(i) <- (Aircraft.string_state a)::table.(i)) env;
      done;
    end;
    table
  with e ->
    let msg = Printexc.to_string e in
    let stack = Printexc.get_backtrace () in
    Printf.eprintf "%s %s\n" msg stack;
    write "Results/dump.txt" table;
    [||]
;;


module AircraftParsingRule = struct
  type t = Aircraft.aircraft
  let parsing_rule = fun fic_line ->
    Scanf.sscanf
      fic_line
      "id=%d start=(%f,%f) heading=%f speed=%f goal=(%f,%f)"
      (fun id x y h s gx gy -> Aircraft.create id (x, y) h s (gx, gy))
end

module EnvParser = Parser.Make(AircraftParsingRule)
  
let () =
  Sys.catch_break true;
  let env = match Sys.argv.(1) with
    | "-cluster" -> cluster_create (int_of_string Sys.argv.(2))
    | "-file" -> EnvParser.parse(Sys.argv.(2))
    | _ -> raise (Invalid_argument Sys.argv.(1))
  in
  let result_file = Sys.argv.(3) in
  let results_table = simu env (float_of_string Sys.argv.(4)) in
  write result_file results_table
;;
