let rec ending a_list dt =
  match a_list with
    [] -> true
  | tete::queue -> (Aircraft.arrived tete dt) && (ending queue dt);;

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
        in write_line table.(i);
        Printf.fprintf file "\n";
      end
    done;
    close_out file;
  end;;

let simu env dt filename n =
  let table = Array.make n [] in
  begin
    while not (ending env dt) do
        Aircraft.updateEnv env dt table;
    done;
  for i=0 to (Array.length table - 1) do
    table.(i) <- List.rev table.(i)
  done;
  write filename table;
  end;;


let () =
  let a1 = Aircraft.createAircraft 0 (-100000.,-100000.) 45. 300. (100000.,100000.) in
  (*let a2 =  Aircraft.createAircraft 1 (100000.,-100000.) 135. 300. (-100000.,100000.) in*)
  let env = [a1] in
  simu env 60. "result.txt" 1;;
