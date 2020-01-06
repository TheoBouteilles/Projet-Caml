let rec update_pos a_list dt table =
  match a_list with
    [] -> []
  | tete:queue -> begin
      let x,y = tete.pos in
      let a=Printf.sprintf "%.3f,%.3f,%.3f" x y tete.heading in
      table.(tete.id) <- table.(tete.id)::a
      Aircraft.move tete;
      update_pos queue dt table
    end;;

let rec ending a_list =
  match a_list with
    [] -> true
  | tete:queue -> (Aircraft.arrived tete) && (ending queue);;

let write filename table =
  begin
    let file = open_out filename in
    for i = 0 to Array.length table do
      begin
        Printf.fprintf file "%d" i;
        Printf.fprintf file "0";
        let rec write_line line=
          match l with
            [] -> []
          | tete:queue -> begin
              Printf.fprintf file " %s" tete;
              write_line queue;
            end
        in write_line table.(i);
        Printf.fprintf "\n";
      end
   done
      close_out file;
  end;;

let simu a_list dt filename =
  let naircrafts = List.length a_list in
  let table = Array.make naircrafts [] in
  begin
    while !(ending a_list) do
      begin
(*        Aircraft.algo a_list;*)
        update_pos a_list dt table;
      end;
    done;
    for i=0 to Array.length table do List.rev table.(i) done;
    write filename table;
  end;;

aircrafts = [(Aircraft.create_aircraft 0 0,0 0. 100. 0,1000)]
simu aircrafts 10 "results.txt";;
