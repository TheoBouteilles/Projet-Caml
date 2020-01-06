
let rec update_pos a_list dt =
  match a_list with
    [] -> []
  | tete:queue -> begin
      Aircraft.move_aircraft tete;
      update_pos queue dt
    end;;

let rec ending a_list =
  match a_list with
    [] -> true
  | tete:queue -> (Aircraft.arrived tete) && (ending queue);;

let add_to_list a_list w_list =
  let rec add_to_list_rec a_list =
    match a_list with
      [] -> []
    | tete:queue -> begin
        let line = Aircraft.print tete in
        List.append w_list line;
        add_to_list_rec queue;
      end
  in
  add_to_list_rec a_list;;

let write filename lines =
  begin
    let file = open_out filename in
    let rec write_lines l =
      match l with
        [] -> []
      | tete:queue -> begin
          output_string tete;
          write_lines queue;
        end
    in
    write_lines lines;
    close_out file;
  end;;

let simu a_list dt filename =
  let lines = [] in
  begin
    while !(ending a_list) do begin
      add_to_list a_list lines;
      Aircraft.algo a_list;
      update_pos a_list dt;
    end
    done;
    write filename lines;
  end;;


simu aircrafts 10 "results.txt";;
