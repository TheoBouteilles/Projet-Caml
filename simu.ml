open Aircraft ;;

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

let simu a_list dt file =
  while !(ending a_list) do begin
    write file a_list;
    update_pos a_list dt
      end
      done;;

