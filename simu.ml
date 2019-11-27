open Aircraft ;;

let rec update_pos a_list dt =
  match a_list with
    [] -> []
  | tete:queue -> begin
      Aircraft.move_aircraft tete;
      update_pos queue dt
    end;;

let simu a_list dt ending =
  while !(ending a_list) do update_pos a_list dt done;;
