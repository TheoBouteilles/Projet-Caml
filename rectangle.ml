type rectangle = int * int * int * int

let add = fun col row width height (list:rectangle list) ->
  (* (col,row) = top left corner coordinate*)
  (col,row,width,height)::list
;;

let listRectangles = fun (w:int) (h:int) (boolmatrix:bool array array) ->
  begin
    let list = ref [] in
    let dN = Array.make_matrix w h 0 in
    
    for col = 0 to (w - 1) do
      dN.(col).(0) <- if boolmatrix.(col).(0) then 0 else -1
    done;
    
    for row = 1 to (h-1) do
      for col = 0 to (w-1) do
        if not boolmatrix.(col).(row) then
          dN.(col).(row) <- -1
        else
          dN.(col).(row) <- dN.(col).(row - 1) + 1
      done
    done;
    
    let dS = Array.make_matrix w h 0 in

    for col = 0 to (w -1) do
      dS.(col).(h - 1) <- if boolmatrix.(col).(h - 1) then 0 else - 1
    done;

    for row = (h - 2) downto 0 do
      for col = 0 to (w - 1) do
        if not boolmatrix.(col).(row) then
          dS.(col).(row) <- -1
        else
          dS.(col).(row + 1) <- +1
      done
    done;

    for col = (w - 1) downto 0 do
      let maxS = ref h in
      for row = (h - 1) downto 0 do
        begin
          incr maxS;
          if boolmatrix.(col).(row)
          && (col == 0 || not boolmatrix.(col - 1).(row)) then
            begin
              let n = ref dN.(col).(row) in
              let s = ref dS.(col).(row) in
              let width = ref 1 in
              while (col + !width < w && boolmatrix.(col + !width).(row)) do
                let nextN = dN.(col + !width).(row) in
                let nextS = dS.(col + !width).(row) in
                begin
                  if ((nextN < !n) || (nextS < !s)) then
                    begin
                      if (s < maxS) then
                        list := add col (row - !n) !width (!n + !s + 1) !list;
                      if (nextN < !n) then
                        n := nextN;
                      if (nextS < !s) then
                        s := nextS;
                    end;
                  incr width;
                end
              done;
              if (s < maxS) then
                list := add col (row - !n) !width (!n + !s + 1) !list;
            end
        end
      done
    done;
    !list
  end
;;


let getMostBalancedRectangle = fun (list:rectangle list) ->
  if list == [] then None
  else
    let rec loop mbr list =
      match list with
      | [] -> Some mbr
      | rect::l' ->
        let _,_,w,h = rect in
        let _,_,wMBR,hMBR = mbr in
        if min w h > min wMBR hMBR then
          loop rect l'
        else if min w h == min wMBR hMBR then
          if max w h > max wMBR hMBR then
            loop rect l'
          else loop mbr l'
        else loop mbr l'
    in
    loop (0,0,0,0) list
;;


let intersection = fun (rect:rectangle) (rect':rectangle) : rectangle option ->
  let j,i,w,h = rect in
  let j',i',w',h' = rect' in
  let newj = max j j' in
  let newi = max i i' in
  let neww = max 0 ((min (j + w) (j' + w')) - newj) in
  let newh = max 0 ((min (i + h) (i' + h')) - newi) in
  if neww == 0 || newh == 0 then None
  else Some (newj, newi, neww, newh)
