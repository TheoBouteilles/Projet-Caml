let parse nom_fic parsing_rule=
  let fic = open_in nom_fic in
  print_string("Begin Parsing\n");
  let rec loop () =
    try
      let s = input_line fic in
      (parsing_rule s)::loop()
    with End_of_file -> [];
  in
  let result = List.rev (loop()) in
  print_string("Done !\n");
  result
;;

module type ParsingRule = sig
  type t
  val parsing_rule : string -> t
end

module Make = functor (PR:ParsingRule) -> struct
  type t = PR.t
  let parsing_rule = PR.parsing_rule
  let parse = fun nom_fic -> parse nom_fic parsing_rule
end
