open Stream;;
open Genlex;;
open Scanf;;

type lex =
  Id of int
| Nombre of float
| Mot of string;;

type ac = { id : int ; vit : float*float ; pos : float*float };;

let lexer = Genlex.make_lexer[];;

let compte_ligne nom_fichier =
  let fic = open_in nom_fichier in
  let nbre = ref(0) in
  let rec init () =
    try
      ignore(input_line fic);incr nbre; init();
    with End_of_file -> close_in fic; !nbre
  in init();;

let mot = compte_ligne "aircraft.txt";;

let remplir_tab nom_fichier =
let nb_colonne = compte_ligne nom_fichier
  and fic = open_in nom_fichier in
let tableau_val = Array.make nb_colonne "" in
for i=0 to (nb_colonne - 1) do
  Array.set tableau_val i (input_line fic)
done;
close_in fic;tableau_val;;

let remplir_tab2 nom_fichier tab =
  let fic = open_in nom_fichier in
  tab := [||];
  try
    let rec loop() =
      tab := Array.append !tab [|(input_line fic)|];loop() in
    loop ();
  with End_of_file -> close_in fic;;


let analyseur nom_fichier =
  let t = ref([||]) in
  remplir_tab2 nom_fichier t;
let nb_avions = Array.length !t and conteneur = ref([]) in
for i = 0 to (nb_avions-1) do
  let mot = !t.(i) in
  let flux = lexer(Stream.of_string mot) in
  let rec analyse f l conteneur =
    try
      match (next flux) with
        Ident s -> conteneur := (Mot s)::!conteneur; analyse f ((Mot s)::l) conteneur
      |Float r -> conteneur := (Nombre r)::!conteneur; analyse f ((Nombre r)::l) conteneur
      |Int a -> conteneur := (Id a)::!conteneur; analyse f ((Id a)::l) conteneur
      |_ -> print_string("fin du début")
    with Stdlib.Stream.Failure -> print_int(0) in
  analyse flux [] conteneur;
done;
conteneur;;

let lex_to_int expr =
  match expr with
    Id i -> i
  |_ -> 0;;

let lex_to_float expr =
  match expr with
    Nombre i -> i
  |_ -> 0.;;

let create_ac i v p =
{id = i; vit = v ; pos = p};;

let parse nom_fic =
  let f = open_in nom_fic and l = [||] in
  print_string("Begin Parsing\n");
  let rec loop fichier tab =
    try
      print_string("...\n"); loop fichier (Array.append tab [|(sscanf (input_line fichier) "%s %d %f %f %f %f" (fun s i v1 v2 p1 p2 -> {id = i; vit = (v1,v2) ; pos = (p1,p2)}))|]);
    with End_of_file -> close_in fichier; print_string("Done !\n"); tab in
  loop f l;;
