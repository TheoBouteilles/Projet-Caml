val ending : Aircraft.environment -> float -> bool
(* ending env dt -> return true if all the aircraft of the environment arrived at their destination *) 
val write : string -> string list array -> unit
(* write filename env_table -> write in the file filename the data stored in the env_table *)
val simu : Aircraft.environment -> float -> string list array
(* simu env dt -> start a simulation with the environment env and the period dt *)
module AircraftParsingRule :
  sig
    type t = Aircraft.aircraft
    val parsing_rule : string -> Aircraft.aircraft
  end
module EnvParser :
  sig
    type t = AircraftParsingRule.t
    val parsing_rule : string -> AircraftParsingRule.t
    val parse : string -> AircraftParsingRule.t list
  end
