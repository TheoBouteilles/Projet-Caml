val ending : Aircraft.aircraft list -> float -> bool
val write : string -> string list array -> unit
val simu : Aircraft.aircraft list -> float -> string list array
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
