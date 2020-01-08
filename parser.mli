val parse : string -> (string -> 'a) -> 'a list
module type ParsingRule = sig type t val parsing_rule : string -> t end
module Make :
  functor (PR : ParsingRule) ->
    sig
      type t = PR.t
      val parsing_rule : string -> PR.t
      val parse : string -> PR.t list
    end
