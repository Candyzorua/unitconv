open! Core

module type Basic = sig
  type t

  val all : t list
  val to_string : t -> string
  val parse : string -> t option
  val unit_scale : t -> float
end

module type S = sig
  type t

  val all : t list
  val to_string : t -> string
  val parse : string -> t option
  val unit_scale : t -> float
  val convert : float -> t -> t -> float
  val supported_units : string
end

module Make (Unit : Basic) : S with type t := Unit.t = struct
  include Unit

  let convert value from_unit to_unit =
    let base_value = value *. unit_scale from_unit in
    base_value /. unit_scale to_unit

  let supported_units =
    all |> List.map ~f:to_string |> String.concat ~sep:", "
end
