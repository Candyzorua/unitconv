open! Core

module Definition = struct
  type t =
    | Kg
    | Lbs
    | G
    | Oz
    | T
  [@@deriving enumerate]

  let to_string = function
    | Kg -> "kg"
    | Lbs -> "lbs"
    | G -> "g"
    | Oz -> "oz"
    | T -> "t"

  let parse value =
    match String.lowercase value with
    | "kg" -> Some Kg
    | "lbs" -> Some Lbs
    | "g" -> Some G
    | "oz" -> Some Oz
    | "t" -> Some T
    | _ -> None

  let unit_scale = function
    | Kg -> 1.0
    | Lbs -> 0.45359237
    | G -> 0.001
    | Oz -> 0.028349523125
    | T -> 1000.0
end

include Definition
include Unit_intf.Make (Definition)
