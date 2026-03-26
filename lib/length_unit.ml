open! Core

module Definition = struct
  type t =
    | Mm
    | Cm
    | M
    | Ft
    | In
  [@@deriving enumerate]

  let to_string = function
    | Mm -> "mm"
    | Cm -> "cm"
    | M -> "m"
    | Ft -> "ft"
    | In -> "in"

  let parse value =
    match String.lowercase value with
    | "mm" -> Some Mm
    | "cm" -> Some Cm
    | "m" -> Some M
    | "ft" -> Some Ft
    | "in" -> Some In
    | _ -> None

  let unit_scale = function
    | Mm -> 0.001
    | Cm -> 0.01
    | M -> 1.0
    | Ft -> 0.3048
    | In -> 0.0254
end

include Definition
include Unit_intf.Make (Definition)
