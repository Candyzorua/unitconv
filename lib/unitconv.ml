open! Core

type unit_name =
  | Mm
  | Cm
  | M
  | Ft
  | In
[@@deriving enumerate]

let unit_to_string = function
  | Mm -> "mm"
  | Cm -> "cm"
  | M -> "m"
  | Ft -> "ft"
  | In -> "in"

let parse_unit value =
  match String.lowercase value with
  | "mm" -> Some Mm
  | "cm" -> Some Cm
  | "m" -> Some M
  | "ft" -> Some Ft
  | "in" -> Some In
  | _ -> None

let unit_to_meters = function
  | Mm -> 0.001
  | Cm -> 0.01
  | M -> 1.0
  | Ft -> 0.3048
  | In -> 0.0254

let convert value from_unit to_unit =
  let meters = value *. unit_to_meters from_unit in
  meters /. unit_to_meters to_unit

let supported_units =
  all_of_unit_name |> List.map ~f:unit_to_string |> String.concat ~sep:", "
