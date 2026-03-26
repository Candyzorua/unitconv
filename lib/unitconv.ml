open! Core

type unit_name =
  | Mm
  | Cm
  | M
  | Ft
  | In
  | Kg
  | Lbs
  | G
  | Oz
  | T
[@@deriving enumerate]

type unit_kind =
  | Length
  | Mass
[@@deriving equal]

let unit_to_string = function
  | Mm -> "mm"
  | Cm -> "cm"
  | M -> "m"
  | Ft -> "ft"
  | In -> "in"
  | Kg -> "kg"
  | Lbs -> "lbs"
  | G -> "g"
  | Oz -> "oz"
  | T -> "t"

let parse_unit value =
  match String.lowercase value with
  | "mm" -> Some Mm
  | "cm" -> Some Cm
  | "m" -> Some M
  | "ft" -> Some Ft
  | "in" -> Some In
  | "kg" -> Some Kg
  | "lbs" -> Some Lbs
  | "g" -> Some G
  | "oz" -> Some Oz
  | "t" -> Some T
  | _ -> None

let unit_kind = function
  | Mm | Cm | M | Ft | In -> Length
  | Kg | Lbs | G | Oz | T -> Mass

let unit_scale = function
  | Mm -> 0.001
  | Cm -> 0.01
  | M -> 1.0
  | Ft -> 0.3048
  | In -> 0.0254
  | Kg -> 1.0
  | Lbs -> 0.45359237
  | G -> 0.001
  | Oz -> 0.028349523125
  | T -> 1000.0

let convert value from_unit to_unit =
  if not ([%equal: unit_kind] (unit_kind from_unit) (unit_kind to_unit))
  then
    failwithf
      "Cannot convert between %s and %s because they are different unit types"
      (unit_to_string from_unit)
      (unit_to_string to_unit)
      ();
  let base_value = value *. unit_scale from_unit in
  base_value /. unit_scale to_unit

let supported_units =
  all_of_unit_name |> List.map ~f:unit_to_string |> String.concat ~sep:", "
