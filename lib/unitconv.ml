open! Core

module Length_unit = Length_unit
module Mass_unit = Mass_unit

type unit_name =
  | Length of Length_unit.t
  | Mass of Mass_unit.t

type unit_kind =
  | Length_kind
  | Mass_kind
[@@deriving equal]

let unit_to_string = function
  | Length unit -> Length_unit.to_string unit
  | Mass unit -> Mass_unit.to_string unit

let parse_unit value =
  match Length_unit.parse value with
  | Some unit -> Some (Length unit)
  | None -> Option.map (Mass_unit.parse value) ~f:(fun unit -> Mass unit)

let unit_kind = function
  | Length _ -> Length_kind
  | Mass _ -> Mass_kind

let convert value from_unit to_unit =
  match from_unit, to_unit with
  | Length from_unit, Length to_unit ->
      Length_unit.convert value from_unit to_unit
  | Mass from_unit, Mass to_unit -> Mass_unit.convert value from_unit to_unit
  | _ ->
      failwithf
        "Cannot convert between %s and %s because they are different unit types"
        (unit_to_string from_unit)
        (unit_to_string to_unit)
        ()

let supported_units =
  String.concat
    ~sep:", "
    [ Length_unit.supported_units; Mass_unit.supported_units ]
