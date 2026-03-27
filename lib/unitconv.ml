open! Core

module Length_unit = Length_unit
module Mass_unit = Mass_unit
module Unit_query_parser = Unit_query_parser

let supported_unit_types =
  Unit_query_parser.supported_unit_types () |> String.concat ~sep:", "

let supported_units_for_type unit_type =
  match Unit_query_parser.find unit_type with
  | None -> None
  | Some (T (module Unit)) -> Some Unit.supported_units

let convert unit_type value ~from_unit ~to_unit =
  match Unit_query_parser.find unit_type with
  | None ->
      Or_error.errorf
        "Unsupported unit type %S. Supported unit types: %s"
        unit_type
        supported_unit_types
  | Some (T (module Unit)) -> (
      match Unit.parse from_unit, Unit.parse to_unit with
      | Some from_unit, Some to_unit ->
          let result = Unit.convert value from_unit to_unit in
          Ok (result, Unit.to_string to_unit)
      | _ ->
          Or_error.errorf
            "Unsupported unit for type %S. Supported units: %s"
            unit_type
            Unit.supported_units)
