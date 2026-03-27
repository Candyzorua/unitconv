open! Core

module Definition = struct
  let name = "length"

  type t =
    | Mm
    | Cm
    | M
    | Ft
    | In
  [@@deriving enumerate, sexp]

  let to_string unit = Sexp.to_string (sexp_of_t unit)

  let parse value =
    try Some (t_of_sexp (Sexp.Atom value)) with
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
