open! Core

module Definition = struct
  let name = "mass"

  type t =
    | Kg
    | Lbs
    | G
    | Oz
    | T
  [@@deriving enumerate, sexp]

  let to_string unit = Sexp.to_string (sexp_of_t unit)

  let parse value =
    try Some (t_of_sexp (Sexp.Atom value)) with
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
