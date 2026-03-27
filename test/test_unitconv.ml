open! Core

let approx_equal expected actual =
  Float.(abs (expected - actual) < 0.000001)

let%test "mm to cm" =
  approx_equal 1.0
    (fst (Or_error.ok_exn (Unitconv.convert "length" 10.0 ~from_unit:"Mm" ~to_unit:"Cm")))

let%test "cm to m" =
  approx_equal 2.5
    (fst (Or_error.ok_exn (Unitconv.convert "length" 250.0 ~from_unit:"Cm" ~to_unit:"M")))

let%test "m to ft" =
  approx_equal 3.280839895
    (fst (Or_error.ok_exn (Unitconv.convert "length" 1.0 ~from_unit:"M" ~to_unit:"Ft")))

let%test "in to ft" =
  approx_equal 1.0
    (fst (Or_error.ok_exn (Unitconv.convert "length" 12.0 ~from_unit:"In" ~to_unit:"Ft")))

let%test "ft to in" =
  approx_equal 36.0
    (fst (Or_error.ok_exn (Unitconv.convert "length" 3.0 ~from_unit:"Ft" ~to_unit:"In")))

let%test "kg to g" =
  approx_equal 1000.0
    (fst (Or_error.ok_exn (Unitconv.convert "mass" 1.0 ~from_unit:"Kg" ~to_unit:"G")))

let%test "lbs to oz" =
  approx_equal 16.0
    (fst (Or_error.ok_exn (Unitconv.convert "mass" 1.0 ~from_unit:"Lbs" ~to_unit:"Oz")))

let%test "t to kg" =
  approx_equal 1000.0
    (fst (Or_error.ok_exn (Unitconv.convert "mass" 1.0 ~from_unit:"T" ~to_unit:"Kg")))

let%test "accepts capitalized unit type" =
  approx_equal 1.0
    (fst (Or_error.ok_exn (Unitconv.convert "Length" 10.0 ~from_unit:"Mm" ~to_unit:"Cm")))

let%test "rejects invalid unit in type" =
  Or_error.is_error
    (Unitconv.convert "length" 1.0 ~from_unit:"M" ~to_unit:"Kg")

let%test "rejects invalid unit type" =
  Or_error.is_error
    (Unitconv.convert "volume" 1.0 ~from_unit:"Liter" ~to_unit:"Ml")
