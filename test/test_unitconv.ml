open! Core

let approx_equal expected actual =
  Float.(abs (expected - actual) < 0.000001)

let%test "mm to cm" =
  approx_equal 1.0 (Unitconv.convert 10.0 Unitconv.Mm Unitconv.Cm)

let%test "cm to m" =
  approx_equal 2.5 (Unitconv.convert 250.0 Unitconv.Cm Unitconv.M)

let%test "m to ft" =
  approx_equal 3.280839895 (Unitconv.convert 1.0 Unitconv.M Unitconv.Ft)

let%test "in to ft" =
  approx_equal 1.0 (Unitconv.convert 12.0 Unitconv.In Unitconv.Ft)

let%test "ft to in" =
  approx_equal 36.0 (Unitconv.convert 3.0 Unitconv.Ft Unitconv.In)

let%test "kg to g" =
  approx_equal 1000.0 (Unitconv.convert 1.0 Unitconv.Kg Unitconv.G)

let%test "lbs to oz" =
  approx_equal 16.0 (Unitconv.convert 1.0 Unitconv.Lbs Unitconv.Oz)

let%test "t to kg" =
  approx_equal 1000.0 (Unitconv.convert 1.0 Unitconv.T Unitconv.Kg)

let%test "mass and length cannot mix" =
  Result.is_error
    (Result.try_with (fun () -> Unitconv.convert 1.0 Unitconv.M Unitconv.Kg))
