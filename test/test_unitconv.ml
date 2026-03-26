open! Core

let approx_equal expected actual =
  Float.(abs (expected - actual) < 0.000001)

let assert_conversion ~value ~from_unit ~to_unit ~expected =
  let actual = Unitconv.convert value from_unit to_unit in
  if not (approx_equal expected actual) then
    failwith
      (Printf.sprintf
         "expected %.6f but got %.6f when converting %.6f %s to %s"
         expected actual value
         (Unitconv.unit_to_string from_unit)
         (Unitconv.unit_to_string to_unit))

let () =
  assert_conversion ~value:10.0 ~from_unit:Unitconv.Mm ~to_unit:Unitconv.Cm
    ~expected:1.0;
  assert_conversion ~value:250.0 ~from_unit:Unitconv.Cm ~to_unit:Unitconv.M
    ~expected:2.5;
  assert_conversion ~value:1.0 ~from_unit:Unitconv.M ~to_unit:Unitconv.Ft
    ~expected:3.280839895;
  assert_conversion ~value:12.0 ~from_unit:Unitconv.In ~to_unit:Unitconv.Ft
    ~expected:1.0;
  assert_conversion ~value:3.0 ~from_unit:Unitconv.Ft ~to_unit:Unitconv.In
    ~expected:36.0
