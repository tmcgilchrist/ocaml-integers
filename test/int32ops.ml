let () = print_endline "Signed Int32 operations test: ?"

(* Asserting max and min integers are correct *)
let () =
  let open Signed.Int32 in
  assert (to_int max_int = 2147483647);
  assert (to_int min_int = -2147483648);
  ()

(* Asserting that arithmetic wraps correctly *)
let () =
  let open Signed.Int32 in
  assert (equal (add max_int one) min_int);
  assert (equal (sub min_int one) max_int);
  ()

(* Asserting of_string/to_string round-trips *)
let () =
  let open Signed.Int32 in
  let check s =
    equal (of_string s) (of_string (to_string (of_string s)))
  in
  assert (check "0");
  assert (check "1");
  assert (check "-1");
  assert (check "2147483647");
  assert (check "-2147483648");
  assert (check "12345678");
  ()

(* Asserting to_hexstring for positive and negative values *)
let () =
  let open Signed.Int32 in
  assert (to_hexstring (of_string "0x23") = "23");
  assert (to_hexstring (of_string "0xDeadBeef") = "deadbeef");
  assert (to_hexstring (of_string "0x0") = "0");
  assert (to_hexstring max_int = "7fffffff");
  assert (to_hexstring min_int = "80000000");
  assert (to_hexstring minus_one = "ffffffff");
  ()

(* Asserting of_int/to_int conversions *)
let () =
  let open Signed.Int32 in
  assert (to_int (of_int 0) = 0);
  assert (to_int (of_int 1) = 1);
  assert (to_int (of_int (-1)) = -1);
  assert (to_int (of_int 2147483647) = 2147483647);
  assert (to_int (of_int (-2147483648)) = -2147483648);
  ()

(* Asserting of_int64/to_int64 round-trips *)
let () =
  let open Signed.Int32 in
  assert (to_int64 (of_int64 0L) = 0L);
  assert (to_int64 (of_int64 1L) = 1L);
  assert (to_int64 (of_int64 (-1L)) = -1L);
  assert (to_int64 (of_int64 2147483647L) = 2147483647L);
  assert (to_int64 (of_int64 (-2147483648L)) = -2147483648L);
  ()

(* Asserting of_string with various bases *)
let () =
  let open Signed.Int32 in
  assert (to_int (of_string "0xff") = 255);
  assert (to_int (of_string "0o77") = 63);
  assert (to_int (of_string "0b1111") = 15);
  assert (to_int (of_string "-0xff") = -255);
  assert (try (ignore (of_string "2147483648"); false) with Failure _ -> true);
  assert (try (ignore (of_string "-2147483649"); false) with Failure _ -> true);
  ()

(* Asserting multiplication wraps correctly *)
let () =
  let open Signed.Int32 in
  assert (equal (mul minus_one one) minus_one);
  assert (equal (mul minus_one min_int) min_int);
  assert (equal (mul max_int (of_int 2)) (of_int (-2)));
  ()

let () = print_endline "Signed Int32 operations test: ✓"
