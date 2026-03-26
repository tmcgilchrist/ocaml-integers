let () = print_endline "Unsigned UInt32 operations test: ?"

(* Asserting max_int value *)
let () =
  let open Unsigned.UInt32 in
  assert (to_int max_int = 4294967295);
  ()

(* Asserting wrapping arithmetic *)
let () =
  let open Unsigned.UInt32 in
  assert (equal (add max_int one) zero);
  assert (equal (sub zero one) max_int);
  ()

(* Asserting of_int/to_int round-trips *)
let () =
  let open Unsigned.UInt32 in
  assert (to_int (of_int 0) = 0);
  assert (to_int (of_int 1) = 1);
  assert (to_int (of_int 255) = 255);
  assert (to_int (of_int 4294967295) = 4294967295);
  ()

(* Asserting of_string/to_string round-trips *)
let () =
  let open Unsigned.UInt32 in
  let check s =
    equal (of_string s) (of_string (to_string (of_string s)))
  in
  assert (check "0");
  assert (check "1");
  assert (check "4294967295");
  assert (check "12345678");
  ()

(* Asserting of_int32/to_int32 round-trips *)
let () =
  let open Unsigned.UInt32 in
  assert (to_int32 (of_int32 0l) = 0l);
  assert (to_int32 (of_int32 1l) = 1l);
  assert (to_int32 (of_int32 (-1l)) = -1l);
  assert (to_int32 (of_int32 Int32.max_int) = Int32.max_int);
  assert (to_int32 (of_int32 Int32.min_int) = Int32.min_int);
  ()

(* Asserting bitwise operations *)
let () =
  let open Unsigned.UInt32 in
  assert (equal (logand max_int zero) zero);
  assert (equal (logor max_int zero) max_int);
  assert (equal (logxor max_int max_int) zero);
  assert (equal (shift_left one 31) (of_string "2147483648"));
  assert (equal (shift_right (of_string "2147483648") 31) one);
  ()

(* Asserting multiplication wraps correctly *)
let () =
  let open Unsigned.UInt32 in
  assert (equal (mul max_int max_int) one);
  ()

(* Asserting hexstring *)
let () =
  let open Unsigned.UInt32 in
  assert (to_hexstring (of_string "0xDeadBeef") = "deadbeef");
  assert (to_hexstring zero = "0");
  assert (to_hexstring max_int = "ffffffff");
  ()

let () = print_endline "Unsigned UInt32 operations test: ✓"
