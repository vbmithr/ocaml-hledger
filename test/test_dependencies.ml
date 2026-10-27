open Base
open Stdio
open Hledger

let%expect_test "decimal quantities stay exact under a small arithmetic context" =
  let original = !Decimal.Context.default in
  Exn.protect
    ~finally:(fun () -> Decimal.Context.default := original)
    ~f:(fun () ->
      Decimal.Context.default := Decimal.Context.make ~prec:2 ~e_max:9 ~e_min:(-9) ();
      List.iter
        [ "1234567890123456789012345678901234567890.000000000000000000000000000009"
        ; "-1234567890123456789012345678901234567890.12345678901234567890123456789"
        ; "1e-40"
        ; "1e40"
        ; "-0.0000"
        ]
        ~f:(fun source ->
          let amount = Amount.of_string_exn source in
          assert (Decimal.equal amount.quantity (Decimal.of_string source));
          let rendered = Amount.to_string amount in
          assert (Amount.equal amount (Amount.of_string_exn rendered));
          assert (Amount.equal amount (Amount.neg (Amount.neg amount)));
          let sexp_roundtrip = Amount.t_of_sexp (Amount.sexp_of_t amount) in
          assert (Amount.equal amount sexp_roundtrip)));
  print_endline "all decimal digits, signs and styles preserved";
  [%expect {| all decimal digits, signs and styles preserved |}]
;;

let%expect_test "non-finite quantities cannot be written as journal amounts" =
  List.iter [ Decimal.nan; Decimal.infinity; Decimal.neg_infinity ] ~f:(fun quantity ->
    assert (
      Result.is_error
        (Or_error.try_with (fun () -> Amount.create ~commodity:"EUR" quantity))));
  print_endline "NaN and infinities rejected";
  [%expect {| NaN and infinities rejected |}]
;;

let%expect_test "Ptime calendar dates, price times and sexp round trips" =
  let source =
    "P 2024-02-29 12:34 EUR $1.00\n2024-02-29=03/01 Leap day\n    a  $1\n    b\n"
  in
  let journal = Parse.journal_exn source in
  let transaction = List.hd_exn (Journal.transactions journal) in
  assert (Poly.equal transaction.date (2024, 2, 29));
  assert (Poly.equal transaction.date2 (Some (2024, 3, 1)));
  assert (Journal.equal journal (Journal.t_of_sexp (Journal.sexp_of_t journal)));
  let printed = Print.journal journal in
  assert (Journal.equal journal (Parse.journal_exn printed));
  assert (String.is_substring printed ~substring:"P 2024-02-29 12:34:00 EUR $1.00");
  List.iter
    [ "2023-02-29\n"; "P 2024-01-01 25:00 EUR $1\n"; "P 2024-01-01 12:61 EUR $1\n" ]
    ~f:(fun source -> assert (Result.is_error (Parse.journal source)));
  print_endline "calendar dates and timezone-free price times preserved";
  [%expect {| calendar dates and timezone-free price times preserved |}]
;;
