open Base
open Stdio
open Hledger

let show ?hint s =
  match Amount.of_string ?hint s with
  | Error e -> print_endline ("error: " ^ Error.to_string_hum e)
  | Ok a ->
    printf
      "%-18s q=%s prec=%d -> %s\n"
      s
      (Decimal.to_string ~format:`plain a.quantity)
      a.style.precision
      (Amount.to_string a)
;;

let%expect_test "amount formats" =
  List.iter
    ~f:show
    [ "$1"
    ; "-$1"
    ; "$-1"
    ; "$ -  1"
    ; "4000 AAPL"
    ; "3 \"green apples\""
    ; "1E-6"
    ; "EUR 1E3"
    ; "$1,000,000.00"
    ; "EUR 2.000.000,00"
    ; "INR 9,99,99,999.00"
    ; "CHF 1'000'000.00"
    ; "1_000_000.00"
    ; "1 000 000.00"
    ; "1\xc2\xa0000.5 €"
    ; "10."
    ; "1,000."
    ; ".5 X"
    ; "€12,50"
    ];
  [%expect
    {|
    $1                 q=1 prec=0 -> $1
    -$1                q=-1 prec=0 -> $-1
    $-1                q=-1 prec=0 -> $-1
    $ -  1             q=-1 prec=0 -> $ -1
    4000 AAPL          q=4000 prec=0 -> 4000 AAPL
    3 "green apples"   q=3 prec=0 -> 3 "green apples"
    1E-6               q=0.000001 prec=6 -> 0.000001
    EUR 1E3            q=1000 prec=0 -> EUR 1000
    $1,000,000.00      q=1000000.00 prec=2 -> $1,000,000.00
    EUR 2.000.000,00   q=2000000.00 prec=2 -> EUR 2.000.000,00
    INR 9,99,99,999.00 q=99999999.00 prec=2 -> INR 9,99,99,999.00
    CHF 1'000'000.00   q=1000000.00 prec=2 -> CHF 1'000'000.00
    1_000_000.00       q=1000000.00 prec=2 -> 1_000_000.00
    1 000 000.00       q=1000000.00 prec=2 -> 1 000 000.00
    1 000.5 €       q=1000.5 prec=1 -> 1 000.5 €
    10.                q=10 prec=0 -> 10
    1,000.             q=1000 prec=0 -> 1,000.
    .5 X               q=0.5 prec=1 -> 0.5 X
    €12,50           q=12.50 prec=2 -> €12,50
    |}]
;;

let%expect_test "ambiguous numbers" =
  (* Without a hint a lone '.' or ',' is a decimal mark, as in hledger. *)
  show "1,000";
  show "1.000";
  show ~hint:(fun _ -> Some '.') "1,000";
  show ~hint:(fun _ -> Some ',') "1.000";
  show ~hint:(fun _ -> Some ',') "1,000.00";
  show ~hint:(fun _ -> Some '.') "1.2.34";
  show "1,000.00,5";
  [%expect
    {|
    1,000              q=1.000 prec=3 -> 1,000
    1.000              q=1.000 prec=3 -> 1.000
    1,000              q=1000 prec=0 -> 1,000.
    1.000              q=1000 prec=0 -> 1.000,
    error: number does not match decimal mark ","
    error: number does not match decimal mark "."
    error: inconsistent digit group marks
    |}]
;;

let%expect_test "building amounts" =
  let a = Amount.create ~commodity:"$" (Decimal.of_string "-12.5") in
  print_endline (Amount.to_string a);
  print_endline (Amount.to_string { a with style = { a.style with precision = 2 } });
  print_endline (Amount.to_string (Amount.create ~commodity:"EUR" (Decimal.of_int 3)));
  print_endline (Amount.to_string (Amount.create ~commodity:"AAAA 2023" Decimal.one));
  [%expect
    {|
    $-12.5
    $-12.50
    3 EUR
    1 "AAAA 2023"
    |}]
;;
