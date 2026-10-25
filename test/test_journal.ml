open Core
open Hledger

let parse s =
  match Parse.journal ~file:"t.journal" s with
  | Ok j -> j
  | Error e ->
    print_endline (Error.to_string_hum e);
    []
;;

let%expect_test "parse a transaction" =
  let j =
    parse
      {|2024-01-15 * (42) Shop | groceries  ; trip: rome
    expenses:food     $12.50 @ €0.92  ; receipt:
    assets:checking   = $100
|}
  in
  print_s [%sexp (j : Journal.t)];
  [%expect
    {|
    ((Transaction
      ((date 2024-01-15) (date2 ()) (status Cleared) (code (42))
       (description "Shop | groceries")
       (comment ((same_line ("trip: rome")) (lines ())))
       (postings
        (((status Unmarked) (account expenses:food) (kind Real)
          (amount
           ((Amount
             ((quantity 12.5) (commodity $)
              (style
               ((side Left) (spaced false) (decimal_mark .) (digit_groups ())
                (precision 2)))))))
          (cost
           ((Unit
             ((quantity 0.92) (commodity "\226\130\172")
              (style
               ((side Left) (spaced false) (decimal_mark .) (digit_groups ())
                (precision 2)))))))
          (lot ()) (assertion ()) (comment ((same_line (receipt:)) (lines ()))))
         ((status Unmarked) (account assets:checking) (kind Real) (amount ())
          (cost ()) (lot ())
          (assertion
           (((expected
              ((quantity 100) (commodity $)
               (style
                ((side Left) (spaced false) (decimal_mark .) (digit_groups ())
                 (precision 0)))))
             (expected_cost ()) (sole false) (inclusive false))))
          (comment ((same_line ()) (lines ())))))))))
    |}]
;;

let%expect_test "write a transaction built in code" =
  let usd q = Amount.create ~precision:2 ~commodity:"$" (Bigdecimal.of_string q) in
  let t =
    Journal.transaction
      ~status:Cleared
      ~description:"Acme | invoice 17"
      ~comment:{ same_line = Some (Tag.to_text [ "client", "acme" ]); lines = [] }
      (Date.of_string "2026-10-25")
      [ Journal.posting "assets:receivable" ~amount:(usd "1500.00")
      ; Journal.posting "revenues:consulting"
      ]
  in
  print_string (Print.transaction t);
  [%expect
    {|
    2026-10-25 * Acme | invoice 17  ; client: acme
        assets:receivable    $1500.00
        revenues:consulting
    |}]
;;

let%expect_test "decimal-mark and commodity directives steer parsing" =
  let j =
    parse
      {|commodity 1.000,00 EUR

2024-01-01
    a  1.000 EUR
    b  1.000 USD
|}
  in
  List.iter (Journal.transactions j) ~f:(fun t ->
    List.iter t.postings ~f:(fun p ->
      match p.amount with
      | Some (Amount a) -> print_endline (Bigdecimal.to_string_no_sn a.quantity)
      | _ -> ()));
  [%expect
    {|
    1000
    1
    |}]
;;

let%expect_test "errors carry file and line" =
  ignore (parse "2024-01-01\n    a  $1 $2\n" : Journal.t);
  ignore (parse "2024-02-30 bad date\n" : Journal.t);
  ignore (parse "1/1 no year\n" : Journal.t);
  ignore (parse "frobnicate x\n" : Journal.t);
  ignore
    (parse "decimal-mark ,\n2024-01-01\n    a  1.000,5\n    b  1,000.5\n" : Journal.t);
  [%expect
    {|
    t.journal:2: unexpected text "$2"
    t.journal:1: invalid date "2024-02-30"
    t.journal:1: date "1/1" has no year, and no Y directive or default year is set
    t.journal:1: unknown directive "frobnicate"
    t.journal:4: number does not match decimal mark ","
    |}]
;;
