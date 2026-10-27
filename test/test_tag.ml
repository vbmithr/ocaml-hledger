open Base
open Stdio
open Hledger

let show s = print_s [%sexp (Tag.of_line s : Tag.t list)]

let%expect_test "tags" =
  show "some-tag:";
  show "tag1: this is tag1's value";
  show "tag1:value 1, tag2:value 2, comment text";
  show "tag1:value 1, tag1:value 2";
  show "see https://foo.com";
  show "foo : not a tag";
  show "no tags here";
  [%expect
    {|
    ((some-tag ""))
    ((tag1 "this is tag1's value"))
    ((tag1 "value 1") (tag2 "value 2"))
    ((tag1 "value 1") (tag1 "value 2"))
    ((https //foo.com))
    ()
    ()
    |}]
;;

let%expect_test "to_text" =
  print_endline (Tag.to_text [ "trip", "rome"; "reimbursable", "" ]);
  [%expect {| trip: rome, reimbursable: |}]
;;
