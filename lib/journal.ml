open Base
module Ptime = Calendar

type status =
  | Unmarked
  | Pending
  | Cleared
[@@deriving sexp, compare, equal]

(* Text after the ';' of a same-line comment, and of following indented comment lines.
   Tags live inside comment text; see [Tag]. *)
type comment =
  { same_line : string option
  ; lines : string list
  }
[@@deriving sexp, compare, equal]

let no_comment = { same_line = None; lines = [] }

type cost =
  | Unit of Amount.t (* @ *)
  | Total of Amount.t (* @@ *)
[@@deriving sexp, compare, equal]

(* Cost basis annotation: hledger's {DATE, "LABEL", COST}, or Ledger's {COST} or
   {{TOTAL}} with [DATE] and (LABEL) annotations. *)
type lot =
  { lot_date : Ptime.date option
  ; label : string option
  ; basis : cost option
  }
[@@deriving sexp, compare, equal]

type posting_kind =
  | Real
  | Virtual (* (account): unbalanced *)
  | Balanced_virtual (* [account] *)
[@@deriving sexp, compare, equal]

type posting_amount =
  | Amount of Amount.t
  | Multiplier of Amount.t (* *N or *AMOUNT, only meaningful in auto posting rules *)
[@@deriving sexp, compare, equal]

type assertion =
  { expected : Amount.t
  ; expected_cost : cost option
  ; sole : bool (* == : no other commodity *)
  ; inclusive : bool (* =* : including subaccounts *)
  }
[@@deriving sexp, compare, equal]

(* A posting with no [amount] but an [assertion] is a balance assignment. *)
type posting =
  { status : status
  ; account : string
  ; kind : posting_kind
  ; amount : posting_amount option
  ; cost : cost option
  ; lot : lot option
  ; assertion : assertion option
  ; comment : comment
  }
[@@deriving sexp, compare, equal]

let posting
      ?(status = Unmarked)
      ?(kind = Real)
      ?amount
      ?cost
      ?lot
      ?assertion
      ?(comment = no_comment)
      account
  =
  { status
  ; account
  ; kind
  ; amount = Option.map amount ~f:(fun a -> Amount a)
  ; cost
  ; lot
  ; assertion
  ; comment
  }
;;

type transaction =
  { date : Ptime.date
  ; date2 : Ptime.date option
  ; status : status
  ; code : string option
  ; description : string (* may be PAYEE | NOTE *)
  ; comment : comment
  ; postings : posting list
  }
[@@deriving sexp, compare, equal]

let transaction
      ?date2
      ?(status = Unmarked)
      ?code
      ?(description = "")
      ?(comment = no_comment)
      date
      postings
  =
  { date; date2; status; code; description; comment; postings }
;;

(* Payee and note, split at the first '|'. Without one, both are the description. *)
let payee_and_note (t : transaction) =
  match String.lsplit2 t.description ~on:'|' with
  | None -> t.description, t.description
  | Some (payee, note) -> String.strip payee, String.strip note
;;

(* Periodic transaction rule. [period] is the period expression, kept as written. *)
type periodic =
  { period : string
  ; status : status
  ; code : string option
  ; description : string
  ; comment : comment
  ; postings : posting list
  }
[@@deriving sexp, compare, equal]

(* Auto posting rule. [query] is kept as written. *)
type auto =
  { query : string
  ; comment : comment
  ; postings : posting list
  }
[@@deriving sexp, compare, equal]

type alias_pattern =
  | Exact of string
  | Regex of string (* without the enclosing slashes *)
[@@deriving sexp, compare, equal]

type directive =
  | Account of
      { name : string
      ; comment : comment
      ; subdirectives : string list (* indented non-comment lines, verbatim *)
      }
  | Commodity of
      { symbol : string
      ; sample : Amount.t option (* the display style sample, if any *)
      ; comment : comment
      ; subdirectives : string list (* other than [format], verbatim *)
      }
  | Decimal_mark of
      { mark : char
      ; comment : comment
      }
  | Include of
      { path : string
      ; comment : comment
      }
  | Payee of
      { name : string
      ; comment : comment
      }
  | Tag of
      { name : string
      ; comment : comment
      }
  | Price of
      { date : Ptime.date
      ; time : Ptime.time option
      ; commodity : string
      ; price : Amount.t
      ; comment : comment
      }
  | Alias of
      { pattern : alias_pattern
      ; replacement : string
      }
  | End_aliases
  | Apply_account of string
  | End_apply_account
  | Default_commodity of
      { sample : Amount.t
      ; comment : comment
      }
    (* D *)
  | Default_year of
      { year : int
      ; comment : comment
      }
    (* Y *)
  | Other of string list (* accepted-but-ignored Ledger directives, verbatim *)
[@@deriving sexp, compare, equal]

type item =
  | Blank
  | Comment_line of string (* a top-level line starting with '#', ';' or '*', verbatim *)
  | Comment_block of string list (* lines between comment and end comment *)
  | Transaction of transaction
  | Periodic of periodic
  | Auto of auto
  | Directive of directive
[@@deriving sexp, compare, equal]

type t = item list [@@deriving sexp, compare, equal]

let transactions t =
  List.filter_map t ~f:(function
    | Transaction tx -> Some tx
    | _ -> None)
;;
