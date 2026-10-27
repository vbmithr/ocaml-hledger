(** Syntax tree of an hledger journal file.

    It keeps what is needed to write the file back in an equivalent form: comment lines,
    blank lines, comment blocks and unrecognised directives are preserved, while layout
    inside entries (alignment, separator characters, date separators) is normalised.

    Nothing is resolved: includes are not followed, aliases not applied, missing amounts
    not inferred, transactions not balanced and balance assertions not checked. Period
    expressions and auto posting queries are kept as written. *)

open Base

type status =
  | Unmarked
  | Pending (** [!] *)
  | Cleared (** [*] *)
[@@deriving sexp, compare, equal]

(** A comment attached to an entry, posting or directive: the text after the [;] of a
    same-line comment, and of each following indented comment line, without the [;]
    and surrounding blanks. Tags are found in this text; see {!module-Tag}. *)
type comment =
  { same_line : string option
  ; lines : string list
  }
[@@deriving sexp, compare, equal]

val no_comment : comment

type cost =
  | Unit of Amount.t (** [@ UNITCOST] *)
  | Total of Amount.t (** [@@ TOTALCOST] *)
[@@deriving sexp, compare, equal]

(** A cost basis annotation, used to record an acquired lot or select a disposed one:
    hledger's [{DATE, "LABEL", COST}], every part optional, or Ledger's [{COST}] or
    [{{TOTAL}}] with [[DATE]] and [(LABEL)]. A per-unit basis is written in hledger's
    syntax, a total one in Ledger's, the only syntax that has it. *)
type lot =
  { lot_date : Ptime.date option
  ; label : string option
  ; basis : cost option
  }
[@@deriving sexp, compare, equal]

type posting_kind =
  | Real
  | Virtual (** [(account)]: not balanced *)
  | Balanced_virtual (** [[account]]: balanced separately from real postings *)
[@@deriving sexp, compare, equal]

type posting_amount =
  | Amount of Amount.t
  | Multiplier of Amount.t
  (** [*N] or [*AMOUNT]: a multiple of the matched posting's amount, only meaningful in
      auto posting rules. *)
[@@deriving sexp, compare, equal]

(** A balance assertion, [= AMOUNT] and its variants. *)
type assertion =
  { expected : Amount.t
  ; expected_cost : cost option (** allowed by hledger, ignored by the check *)
  ; sole : bool (** [==]: no other commodity in the account *)
  ; inclusive : bool (** [=*]: including subaccounts *)
  }
[@@deriving sexp, compare, equal]

(** A posting with no [amount] but an [assertion] is a balance assignment. A posting
    with neither has its amount inferred by hledger. *)
type posting =
  { status : status
  ; account : string (** without the brackets of virtual postings *)
  ; kind : posting_kind
  ; amount : posting_amount option
  ; cost : cost option
  ; lot : lot option
  ; assertion : assertion option
  ; comment : comment
  }
[@@deriving sexp, compare, equal]

val posting
  :  ?status:status
  -> ?kind:posting_kind
  -> ?amount:Amount.t
  -> ?cost:cost
  -> ?lot:lot
  -> ?assertion:assertion
  -> ?comment:comment
  -> string
  -> posting

type transaction =
  { date : Ptime.date (** [(year, month, day)], without an invented timestamp. *)
  ; date2 : Ptime.date option (** deprecated secondary date, [DATE=DATE2] *)
  ; status : status
  ; code : string option (** [(CODE)] *)
  ; description : string (** may be [PAYEE | NOTE]; see {!payee_and_note} *)
  ; comment : comment
  ; postings : posting list
  }
[@@deriving sexp, compare, equal]

val transaction
  :  ?date2:Ptime.date
  -> ?status:status
  -> ?code:string
  -> ?description:string
  -> ?comment:comment
  -> Ptime.date
  -> posting list
  -> transaction

(** Payee and note: the description split at its first [|]. Without one, both are the
    whole description, as in hledger. *)
val payee_and_note : transaction -> string * string

(** A periodic transaction rule, [~ PERIOD  DESCRIPTION]. *)
type periodic =
  { period : string (** the period expression, as written *)
  ; status : status
  ; code : string option
  ; description : string
  ; comment : comment
  ; postings : posting list
  }
[@@deriving sexp, compare, equal]

(** An auto posting rule, [= QUERY]. *)
type auto =
  { query : string (** as written *)
  ; comment : comment
  ; postings : posting list
  }
[@@deriving sexp, compare, equal]

type alias_pattern =
  | Exact of string
  | Regex of string (** without the enclosing slashes *)
[@@deriving sexp, compare, equal]

type directive =
  | Account of
      { name : string
      ; comment : comment
      ; subdirectives : string list (** indented non-comment lines, verbatim *)
      }
  | Commodity of
      { symbol : string
      ; sample : Amount.t option
        (** the display style sample, from the directive or its [format] subdirective *)
      ; comment : comment
      ; subdirectives : string list (** other than [format], verbatim *)
      }
  | Decimal_mark of
      { mark : char
      ; comment : comment
      }
  | Include of
      { path : string (** as written: may be relative, start with [~], or be a glob *)
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
        (** [((hour, minute, second), 0)]; journal price times have no timezone
            suffix, so the offset must be zero. Seconds are integral. *)
      ; commodity : string
      ; price : Amount.t
      ; comment : comment
      } (** [P DATE [TIME] COMMODITY AMOUNT] *)
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
      } (** [D AMOUNT] *)
  | Default_year of
      { year : int
      ; comment : comment
      } (** [Y YEAR] *)
  | Other of string list
  (** a Ledger directive hledger accepts and ignores, with its indented lines,
      verbatim *)
[@@deriving sexp, compare, equal]

type item =
  | Blank
  | Comment_line of string
  (** a top-level line starting with [#], [;] or [*], or an indented comment line outside
      any entry, verbatim *)
  | Comment_block of string list (** the lines between [comment] and [end comment] *)
  | Transaction of transaction
  | Periodic of periodic
  | Auto of auto
  | Directive of directive
[@@deriving sexp, compare, equal]

(** The items of one file, in order. *)
type t = item list [@@deriving sexp, compare, equal]

val transactions : t -> transaction list
