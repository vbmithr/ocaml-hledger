(** Single-commodity amounts, with the display style they were written in.

    The quantity is exact. Since [Bigdecimal] does not keep trailing zeros, the number
    of decimal digits to show is part of the {!type-style}, as in hledger. *)

open Core

type side =
  | Left (** [$1] *)
  | Right (** [1 EUR] *)
[@@deriving sexp, compare, equal]

(** Digit grouping of the integer part. [sizes] are listed from the decimal mark
    leftwards, the last one repeating: [[3]] is 1,000,000 and [[3; 2]] is 1,00,00,000.
    [mark] is ",", ".", "_", "'", or a space (ASCII, no-break or thin, in UTF-8). *)
type digit_groups =
  { mark : string
  ; sizes : int list
  }
[@@deriving sexp, compare, equal]

type style =
  { side : side (** Which side of the number the commodity symbol goes. *)
  ; spaced : bool (** Whether a space separates symbol and number. *)
  ; decimal_mark : char (** ['.'] or [','] *)
  ; digit_groups : digit_groups option
  ; precision : int (** Minimum number of decimal digits shown. *)
  }
[@@deriving sexp, compare, equal]

type t =
  { quantity : Bigdecimal.t
  ; commodity : string (** Unquoted; [""] for the no-symbol commodity. *)
  ; style : style
  }
[@@deriving sexp, compare, equal]

(** hledger's default style for a commodity: a symbol of one non-letter character
    ([$], [€]) goes on the left with no space, anything else on the right after a
    space. [precision] defaults to 0. *)
val default_style : ?precision:int -> string -> style

(** An amount in [commodity]. Without [style], uses {!default_style}, with [precision]
    defaulting to the number of decimal digits [quantity] needs. Pass [~precision:2] to
    write [$1500.00] rather than [$1500]. *)
val create : ?style:style -> ?precision:int -> commodity:string -> Bigdecimal.t -> t

val is_zero : t -> bool
val neg : t -> t

(** The commodity symbol as written in a journal: double-quoted when it contains a
    character that hledger does not allow in a bare symbol: a digit, a blank, a double
    quote, or one of [- + . @ * ; { } =]. *)
val format_commodity : string -> string

(** The amount as written in a journal, e.g. [$-1,000.50] or [3 "green apples"]. Digits
    are never dropped: a quantity more precise than its style is shown in full. When
    there are no decimal digits, a trailing decimal mark is added if the number has
    ['.'] or [','] digit group marks (as hledger does, so that [1,000.] does not read
    back as one), or always if [force_decimal_mark] is set, as commodity directive
    samples need. *)
val to_string : ?force_decimal_mark:bool -> t -> string

(** The decimal mark known for a commodity, if any. In a journal it comes from
    [decimal-mark], [commodity] and [D] directives. *)
type decimal_mark_hint = string -> char option

(** Parses an amount in any of the forms hledger accepts: sign before or after a left
    symbol, quoted symbols, digit group marks, scientific notation.

    A number with a single ['.'] or [','] and no hint, like [1,000], is ambiguous; it is
    read as a decimal mark (so [1,000] is one), like hledger does. With a hint, the
    number must use that decimal mark, and the other one is a digit group mark. *)
val of_string : ?hint:decimal_mark_hint -> string -> t Or_error.t

val of_string_exn : ?hint:decimal_mark_hint -> string -> t

(** Scanning primitives shared with {!Parse}. Not part of the public interface. *)
module Private : sig
  val is_blank : char -> bool
  val skip_blanks : string -> int -> int
  val is_dot_or_comma : char -> bool

  (** [scan s i] scans an amount starting at [i], returning it and the index after it. *)
  val scan : ?hint:decimal_mark_hint -> string -> int -> (t * int) Or_error.t

  (** A quoted or bare commodity symbol at [i], if there is one. *)
  val scan_symbol : string -> int -> (string * int) option Or_error.t
end
