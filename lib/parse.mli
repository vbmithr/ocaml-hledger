(** Reading journal files. *)

open Base

(** Parses the contents of one journal file. [file] names it in error messages, which
    look like ["FILE:LINE: message"].

    Yearless dates take their year from the last [Y] directive above them, or else from
    [default_year]; without either they are an error. hledger uses the current year
    there, and callers who want that must pass it. [decimal-mark], [commodity] and [D]
    directives above an amount decide how its ambiguous numbers are read, as in hledger.

    Include directives are not followed. *)
val journal : ?file:string -> ?default_year:int -> string -> Journal.t Or_error.t

val journal_exn : ?file:string -> ?default_year:int -> string -> Journal.t
