(** Writing journal files.

    Output is valid hledger journal text that reads back to the same {!Journal.t}.
    Postings are laid out like [hledger print]: accounts indented by four spaces, amounts
    right-aligned in a column two spaces after the longest account of the entry, then cost
    basis, cost and balance assertion. Dates are written as YYYY-MM-DD. Each line ends
    with a newline.

    Strings in the tree are written as they are, so they must not break the syntax: no
    [;] in descriptions, no two consecutive blanks in account names, no newlines. *)

val journal : Journal.t -> string
val item : Journal.item -> string
val transaction : Journal.transaction -> string
