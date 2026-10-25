(** Tags: [name:value] pairs written inside comments. *)

(** Name and value; the value is [""] for a tag written as [name:]. *)
type t = string * string [@@deriving sexp, compare, equal]

(** Tags in one line of comment text, found the way hledger finds them: the word just
    before each [:] is a tag name, and its value runs up to the next [,] or the end of
    the line, without surrounding blanks. This is loose on purpose, as in hledger:
    ["see https://foo"] has an [https] tag. *)
val of_line : string -> t list

(** Tags of all the lines of a comment, in order. Tags inherited from accounts,
    commodities or the enclosing transaction are not included. *)
val of_comment : Journal.comment -> t list

(** The value of the first tag named [name]. *)
val find : t list -> string -> string option

(** The values of all tags named [name], since a tag can be repeated. *)
val find_all : t list -> string -> string list

(** Comment text declaring [tags], e.g. ["trip: rome, reimbursable:"].

    Raises [Invalid_argument] if a name is empty or contains a blank, [:] or newline, or a
    value contains [,] or a newline, which could not be read back. *)
val to_text : t list -> string
