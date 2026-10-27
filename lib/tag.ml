open Base

type t = string * string [@@deriving sexp, compare, equal]

let is_blank c = Char.equal c ' ' || Char.equal c '\t'

(* What follows the last blank of [s]: empty if [s] ends with a blank, as in hledger,
   so "foo :" is not a tag. *)
let last_word s =
  Option.value (List.last (String.split_on_chars s ~on:[ ' '; '\t' ])) ~default:""
;;

(* Tags in one line of comment text, the way hledger finds them: the word just before
   each ':' is a tag name, and its value runs up to the next ',' or the end of line. This
   is loose on purpose: "see https://foo" has an "https" tag, as in hledger. *)
let of_line line =
  let n = String.length line in
  let rec go i acc =
    if i >= n
    then List.rev acc
    else (
      match String.index_from line i ':' with
      | None -> List.rev acc
      | Some c ->
        let name = last_word (String.sub line ~pos:i ~len:(c - i)) in
        if String.is_empty name
        then go (c + 1) acc
        else (
          let stop = Option.value (String.index_from line (c + 1) ',') ~default:n in
          let value = String.strip (String.sub line ~pos:(c + 1) ~len:(stop - c - 1)) in
          go (stop + 1) ((name, value) :: acc)))
  in
  go 0 []
;;

let of_comment ({ same_line; lines } : Journal.comment) =
  List.concat_map (Option.to_list same_line @ lines) ~f:of_line
;;

let find tags name = List.Assoc.find tags name ~equal:String.equal

let find_all tags name =
  List.filter_map tags ~f:(fun (n, v) -> Option.some_if (String.equal n name) v)
;;

(* Comment text declaring [tags], e.g. "trip: 2024, reimbursable:". Names must not contain
   whitespace or ':', and values must not contain ',' or newlines. *)
let to_text tags =
  List.iter tags ~f:(fun (name, value) ->
    if
      String.is_empty name
      || String.exists name ~f:(fun c ->
        is_blank c || Char.equal c ':' || Char.equal c '\n')
    then Printf.invalid_argf "Hledger.Tag.to_text: invalid tag name %S" name ();
    if String.exists value ~f:(fun c -> Char.equal c ',' || Char.equal c '\n')
    then
      Printf.invalid_argf "Hledger.Tag.to_text: invalid value %S for tag %s" value name ());
  List.map tags ~f:(fun (n, v) -> if String.is_empty v then n ^ ":" else n ^ ": " ^ v)
  |> String.concat ~sep:", "
;;
