open Core

type side =
  | Left
  | Right
[@@deriving sexp, compare, equal]

(* Digit group sizes are listed from the decimal mark leftwards, the last one repeating:
   [3] is 1,000,000 and [3; 2] is 1,00,00,000. [mark] is a string because the no-break
   and thin space variants hledger accepts are multi-byte in UTF-8. *)
type digit_groups =
  { mark : string
  ; sizes : int list
  }
[@@deriving sexp, compare, equal]

type style =
  { side : side
  ; spaced : bool
  ; decimal_mark : char
  ; digit_groups : digit_groups option
  ; precision : int
  }
[@@deriving sexp, compare, equal]

type t =
  { quantity : Bigdecimal.t
  ; commodity : string
  ; style : style
  }
[@@deriving sexp, compare, equal]

(* Number of decimal digits needed to write [q] exactly. *)
let scale q = Int.max 0 (-Bigdecimal.exponent q)

(* Length in bytes of the UTF-8 sequence starting with byte [c]. *)
let utf8_len c =
  let c = Char.to_int c in
  if c >= 0xf0 then 4 else if c >= 0xe0 then 3 else if c >= 0xc0 then 2 else 1
;;

(* Like hledger: a symbol made of one non-letter character ($, €, £) goes on the left
   with no space, anything else on the right after a space. *)
let default_style ?(precision = 0) commodity =
  let side, spaced =
    if String.is_empty commodity
    then Right, false
    else if
      utf8_len commodity.[0] = String.length commodity
      && not (Char.is_alpha commodity.[0])
    then Left, false
    else Right, true
  in
  { side; spaced; decimal_mark = '.'; digit_groups = None; precision }
;;

(* Bigdecimal does not keep trailing zeros, so give [precision] to show $1500.00 rather
   than $1500. It defaults to the digits [quantity] needs. *)
let create ?style ?precision ~commodity quantity =
  let style =
    match style with
    | Some s -> s
    | None ->
      default_style
        ~precision:(Option.value precision ~default:(scale quantity))
        commodity
  in
  { quantity; commodity; style }
;;

let is_zero t = Bigdecimal.is_zero t.quantity
let neg t = { t with quantity = Bigdecimal.neg t.quantity }

(* Characters allowed in an unquoted commodity symbol: hledger's
   isNonsimpleCommodityChar, negated. *)
let is_symbol_char = function
  | '0' .. '9'
  | '-' | '+' | '.' | '@' | '*' | ';' | '\t' | '\n' | '\r' | ' ' | '"' | '{' | '}' | '='
    -> false
  | _ -> true
;;

let needs_quotes symbol =
  (not (String.is_empty symbol)) && not (String.for_all symbol ~f:is_symbol_char)
;;

let format_commodity symbol = if needs_quotes symbol then "\"" ^ symbol ^ "\"" else symbol

let group_integer_part digits = function
  | None -> digits
  | Some { mark; sizes } ->
    let rec take acc rest sizes =
      let size, sizes =
        match sizes with
        | [] -> String.length rest, []
        | [ s ] -> s, [ s ]
        | s :: tl -> s, tl
      in
      let n = String.length rest in
      if size <= 0 || n <= size
      then rest :: acc
      else take (String.suffix rest size :: acc) (String.prefix rest (n - size)) sizes
    in
    String.concat ~sep:mark (take [] digits sizes)
;;

(* The number without sign, with at least [style.precision] decimal digits. Digits are
   never dropped: an amount more precise than its style is shown in full. *)
let format_number ?(force_decimal_mark = false) style q =
  let int_part, frac =
    match String.lsplit2 (Bigdecimal.to_string_no_sn (Bigdecimal.abs q)) ~on:'.' with
    | None -> Bigdecimal.to_string_no_sn (Bigdecimal.abs q), ""
    | Some (i, f) -> i, f
  in
  let frac =
    if String.length frac < style.precision
    then frac ^ String.make (style.precision - String.length frac) '0'
    else frac
  in
  let grouped = group_integer_part int_part style.digit_groups in
  let mark = String.of_char style.decimal_mark in
  if not (String.is_empty frac)
  then grouped ^ mark ^ frac
  else (
    match style.digit_groups with
    | Some { mark = "." | ","; _ } when String.length grouped > String.length int_part ->
      (* A '.' or ',' group mark could read back as a decimal mark (1,000 is one), so
         end with a decimal mark to disambiguate, as hledger does. *)
      grouped ^ mark
    | _ -> if force_decimal_mark then grouped ^ mark else grouped)
;;

(* [force_decimal_mark] always writes a decimal mark, as commodity directive samples
   need: "1000." rather than "1000". *)
let to_string ?force_decimal_mark { quantity; commodity; style } =
  let sign = if Sign.equal (Bigdecimal.sign quantity) Neg then "-" else "" in
  let number = format_number ?force_decimal_mark style quantity in
  let symbol = format_commodity commodity in
  let space = if style.spaced && not (String.is_empty symbol) then " " else "" in
  match style.side with
  | Left -> symbol ^ space ^ sign ^ number
  | Right -> sign ^ number ^ space ^ symbol
;;

(* ---------------------------------------------------------------------------------- *)
(* Scanning *)

type decimal_mark_hint = string -> char option

let is_blank c = Char.equal c ' ' || Char.equal c '\t'

let skip_blanks s i =
  let n = String.length s in
  let rec go i = if i < n && is_blank s.[i] then go (i + 1) else i in
  go i
;;

(* Length of a Unicode space accepted as a digit group mark at [i], or 0. *)
let unicode_space_len s i =
  let n = String.length s in
  let at k c = i + k < n && Char.equal s.[i + k] c in
  if at 0 '\xc2' && at 1 '\xa0'
  then 2
  else if at 0 '\xe2' && at 1 '\x80' && (at 2 '\xaf' || at 2 '\x89' || at 2 '\x8a')
  then 3
  else 0
;;

let is_dot_or_comma c = Char.equal c '.' || Char.equal c ','

(* Raw number: runs of digits separated by single separator marks. *)
type raw_number =
  { runs : string list
  ; seps : string list
  ; exponent : int
  }

let scan_raw_number s i =
  let n = String.length s in
  let rec digits j = if j < n && Char.is_digit s.[j] then digits (j + 1) else j in
  let sep_len j =
    if j >= n
    then 0
    else (
      match s.[j] with
      | '.' | ',' | '_' | '\'' | ' ' -> 1
      | _ -> unicode_space_len s j)
  in
  let rec loop j runs seps =
    let k = digits j in
    let runs = String.sub s ~pos:j ~len:(k - j) :: runs in
    let l = sep_len k in
    if l > 0 && k + l < n && Char.is_digit s.[k + l]
    then loop (k + l) runs (String.sub s ~pos:k ~len:l :: seps)
    else if k < n && is_dot_or_comma s.[k]
    then (* trailing decimal mark, as in "10." *)
      k + 1, List.rev ("" :: runs), List.rev (String.of_char s.[k] :: seps)
    else k, List.rev runs, List.rev seps
  in
  let starts_with_digit = i < n && Char.is_digit s.[i] in
  let lead_mark = i + 1 < n && is_dot_or_comma s.[i] && Char.is_digit s.[i + 1] in
  if not (starts_with_digit || lead_mark)
  then None
  else (
    let j, runs, seps =
      if lead_mark
      then (
        let j, runs, seps = loop (i + 1) [] [] in
        j, "" :: runs, String.of_char s.[i] :: seps)
      else loop i [] []
    in
    (* Scientific notation: E or e followed by an optionally signed integer. *)
    let j, exponent =
      if j < n && Char.equal (Char.uppercase s.[j]) 'E'
      then (
        let signed =
          j + 1 < n && (Char.equal s.[j + 1] '-' || Char.equal s.[j + 1] '+')
        in
        let k = if signed then j + 2 else j + 1 in
        let m = digits k in
        if m = k
        then j, 0
        else (
          let e = Int.of_string (String.sub s ~pos:k ~len:(m - k)) in
          m, if signed && Char.equal s.[j + 1] '-' then -e else e))
      else j, 0
    in
    Some ({ runs; seps; exponent }, j))
;;

type number =
  { value : Bigdecimal.t
  ; mark : char option
  ; groups : digit_groups option
  ; decimals : int
  }

(* Decides which separator, if any, is the decimal mark. With no hint, a '.' or ','
   that is the last separator and occurs only once is the decimal mark (so an ambiguous
   1,000 is one, like hledger). *)
let interpret_number ~hint { runs; seps; exponent } =
  let open Or_error.Let_syntax in
  let count m = List.count seps ~f:(String.equal m) in
  let%bind decimal =
    match hint, List.last seps with
    | _, None -> return None
    | Some d, Some l ->
      let d = String.of_char d in
      if count d = 0
      then return None
      else if count d = 1 && String.equal l d
      then return (Some d)
      else Or_error.errorf "number does not match decimal mark %S" d
    | None, Some l ->
      return (if is_dot_or_comma l.[0] && count l = 1 then Some l else None)
  in
  let int_runs, frac, group_seps =
    match decimal with
    | None -> runs, "", seps
    | Some _ -> List.drop_last_exn runs, List.last_exn runs, List.drop_last_exn seps
  in
  let%bind group_mark =
    match group_seps with
    | [] -> return None
    | m :: rest ->
      if List.for_all rest ~f:(String.equal m)
      then return (Some m)
      else Or_error.error_string "inconsistent digit group marks"
  in
  let%bind () =
    match decimal, group_mark with
    | Some d, Some g when String.equal d g ->
      Or_error.error_string "digit group mark is also the decimal mark"
    | _ -> return ()
  in
  let groups =
    Option.map group_mark ~f:(fun mark ->
      (* Sizes right to left, with the repeating tail collapsed. *)
      let rec drop_repeats = function
        | a :: (b :: _ as tl) when a = b -> drop_repeats tl
        | l -> l
      in
      let sizes = List.map (List.tl_exn int_runs) ~f:String.length |> drop_repeats in
      { mark; sizes = List.rev sizes })
  in
  let digits = String.concat int_runs ^ frac in
  let digits = if String.is_empty digits then "0" else digits in
  let value =
    Bigdecimal.scale_by
      (Bigdecimal.of_string digits)
      ~power_of_ten:(exponent - String.length frac)
  in
  return
    { value
    ; mark = Option.map decimal ~f:(fun d -> d.[0])
    ; groups
    ; decimals = Int.max 0 (String.length frac - exponent)
    }
;;

(* A commodity symbol at [i], quoted or bare. *)
let scan_symbol s i =
  let n = String.length s in
  if i < n && Char.equal s.[i] '"'
  then (
    match String.index_from s (i + 1) '"' with
    | None -> Or_error.error_string "unterminated quoted commodity symbol"
    | Some j -> Ok (Some (String.sub s ~pos:(i + 1) ~len:(j - i - 1), j + 1)))
  else (
    let rec go j =
      if j < n && is_symbol_char s.[j] && unicode_space_len s j = 0 then go (j + 1) else j
    in
    let j = go i in
    Ok (if j > i then Some (String.sub s ~pos:i ~len:(j - i), j) else None))
;;

let scan_sign s i =
  if i < String.length s && (Char.equal s.[i] '-' || Char.equal s.[i] '+')
  then Char.equal s.[i] '-', skip_blanks s (i + 1)
  else false, i
;;

let make ~hint ~negative ~commodity ~side ~spaced raw stop =
  let open Or_error.Let_syntax in
  let%map num = interpret_number ~hint:(hint commodity) raw in
  let decimal_mark =
    match num.mark, hint commodity with
    | Some d, _ | None, Some d -> d
    | None, None -> '.'
  in
  let quantity = if negative then Bigdecimal.neg num.value else num.value in
  let style =
    { side; spaced; decimal_mark; digit_groups = num.groups; precision = num.decimals }
  in
  { quantity; commodity; style }, stop
;;

(* Scans an amount starting at [i]: optional sign, then either a left-side commodity
   symbol followed by a number, or a number optionally followed by a right-side symbol.
   [hint] gives the decimal mark known for a commodity, if any. Returns the amount and
   the index just after it. *)
let scan ?(hint = fun _ -> None) s i =
  let open Or_error.Let_syntax in
  let negative1, i = scan_sign s i in
  match%bind scan_symbol s i with
  | Some (commodity, j) ->
    let k = skip_blanks s j in
    let spaced = k > j in
    let negative2, k = scan_sign s k in
    (match scan_raw_number s k with
     | None -> Or_error.error_string "expected a number after commodity symbol"
     | Some (raw, stop) ->
       make
         ~hint
         ~negative:(Bool.( <> ) negative1 negative2)
         ~commodity
         ~side:Left
         ~spaced
         raw
         stop)
  | None ->
    (match scan_raw_number s i with
     | None -> Or_error.error_string "expected an amount"
     | Some (raw, j) ->
       let k = skip_blanks s j in
       (match%bind scan_symbol s k with
        | Some (commodity, stop) ->
          make ~hint ~negative:negative1 ~commodity ~side:Right ~spaced:(k > j) raw stop
        | None ->
          make ~hint ~negative:negative1 ~commodity:"" ~side:Right ~spaced:false raw j))
;;

let of_string ?hint s =
  let s = String.strip s in
  match scan ?hint s 0 with
  | Ok (t, j) when j = String.length s -> Ok t
  | Ok _ -> Or_error.errorf "trailing characters in amount %S" s
  | Error _ as e -> e
;;

let of_string_exn ?hint s = Or_error.ok_exn (of_string ?hint s)

module Private = struct
  let is_blank = is_blank
  let skip_blanks = skip_blanks
  let is_dot_or_comma = is_dot_or_comma
  let scan = scan
  let scan_symbol = scan_symbol
end
