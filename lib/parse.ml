open Core
open Journal

(* Parse-time state that changes how later lines are read. *)
type state =
  { mutable year : int option (* Y directive *)
  ; mutable decimal_mark : char option (* decimal-mark directive *)
  ; commodity_marks : char String.Table.t (* from commodity and D samples *)
  }

let hint st commodity =
  match st.decimal_mark with
  | Some _ as d -> d
  | None -> Hashtbl.find st.commodity_marks commodity
;;

let is_blank = Amount.Private.is_blank
let skip_blanks = Amount.Private.skip_blanks
let is_blank_line l = String.for_all l ~f:Char.is_whitespace
let is_indented l = (not (String.is_empty l)) && is_blank l.[0]
let error fmt = Printf.ksprintf Or_error.error_string fmt

(* Prefixes an error with its location. *)
let at ~file number r =
  Result.map_error r ~f:(fun e ->
    Error.of_string (sprintf "%s:%d: %s" file number (Error.to_string_hum e)))
;;

(* Index of the first run of two or more blanks at or after [i], or the length of [s].
   This is how hledger ends account names, payee names and period expressions. *)
let find_separator s i =
  let n = String.length s in
  let rec go k =
    if k + 1 >= n
    then n
    else if is_blank s.[k] && is_blank s.[k + 1]
    then k
    else go (k + 1)
  in
  go i
;;

(* Same-line comment: the text after ';', or an error if something else is left. *)
let trailing_comment s i =
  let i = skip_blanks s i in
  if i >= String.length s
  then Ok None
  else if Char.equal s.[i] ';'
  then Ok (Some (String.strip (String.drop_prefix s (i + 1))))
  else error "unexpected text %S" (String.drop_prefix s i)
;;

let parse_date ?default_year s =
  let fields =
    match
      String.find s ~f:(fun c -> Char.equal c '-' || Char.equal c '/' || Char.equal c '.')
    with
    | None -> [ s ]
    | Some sep -> String.split s ~on:sep
  in
  let number f = (not (String.is_empty f)) && String.for_all f ~f:Char.is_digit in
  let make y m d =
    Or_error.try_with (fun () ->
      Date.create_exn
        ~y:(Int.of_string y)
        ~m:(Month.of_int_exn (Int.of_string m))
        ~d:(Int.of_string d))
    |> Result.map_error ~f:(fun _ -> Error.of_string (sprintf "invalid date %S" s))
  in
  match fields with
  | fs when not (List.for_all fs ~f:number) -> error "invalid date %S" s
  | [ y; m; d ] -> make y m d
  | [ m; d ] ->
    (match default_year with
     | Some y -> make (Int.to_string y) m d
     | None -> error "date %S has no year, and no Y directive or default year is set" s)
  | _ -> error "invalid date %S" s
;;

(* A date at [i], made of digits and separators. *)
let scan_date ?default_year s i =
  let n = String.length s in
  let rec go j =
    if
      j < n
      && (Char.is_digit s.[j]
          || Char.equal s.[j] '-'
          || Char.equal s.[j] '/'
          || Char.equal s.[j] '.')
    then go (j + 1)
    else j
  in
  let j = go i in
  if j = i
  then error "expected a date"
  else
    Or_error.map
      (parse_date ?default_year (String.sub s ~pos:i ~len:(j - i)))
      ~f:(fun d -> d, j)
;;

let scan_status s i =
  if i < String.length s
  then (
    match s.[i] with
    | '*' -> Cleared, skip_blanks s (i + 1)
    | '!' -> Pending, skip_blanks s (i + 1)
    | _ -> Unmarked, i)
  else Unmarked, i
;;

let scan_code s i =
  if i < String.length s && Char.equal s.[i] '('
  then (
    match String.index_from s (i + 1) ')' with
    | None -> error "unterminated transaction code"
    | Some j ->
      Ok (Some (String.sub s ~pos:(i + 1) ~len:(j - i - 1)), skip_blanks s (j + 1)))
  else Ok (None, i)
;;

(* Status, code, description and same-line comment, as in transaction headers. *)
let scan_header_tail s i =
  let open Or_error.Let_syntax in
  let status, i = scan_status s (skip_blanks s i) in
  let%map code, i = scan_code s i in
  let rest = String.drop_prefix s i in
  let description, comment =
    match String.lsplit2 rest ~on:';' with
    | None -> String.strip rest, None
    | Some (d, c) -> String.strip d, Some (String.strip c)
  in
  status, code, description, comment
;;

(* ---------------------------------------------------------------------------------- *)
(* Postings *)

let scan_amount st s i = Amount.Private.scan ~hint:(hint st) s i
let amount_of_string st s = Amount.of_string ~hint:(hint st) s

(* hledger's {DATE, "LABEL", COST}, every part optional, or Ledger's {COST} / {=COST}. *)
let parse_lot_body st body =
  let open Or_error.Let_syntax in
  let n = String.length body in
  let skip_comma i =
    let i = skip_blanks body i in
    if i < n && Char.equal body.[i] ',' then skip_blanks body (i + 1) else i
  in
  let i = skip_blanks body 0 in
  let%bind lot_date, i =
    let looks_like_date =
      i + 4 < n
      && String.for_all (String.sub body ~pos:i ~len:4) ~f:Char.is_digit
      && Char.equal body.[i + 4] '-'
    in
    if looks_like_date
    then (
      let%map d, j = scan_date body i in
      Some d, skip_comma j)
    else return (None, i)
  in
  let%bind label, i =
    if i < n && Char.equal body.[i] '"'
    then (
      match String.index_from body (i + 1) '"' with
      | None -> error "unterminated lot label"
      | Some j ->
        return (Some (String.sub body ~pos:(i + 1) ~len:(j - i - 1)), skip_comma (j + 1)))
    else return (None, i)
  in
  let rest = String.strip (String.drop_prefix body i) in
  let rest = String.chop_prefix_if_exists rest ~prefix:"=" in
  let%map basis =
    if String.is_empty rest
    then return None
    else Or_error.map (amount_of_string st rest) ~f:(fun a -> Some (Unit a))
  in
  { lot_date; label; basis }
;;

(* A {...} or {{...}} annotation at [i]. *)
let scan_lot st s i =
  let open Or_error.Let_syntax in
  if i + 1 < String.length s && Char.equal s.[i + 1] '{'
  then (
    match String.substr_index s ~pos:i ~pattern:"}}" with
    | None -> error "unterminated {{ cost basis"
    | Some j ->
      let body = String.sub s ~pos:(i + 2) ~len:(j - i - 2) |> String.strip in
      let%map a = amount_of_string st (String.chop_prefix_if_exists body ~prefix:"=") in
      { lot_date = None; label = None; basis = Some (Total a) }, j + 2)
  else (
    let n = String.length s in
    let rec close j quoted =
      if j >= n
      then None
      else (
        match s.[j] with
        | '"' -> close (j + 1) (not quoted)
        | '}' when not quoted -> Some j
        | _ -> close (j + 1) quoted)
    in
    match close (i + 1) false with
    | None -> error "unterminated { cost basis"
    | Some j ->
      let%map lot = parse_lot_body st (String.sub s ~pos:(i + 1) ~len:(j - i - 1)) in
      lot, j + 1)
;;

let scan_cost st s i =
  let open Or_error.Let_syntax in
  (* [i] is at '@'. *)
  let total = i + 1 < String.length s && Char.equal s.[i + 1] '@' in
  let j = skip_blanks s (if total then i + 2 else i + 1) in
  let%map a, j = scan_amount st s j in
  (if total then Total a else Unit a), j
;;

type tail =
  { amount : posting_amount option
  ; cost : cost option
  ; lot : lot option
  ; assertion : assertion option
  ; comment : string option
  }

(* Everything after the account name of a posting. *)
let parse_posting_tail st s i =
  let open Or_error.Let_syntax in
  let n = String.length s in
  let peek i = if i < n then Some s.[i] else None in
  let i = skip_blanks s i in
  let%bind amount, i =
    match peek i with
    | None | Some (';' | '=') -> return (None, i)
    | Some '*' ->
      let%map a, j = scan_amount st s (skip_blanks s (i + 1)) in
      Some (Multiplier a), j
    | Some _ ->
      let%map a, j = scan_amount st s i in
      Some (Amount a), j
  in
  let set_once what current v =
    match current with
    | None -> return (Some v)
    | Some _ -> error "duplicate %s" what
  in
  let merge_lot (lot : lot option) (l : lot) =
    let base =
      Option.value lot ~default:{ lot_date = None; label = None; basis = None }
    in
    let pick what a b =
      match a, b with
      | Some _, Some _ -> error "duplicate lot %s" what
      | Some _, None -> return a
      | None, _ -> return b
    in
    let%bind lot_date = pick "date" base.lot_date l.lot_date in
    let%bind label = pick "label" base.label l.label in
    let%map basis = pick "cost basis" base.basis l.basis in
    Some { lot_date; label; basis }
  in
  (* Cost and cost basis annotations, in any order. *)
  let rec annotations i ~cost ~lot =
    let i = skip_blanks s i in
    match peek i, peek (i + 1) with
    | Some '{', _ when Option.is_some amount ->
      let%bind l, j = scan_lot st s i in
      let%bind lot = merge_lot lot l in
      annotations j ~cost ~lot
    | Some '@', _ when Option.is_some amount ->
      let%bind c, j = scan_cost st s i in
      let%bind cost = set_once "cost" cost c in
      annotations j ~cost ~lot
    | Some '(', Some '@' when Option.is_some amount ->
      (* Ledger's virtual costs (@) and (@@), same as @ and @@ in hledger. *)
      let total = i + 2 < n && Char.equal s.[i + 2] '@' in
      let close = if total then i + 3 else i + 2 in
      if not (close < n && Char.equal s.[close] ')')
      then error "malformed (@) cost"
      else (
        let%bind a, j = scan_amount st s (skip_blanks s (close + 1)) in
        let%bind cost = set_once "cost" cost (if total then Total a else Unit a) in
        annotations j ~cost ~lot)
    | Some '[', _ when Option.is_some amount ->
      (match String.index_from s i ']' with
       | None -> error "unterminated [ lot date"
       | Some j ->
         let%bind d = parse_date (String.sub s ~pos:(i + 1) ~len:(j - i - 1)) in
         let%bind lot = merge_lot lot { lot_date = Some d; label = None; basis = None } in
         annotations (j + 1) ~cost ~lot)
    | Some '(', _ when Option.is_some amount ->
      (match String.index_from s i ')' with
       | None -> error "unterminated ( lot note"
       | Some j ->
         let note = String.sub s ~pos:(i + 1) ~len:(j - i - 1) |> String.strip in
         let note = String.strip ~drop:(Char.equal '"') note in
         let%bind lot =
           merge_lot lot { lot_date = None; label = Some note; basis = None }
         in
         annotations (j + 1) ~cost ~lot)
    | _ -> return (i, cost, lot)
  in
  let%bind i, cost, lot = annotations i ~cost:None ~lot:None in
  let%bind assertion, i =
    if i < n && Char.equal s.[i] '='
    then (
      let sole = i + 1 < n && Char.equal s.[i + 1] '=' in
      let j = if sole then i + 2 else i + 1 in
      let inclusive = j < n && Char.equal s.[j] '*' in
      let j = skip_blanks s (if inclusive then j + 1 else j) in
      let%bind expected, j = scan_amount st s j in
      let j = skip_blanks s j in
      let%map expected_cost, j =
        if j < n && Char.equal s.[j] '@'
        then Or_error.map (scan_cost st s j) ~f:(fun (c, j) -> Some c, j)
        else return (None, j)
      in
      Some { expected; expected_cost; sole; inclusive }, j)
    else return (None, i)
  in
  let%map comment = trailing_comment s i in
  { amount; cost; lot; assertion; comment }
;;

(* A posting line, with its leading indentation removed. *)
let parse_posting st s =
  let open Or_error.Let_syntax in
  let status, i =
    if
      String.length s > 1
      && (Char.equal s.[0] '*' || Char.equal s.[0] '!')
      && is_blank s.[1]
    then fst (scan_status s 0), skip_blanks s 1
    else Unmarked, 0
  in
  let stop = find_separator s i in
  let raw = String.strip (String.sub s ~pos:i ~len:(stop - i)) in
  let enclosed o c =
    String.length raw >= 2
    && Char.equal raw.[0] o
    && Char.equal raw.[String.length raw - 1] c
  in
  let kind, account =
    if enclosed '(' ')'
    then Virtual, String.sub raw ~pos:1 ~len:(String.length raw - 2)
    else if enclosed '[' ']'
    then Balanced_virtual, String.sub raw ~pos:1 ~len:(String.length raw - 2)
    else Real, raw
  in
  if String.is_empty account
  then error "missing account name"
  else (
    let%map { amount; cost; lot; assertion; comment } = parse_posting_tail st s stop in
    { status
    ; account
    ; kind
    ; amount
    ; cost
    ; lot
    ; assertion
    ; comment = { same_line = comment; lines = [] }
    })
;;

(* ---------------------------------------------------------------------------------- *)
(* Entries and directives *)

type line =
  { number : int (* 1-based *)
  ; text : string
  }

(* Text of an indented comment line, given without its indentation. Inside entries and
   directives only ';' starts a comment, as in hledger. *)
let comment_line_text text =
  if String.is_prefix text ~prefix:";"
  then Some (String.strip (String.drop_prefix text 1))
  else None
;;

let add_comment_line (c : comment) text = { c with lines = c.lines @ [ text ] }

(* Postings and comment lines of an entry. Comment lines before the first posting belong
   to the entry, later ones to the posting above them. *)
let parse_body st ~file (header_comment : comment) body =
  let at number r = at ~file number r in
  let open Or_error.Let_syntax in
  let%map comment, postings =
    List.fold_result body ~init:(header_comment, []) ~f:(fun (comment, postings) l ->
      let text = String.lstrip l.text in
      match comment_line_text text, postings with
      | Some c, [] -> return (add_comment_line comment c, [])
      | Some c, (p : posting) :: rest ->
        return (comment, { p with comment = add_comment_line p.comment c } :: rest)
      | None, _ ->
        let%map p = at l.number (parse_posting st text) in
        comment, p :: postings)
  in
  comment, List.rev postings
;;

let parse_transaction st ~default_year ~file header body =
  let at number r = at ~file number r in
  let open Or_error.Let_syntax in
  let default_year = Option.first_some st.year default_year in
  let%bind date, i = at header.number (scan_date ?default_year header.text 0) in
  let%bind date2, i =
    if i < String.length header.text && Char.equal header.text.[i] '='
    then
      at
        header.number
        (Or_error.map
           (scan_date ~default_year:(Date.year date) header.text (i + 1))
           ~f:(fun (d, j) -> Some d, j))
    else return (None, i)
  in
  let%bind status, code, description, same_line =
    at header.number (scan_header_tail header.text i)
  in
  let%map comment, postings = parse_body st ~file { same_line; lines = [] } body in
  ({ date; date2; status; code; description; comment; postings } : transaction)
;;

let parse_periodic st ~file header body =
  let at number r = at ~file number r in
  let open Or_error.Let_syntax in
  let s = header.text in
  let i = skip_blanks s 1 in
  let stop =
    Int.min
      (find_separator s i)
      (Option.value (String.index_from s i ';') ~default:(String.length s))
  in
  let period = String.strip (String.sub s ~pos:i ~len:(stop - i)) in
  let%bind status, code, description, same_line =
    at header.number (scan_header_tail s stop)
  in
  let%map comment, postings = parse_body st ~file { same_line; lines = [] } body in
  { period; status; code; description; comment; postings }
;;

let parse_auto st ~file header body =
  let open Or_error.Let_syntax in
  let rest = String.drop_prefix header.text 1 in
  let query, same_line =
    match String.lsplit2 rest ~on:';' with
    | None -> String.strip rest, None
    | Some (q, c) -> String.strip q, Some (String.strip c)
  in
  let%map comment, postings = parse_body st ~file { same_line; lines = [] } body in
  { query; comment; postings }
;;

(* NAME, then optionally two blanks and a comment: account, payee and tag directives. *)
let name_and_comment rest =
  let open Or_error.Let_syntax in
  let stop = find_separator rest 0 in
  let name = String.strip (String.sub rest ~pos:0 ~len:stop) in
  let%map comment = trailing_comment rest stop in
  name, comment
;;

(* Comment lines and other indented lines below a directive. *)
let split_directive_body body =
  List.partition_map body ~f:(fun l ->
    let text = String.strip l.text in
    match comment_line_text text with
    | Some c -> First c
    | None -> Second text)
;;

let ledger_directives =
  [ "assert"
  ; "bucket"
  ; "A"
  ; "capture"
  ; "check"
  ; "define"
  ; "eval"
  ; "expr"
  ; "python"
  ; "value"
  ]
;;

let parse_alias rest =
  let rest = String.strip rest in
  let split_at k =
    match String.index_from rest k '=' with
    | None -> error "alias directive needs OLD = NEW"
    | Some e ->
      Ok
        ( String.strip (String.prefix rest e)
        , String.lstrip (String.drop_prefix rest (e + 1)) )
  in
  let open Or_error.Let_syntax in
  if String.is_prefix rest ~prefix:"/"
  then (
    (* Find the closing slash, skipping escaped ones. *)
    let n = String.length rest in
    let rec close j =
      if j >= n
      then None
      else if Char.equal rest.[j] '\\'
      then close (j + 2)
      else if Char.equal rest.[j] '/'
      then Some j
      else close (j + 1)
    in
    match close 1 with
    | None -> error "unterminated alias regex"
    | Some j ->
      let%map _, replacement = split_at (j + 1) in
      Alias { pattern = Regex (String.sub rest ~pos:1 ~len:(j - 1)); replacement })
  else (
    let%map old, replacement = split_at 0 in
    Alias { pattern = Exact old; replacement })
;;

(* Strips a comment preceded by blanks from the end of a directive argument. *)
let strip_comment rest =
  let n = String.length rest in
  let rec go i =
    if i >= n
    then String.strip rest, None
    else if Char.equal rest.[i] ';' && (i = 0 || is_blank rest.[i - 1])
    then
      ( String.strip (String.prefix rest i)
      , Some (String.strip (String.drop_prefix rest (i + 1))) )
    else go (i + 1)
  in
  go 0
;;

let register_mark st (sample : Amount.t) raw =
  (* Only a sample that writes a decimal mark tells us something. *)
  if String.exists raw ~f:Amount.Private.is_dot_or_comma
  then
    Hashtbl.set st.commodity_marks ~key:sample.commodity ~data:sample.style.decimal_mark
;;

let parse_commodity st rest body =
  let open Or_error.Let_syntax in
  let rest = String.strip rest in
  let%bind symbol, sample, comment =
    match Amount.Private.scan rest 0 with
    | Ok (a, j) ->
      let%map comment = trailing_comment rest j in
      register_mark st a (String.prefix rest j);
      a.commodity, Some a, comment
    | Error _ ->
      let%bind sym = Amount.Private.scan_symbol rest 0 in
      (match sym with
       | Some (symbol, j) ->
         let%map comment = trailing_comment rest j in
         symbol, None, comment
       | None -> error "commodity directive needs a symbol or sample amount")
  in
  let lines, others = split_directive_body body in
  (* Ledger's indented "format SAMPLE" sets the display style. *)
  let%map sample, subdirectives =
    List.fold_result others ~init:(sample, []) ~f:(fun (sample, subs) text ->
      match String.lsplit2 text ~on:' ' with
      | Some ("format", fmt) ->
        let fmt = String.strip fmt in
        let%bind a = Amount.of_string fmt in
        if not (String.equal a.commodity symbol)
        then error "commodity format %S does not match symbol %S" fmt symbol
        else (
          register_mark st a fmt;
          return (Some a, subs))
      | _ -> return (sample, subs @ [ text ]))
  in
  Commodity { symbol; sample; comment = { same_line = comment; lines }; subdirectives }
;;

let parse_price st ~default_year rest =
  let open Or_error.Let_syntax in
  let rest = String.strip rest in
  let%bind date, i = scan_date ?default_year rest 0 in
  let i = skip_blanks rest i in
  (* An optional time of day, HH:MM[:SS]. *)
  let%bind time, i =
    let stop =
      Option.value (String.index_from rest i ' ') ~default:(String.length rest)
    in
    let word = String.sub rest ~pos:i ~len:(stop - i) in
    if
      String.length word >= 4
      && String.contains word ':'
      && String.for_all word ~f:(fun c -> Char.is_digit c || Char.equal c ':')
    then (
      match Or_error.try_with (fun () -> Time_ns.Ofday.of_string word) with
      | Ok t -> return (Some t, skip_blanks rest stop)
      | Error _ -> error "invalid time %S" word)
    else return (None, i)
  in
  let%bind commodity, i =
    match%bind Amount.Private.scan_symbol rest i with
    | Some (c, j) -> return (c, skip_blanks rest j)
    | None -> error "P directive needs a commodity symbol"
  in
  let%bind price, i = scan_amount st rest i in
  let%map comment = trailing_comment rest i in
  Price { date; time; commodity; price; comment = { same_line = comment; lines = [] } }
;;

(* The argument of Y, year or apply year. *)
let parse_year st arg =
  match strip_comment arg with
  | y, same_line ->
    (match Int.of_string_opt y with
     | Some year ->
       st.year <- Some year;
       Ok (Default_year { year; comment = { same_line; lines = [] } })
     | None -> error "invalid year %S" y)
;;

(* A directive header line (in column 0) and its indented body lines. *)
let parse_directive st ~default_year (header : line) body =
  let open Or_error.Let_syntax in
  let text = header.text in
  let keyword, rest =
    match String.lsplit2 text ~on:' ' with
    | Some (k, r) -> k, r
    | None ->
      (match String.lsplit2 text ~on:'\t' with
       | Some (k, r) -> k, r
       | None -> text, "")
  in
  let no_body d =
    if List.is_empty body
    then return d
    else error "unexpected indented lines below %s" keyword
  in
  let words =
    String.split (String.strip rest) ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
  in
  match keyword with
  | "account" ->
    let%map name, same_line = name_and_comment rest in
    let lines, subdirectives = split_directive_body body in
    Account { name; comment = { same_line; lines }; subdirectives }
  | "commodity" -> parse_commodity st rest body
  | "decimal-mark" ->
    (match strip_comment rest with
     | (("." | ",") as m), same_line ->
       st.decimal_mark <- Some m.[0];
       no_body (Decimal_mark { mark = m.[0]; comment = { same_line; lines = [] } })
     | m, _ -> error "decimal-mark must be . or , (got %S)" m)
  | "include" ->
    let path, same_line = strip_comment rest in
    if String.is_empty path
    then error "include needs a file path"
    else no_body (Include { path; comment = { same_line; lines = [] } })
  | ("payee" | "tag") as kw ->
    (* hledger ignores indented subdirectives of these; so do we. *)
    let%map name, same_line = name_and_comment rest in
    let lines, _ = split_directive_body body in
    let name = if String.equal name "\"\"" then "" else name in
    let comment = { same_line; lines } in
    if String.equal kw "payee" then Payee { name; comment } else Tag { name; comment }
  | "P" ->
    let%bind d =
      parse_price st ~default_year:(Option.first_some st.year default_year) rest
    in
    let lines, others = split_directive_body body in
    if not (List.is_empty others)
    then error "unexpected indented lines below P"
    else (
      match d with
      | Price p -> return (Price { p with comment = { p.comment with lines } })
      | d -> return d)
  | "alias" -> Result.bind (parse_alias rest) ~f:no_body
  | "end" ->
    (match words with
     | [ "aliases" ] -> no_body End_aliases
     | [ "apply"; "account" ] -> no_body End_apply_account
     | _ -> return (Other (text :: List.map body ~f:(fun l -> l.text))))
  | "apply" ->
    (match words with
     | "account" :: _ :: _ ->
       let name =
         String.strip (String.chop_prefix_exn (String.strip rest) ~prefix:"account")
       in
       no_body (Apply_account name)
     | "year" :: _ ->
       Result.bind
         (parse_year st (String.chop_prefix_exn (String.strip rest) ~prefix:"year"))
         ~f:no_body
     | _ -> return (Other (text :: List.map body ~f:(fun l -> l.text))))
  | "year" -> Result.bind (parse_year st rest) ~f:no_body
  | "D" ->
    let arg, same_line = strip_comment rest in
    let%bind sample = Amount.of_string arg in
    register_mark st sample arg;
    no_body (Default_commodity { sample; comment = { same_line; lines = [] } })
  | kw
    when List.mem ledger_directives kw ~equal:String.equal
         || String.is_prefix kw ~prefix:"--" ->
    return (Other (text :: List.map body ~f:(fun l -> l.text)))
  | kw -> error "unknown directive %S" kw
;;

let journal ?(file = "<input>") ?default_year contents =
  let open Or_error.Let_syntax in
  let st =
    { year = None; decimal_mark = None; commodity_marks = String.Table.create () }
  in
  let at number r = at ~file number r in
  let lines =
    String.split contents ~on:'\n'
    |> List.mapi ~f:(fun i text ->
      { number = i + 1; text = String.chop_suffix_if_exists text ~suffix:"\r" })
  in
  (* A file ending in a newline has an empty last element, which is not a line. *)
  let lines =
    match List.last lines with
    | Some { text = ""; _ } -> List.drop_last_exn lines
    | _ -> lines
  in
  let rec loop lines acc =
    match lines with
    | [] -> return (List.rev acc)
    | l :: rest when is_blank_line l.text -> loop rest (Blank :: acc)
    | l :: rest when String.equal (String.strip l.text) "comment" ->
      let block, rest =
        List.split_while rest ~f:(fun l ->
          not (String.equal (String.strip l.text) "end comment"))
      in
      let rest = List.drop rest 1 in
      loop rest (Comment_block (List.map block ~f:(fun l -> l.text)) :: acc)
    | l :: rest when is_indented l.text ->
      (* Indented comment lines outside an entry are kept as top-level comments. *)
      (match (String.lstrip l.text).[0] with
       | ';' | '#' -> loop rest (Comment_line l.text :: acc)
       | _ -> at l.number (error "unexpected indented line"))
    | l :: rest
      when Char.equal l.text.[0] '#'
           || Char.equal l.text.[0] ';'
           || Char.equal l.text.[0] '*' -> loop rest (Comment_line l.text :: acc)
    | l :: rest ->
      let body, rest =
        List.split_while rest ~f:(fun l ->
          is_indented l.text && not (is_blank_line l.text))
      in
      let%bind item =
        match l.text.[0] with
        | '0' .. '9' ->
          let%map t = parse_transaction st ~default_year ~file l body in
          Transaction t
        | '~' ->
          let%map p = parse_periodic st ~file l body in
          Periodic p
        | '=' ->
          let%map a = parse_auto st ~file l body in
          Auto a
        | 'Y'
          when String.length l.text > 1
               && (Char.is_digit l.text.[1] || is_blank l.text.[1]) ->
          (* Y2024 or Y 2024. *)
          let%map d = at l.number (parse_year st (String.drop_prefix l.text 1)) in
          Directive d
        | _ ->
          let%map d = at l.number (parse_directive st ~default_year l body) in
          Directive d
      in
      loop rest (item :: acc)
  in
  loop lines []
;;

let journal_exn ?file ?default_year contents =
  Or_error.ok_exn (journal ?file ?default_year contents)
;;
