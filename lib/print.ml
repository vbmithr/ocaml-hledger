open Base
open Journal

(* Display width of UTF-8 text, counting code points. *)
let width s = String.count s ~f:(fun c -> Char.to_int c land 0xc0 <> 0x80)
let date = Calendar.date_to_string

let status_mark = function
  | Unmarked -> ""
  | Pending -> "! "
  | Cleared -> "* "
;;

let cost = function
  | Unit a -> "@ " ^ Amount.to_string a
  | Total a -> "@@ " ^ Amount.to_string a
;;

(* hledger's {DATE, "LABEL", COST} when the basis is per unit, else Ledger's {{TOTAL}}
   with [DATE] and (LABEL), the only syntax for a total basis. *)
let lot { lot_date; label; basis } =
  match basis with
  | Some (Total a) ->
    String.concat
      ~sep:" "
      (("{{" ^ Amount.to_string a ^ "}}")
       :: List.filter_opt
            [ Option.map lot_date ~f:(fun d -> "[" ^ date d ^ "]")
            ; Option.map label ~f:(fun l -> "(" ^ l ^ ")")
            ])
  | Some (Unit _) | None ->
    let parts =
      List.filter_opt
        [ Option.map lot_date ~f:date
        ; Option.map label ~f:(fun l -> "\"" ^ l ^ "\"")
        ; Option.map basis ~f:(function Unit a | Total a -> Amount.to_string a)
        ]
    in
    "{" ^ String.concat ~sep:", " parts ^ "}"
;;

let same_line_comment = function
  | None -> ""
  | Some "" -> "  ;"
  | Some c -> "  ; " ^ c
;;

let comment_lines ~indent (c : comment) =
  List.map c.lines ~f:(fun l ->
    String.make indent ' ' ^ if String.is_empty l then ";" else "; " ^ l)
;;

let account (p : posting) =
  let name =
    match p.kind with
    | Real -> p.account
    | Virtual -> "(" ^ p.account ^ ")"
    | Balanced_virtual -> "[" ^ p.account ^ "]"
  in
  status_mark p.status ^ name
;;

let amount_column (p : posting) =
  match p.amount with
  | None -> ""
  | Some (Amount a) -> Amount.to_string a
  | Some (Multiplier a) -> "*" ^ Amount.to_string a
;;

let assertion { expected; expected_cost; sole; inclusive } =
  (if sole then "==" else "=")
  ^ (if inclusive then "*" else "")
  ^ " "
  ^ Amount.to_string expected
  ^ Option.value_map expected_cost ~default:"" ~f:(fun c -> " " ^ cost c)
;;

let indent = 4

(* Posting lines aligned like hledger print: accounts left, amounts right-aligned in a
   column two spaces after the longest account, then cost basis, cost and assertion. *)
let postings ps =
  let accounts = List.map ps ~f:account in
  let amounts = List.map ps ~f:amount_column in
  let account_width = List.fold accounts ~init:0 ~f:(fun m a -> Int.max m (width a)) in
  let amount_width = List.fold amounts ~init:0 ~f:(fun m a -> Int.max m (width a)) in
  List.concat
    (List.map3_exn ps accounts amounts ~f:(fun p acct amt ->
       let has_amount = not (String.is_empty amt) in
       let tail =
         match has_amount, p.assertion with
         | false, None -> acct
         | _ ->
           let pad = account_width - width acct + 2 + amount_width - width amt in
           let annotations =
             List.filter_opt
               [ Option.map p.lot ~f:lot
               ; Option.map p.cost ~f:cost
               ; Option.map p.assertion ~f:assertion
               ]
           in
           String.concat ~sep:" " ((acct ^ String.make pad ' ' ^ amt) :: annotations)
       in
       (String.make indent ' ' ^ tail ^ same_line_comment p.comment.same_line)
       :: comment_lines ~indent:(indent + 2) p.comment))
;;

let entry ~header (comment : comment) ps =
  ((header ^ same_line_comment comment.same_line) :: comment_lines ~indent comment)
  @ postings ps
;;

let header_tail ~status ~code ~description =
  String.concat
    (List.filter_opt
       [ (match status with
          | Unmarked -> None
          | Pending -> Some " !"
          | Cleared -> Some " *")
       ; Option.map code ~f:(fun c -> " (" ^ c ^ ")")
       ; Option.some_if (not (String.is_empty description)) (" " ^ description)
       ])
;;

let transaction_lines (t : transaction) =
  let header =
    date t.date
    ^ Option.value_map t.date2 ~default:"" ~f:(fun d -> "=" ^ date d)
    ^ header_tail ~status:t.status ~code:t.code ~description:t.description
  in
  entry ~header t.comment t.postings
;;

let periodic_lines (p : periodic) =
  let tail = header_tail ~status:p.status ~code:p.code ~description:p.description in
  (* Two spaces end the period expression. *)
  let header = "~ " ^ p.period ^ if String.is_empty tail then "" else " " ^ tail in
  entry ~header p.comment p.postings
;;

let auto_lines (a : auto) = entry ~header:("= " ^ a.query) a.comment a.postings

let with_comment line (c : comment) =
  (line ^ same_line_comment c.same_line) :: comment_lines ~indent:2 c
;;

let quoted_name name = if String.is_empty name then "\"\"" else name

let directive_lines = function
  | Account { name; comment; subdirectives } ->
    with_comment ("account " ^ name) comment
    @ List.map subdirectives ~f:(fun s -> "  " ^ s)
  | Commodity { symbol; sample; comment; subdirectives } ->
    let arg =
      match sample with
      | Some a -> Amount.to_string ~force_decimal_mark:true a
      | None -> if String.is_empty symbol then "\"\"" else Amount.format_commodity symbol
    in
    with_comment ("commodity " ^ arg) comment
    @ List.map subdirectives ~f:(fun s -> "  " ^ s)
  | Decimal_mark { mark; comment } ->
    with_comment ("decimal-mark " ^ String.of_char mark) comment
  | Include { path; comment } -> with_comment ("include " ^ path) comment
  | Payee { name; comment } -> with_comment ("payee " ^ quoted_name name) comment
  | Tag { name; comment } -> with_comment ("tag " ^ name) comment
  | Price { date = d; time; commodity; price; comment } ->
    let time =
      Option.value_map time ~default:"" ~f:(fun t -> " " ^ Calendar.time_to_string t)
    in
    with_comment
      (Printf.sprintf
         "P %s%s %s %s"
         (date d)
         time
         (Amount.format_commodity commodity)
         (Amount.to_string price))
      comment
  | Alias { pattern; replacement } ->
    let pattern =
      match pattern with
      | Exact s -> s
      | Regex r -> "/" ^ r ^ "/"
    in
    [ "alias " ^ pattern ^ " = " ^ replacement ]
  | End_aliases -> [ "end aliases" ]
  | Apply_account a -> [ "apply account " ^ a ]
  | End_apply_account -> [ "end apply account" ]
  | Default_commodity { sample; comment } ->
    with_comment ("D " ^ Amount.to_string ~force_decimal_mark:true sample) comment
  | Default_year { year; comment } -> with_comment ("Y " ^ Int.to_string year) comment
  | Other lines -> lines
;;

let item_lines = function
  | Blank -> [ "" ]
  | Comment_line l -> [ l ]
  | Comment_block ls -> ("comment" :: ls) @ [ "end comment" ]
  | Transaction t -> transaction_lines t
  | Periodic p -> periodic_lines p
  | Auto a -> auto_lines a
  | Directive d -> directive_lines d
;;

let lines_to_string ls = String.concat (List.map ls ~f:(fun l -> l ^ "\n"))
let transaction t = lines_to_string (transaction_lines t)
let item i = lines_to_string (item_lines i)
let journal (j : Journal.t) = lines_to_string (List.concat_map j ~f:item_lines)
