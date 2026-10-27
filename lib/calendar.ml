open Base
include Ptime

(* Journal syntax uses calendar dates and timezone-free price times. Ptime does
   the validation; these helpers provide journal text and Base sexp conversion. *)
let date_exn year month day =
  let date = year, month, day in
  match Ptime.of_date date with
  | Some _ -> date
  | None -> invalid_arg "invalid calendar date"
;;

let date_to_string ((year, month, day) : Ptime.date) =
  ignore (date_exn year month day : Ptime.date);
  Printf.sprintf "%04d-%02d-%02d" year month day
;;

let date_of_string s =
  match String.split s ~on:'-' with
  | [ y; m; d ] -> date_exn (Int.of_string y) (Int.of_string m) (Int.of_string d)
  | _ -> invalid_arg "expected YYYY-MM-DD"
;;

let time_of_string s =
  let parts = List.map (String.split s ~on:':') ~f:Int.of_string in
  let time =
    match parts with
    | [ h; m ] -> (h, m, 0), 0
    | [ h; m; s ] -> (h, m, s), 0
    | _ -> invalid_arg "expected HH:MM[:SS]"
  in
  match Ptime.of_date_time ((2000, 1, 1), time) with
  | Some _ -> time
  | None -> invalid_arg "invalid time of day"
;;

let time_to_string (((h, m, s), offset) : Ptime.time) =
  if offset <> 0 then invalid_arg "journal price times cannot have a timezone offset";
  let value = Printf.sprintf "%02d:%02d:%02d" h m s in
  ignore (time_of_string value : Ptime.time);
  value
;;

let sexp_of_date date = Sexp.Atom (date_to_string date)
let date_of_sexp sexp = date_of_string (String.t_of_sexp sexp)
let compare_date = Poly.compare
let equal_date = Poly.equal
let sexp_of_time time = Sexp.Atom (time_to_string time)
let time_of_sexp sexp = time_of_string (String.t_of_sexp sexp)
let compare_time = Poly.compare
let equal_time = Poly.equal
