open Base
include Decimal

(* Sexp glue only: amounts remain ordinary Decimal.t values. Copy operations
   avoid context-sensitive rounding when displaying or negating quantities. *)
let finite_exn value =
  if not (Decimal.is_finite value) then invalid_arg "journal quantities must be finite";
  value
;;

let to_plain_string value =
  let value = finite_exn value in
  if Decimal.equal value Decimal.zero
  then "0"
  else (
    let value = Decimal.to_string ~format:`plain value in
    match String.lsplit2 value ~on:'.' with
    | None -> value
    | Some (whole, fraction) ->
      let fraction = String.rstrip fraction ~drop:(Char.equal '0') in
      if String.is_empty fraction then whole else whole ^ "." ^ fraction)
;;

let sexp_of_t value = Sexp.Atom (to_plain_string value)
let t_of_sexp sexp = finite_exn (Decimal.of_string (String.t_of_sexp sexp))
