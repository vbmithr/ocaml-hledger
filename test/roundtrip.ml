open Core

(* Parses each file and prints it back to stdout. Yearless dates default to the current
   year, as in hledger. *)
let () =
  let default_year =
    Date.year (Time_ns.to_date (Time_ns.now ()) ~zone:Time_float.Zone.utc)
  in
  List.iter
    (List.tl_exn (Array.to_list (Sys.get_argv ())))
    ~f:(fun file ->
      match Hledger.Parse.journal ~file ~default_year (In_channel.read_all file) with
      | Ok j -> print_string (Hledger.Print.journal j)
      | Error e ->
        eprintf "%s\n" (Error.to_string_hum e);
        exit 1)
;;
