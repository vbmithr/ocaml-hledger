(lang dune 3.10)

(name hledger)

(generate_opam_files true)

(source
 (github vbmithr/ocaml-hledger))

(authors "Vincent Bernardoff")

(maintainers "Vincent Bernardoff")

(license ISC)

(package
 (name hledger)
 (synopsis "Read and write hledger 2.0 journal files")
 (depends
  (ocaml
   (>= 5.1))
  base
  (decimal (>= 1.0.2))
  ptime
  ppx_sexp_conv
  ppx_compare
  ppx_let
  (stdio :with-test)
  (ppx_sexp_value :with-test)
  (ppx_expect :with-test)))
