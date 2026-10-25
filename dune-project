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
  core
  bigdecimal
  ppx_jane
  (ppx_expect :with-test)))
