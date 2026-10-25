# hledger

Read and write [hledger](https://hledger.org) 2.0 journal files from OCaml.
Built with Core; amounts are Jane Street `Bigdecimal`s.

    match Hledger.Parse.journal ~file contents with
    | Ok journal -> print_string (Hledger.Print.journal journal)
    | Error e -> prerr_endline (Error.to_string_hum e)

## Scope

`Parse` produces a syntax tree (`Journal.t`) that keeps comment lines, blank lines,
comment blocks and unrecognised Ledger directives, so a file can be rewritten without
losing what the user wrote. Layout inside entries is normalised: postings are aligned
like `hledger print`, dates are written `YYYY-MM-DD`, and yearless dates are resolved
from `Y` or `~default_year`.

Nothing is resolved semantically yet: includes are not followed, aliases not applied,
missing amounts not inferred, transactions not balanced, assertions not checked, and
period expressions and auto-posting queries are kept as strings.

Amounts carry their display style (symbol side, spacing, decimal mark, digit groups,
precision), as in hledger, since `Bigdecimal` does not keep trailing zeros.

## Tests

    dune runtest

Runs expect tests, then `test/conformance.sh`, which checks with the `hledger` binary
that each corpus journal and its parse/print round trip give the same `print`,
`print --auto`, `print --forecast`, `accounts`, `commodities`, `prices`, `tags`,
`payees` and `balance` output, and that printing is idempotent. It is skipped when
`hledger` is not installed. Pass any other journals to the script directly:

    test/conformance.sh _build/default/test/roundtrip.exe ~/code/hledger/examples/*.journal

The reference is hledger 1.99.5 (the 2.0 preview line).
