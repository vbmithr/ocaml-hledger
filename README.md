# hledger

Read and write [hledger](https://hledger.org) 2.0 journal files from OCaml.
Built with Jane Street Base, `decimal` and Ptime. Core, Bigdecimal and
`ppx_jane` are not dependencies.

    match Hledger.Parse.journal ~file contents with
    | Ok journal -> print_string (Hledger.Print.journal journal)
    | Error e -> prerr_endline (Base.Error.to_string_hum e)

## Dependencies and API

Base provides collections, strings, errors and S-expressions. It supplies the
standard-library functions this binding needs without Core's date, time and Unix
stack. Parsing errors remain `Base.Or_error.t`; generated comparison, equality and
S-expression functions remain available. The build uses only `ppx_sexp_conv`,
`ppx_compare` and `ppx_let`, rather than the full `ppx_jane` suite. Tests additionally
use Stdio, expect-test PPXs and the standard Unix module for the current year.

Amounts now contain ordinary `Decimal.t` values from the `decimal` package:

```ocaml
let amount =
  Hledger.Amount.create ~commodity:"EUR" (Decimal.of_string "12.50")

let transaction =
  Hledger.Journal.transaction
    ~description:"Lunch"
    (2026, 10, 25)
    [ Hledger.Journal.posting "expenses:food" ~amount
    ; Hledger.Journal.posting "assets:bank"
    ]
```

This changes public types from the previous Core-based API:

| Previous type | Current type |
| --- | --- |
| `Bigdecimal.t` | `Decimal.t` |
| `Core.Date.t` | `Ptime.date`, a `(year, month, day)` tuple |
| `Core.Time_ns.Ofday.t` | `Ptime.time`, `((hour, minute, second), 0)` |

Dates stay calendar dates; no timestamp or timezone is invented for a transaction.
Ptime validates parsed dates and price times. Price directives have no timezone
suffix, so a programmatically supplied price time must use offset zero; rendering
rejects other offsets. Price times have integral seconds and print as `HH:MM:SS`.
The private calendar helpers only adapt journal syntax and S-expressions to Ptime.

Parsing decimal quantities constructs their digits and exponent directly, without
an arithmetic scaling operation. Negation and display use exact copy operations,
so a caller's arithmetic precision or rounding settings cannot truncate journal
quantities. NaN and infinities are rejected by amount construction and rendering.
The binding does not change `Decimal.Context.default`. Arithmetic performed by
callers still follows the decimal library's context rules. Zarith/GMP remains a
dependency of `decimal`.

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
precision), as in hledger. Decimal values retain trailing zeros, while journal
style controls how many digits to show. `Amount.create` defaults to the quantity's
decimal scale; an explicit style or precision can choose another minimum. Digits
needed to represent the quantity are never dropped. Quantity S-expressions use
canonical plain decimal text; the separate style preserves display precision.

## Tests

    dune runtest

Runs expect tests, including exact quantities under a low-precision arithmetic
context, invalid calendar dates/times and S-expression round trips. Then
`test/conformance.sh` checks with the `hledger` binary
that each corpus journal and its parse/print round trip give the same `print`,
`print --auto`, `print --forecast`, `accounts`, `commodities`, `prices`, `tags`,
`payees` and `balance` output, and that printing is idempotent. It is skipped when
`hledger` is not installed. Pass any other journals to the script directly:

    test/conformance.sh _build/default/test/roundtrip.exe ~/code/hledger/examples/*.journal

The reference is hledger 1.99.5 (the 2.0 preview line).
