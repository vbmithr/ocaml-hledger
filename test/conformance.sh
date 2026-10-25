#!/usr/bin/env bash
# Checks that hledger reads a journal and its parse/print round trip identically,
# and that printing is idempotent. Usage: conformance.sh ROUNDTRIP_EXE FILE...
set -u
roundtrip=$1; shift
command -v hledger >/dev/null || { echo "hledger not found, skipping"; exit 0; }
tmp=$(mktemp -d "${TMPDIR:-$HOME/.cache}/hledger-conf.XXXXXX")
trap 'rm -rf "$tmp"' EXIT
status=0
reports=(
  "print"
  "print --auto"
  "print --forecast=2025-01..2025-04"
  "accounts --types --directives"
  "commodities"
  "prices"
  "tags --values"
  "payees"
  "balance --flat --no-total"
)
for f in "$@"; do
  dir=$(dirname "$f")
  out="$dir/.roundtrip.$(basename "$f")"
  "$roundtrip" "$f" > "$out" || { echo "FAIL parse $f"; status=1; continue; }
  "$roundtrip" "$out" > "$tmp/twice" || { echo "FAIL reparse $f"; status=1; rm -f "$out"; continue; }
  cmp -s "$out" "$tmp/twice" || { echo "FAIL idempotence $f"; diff "$out" "$tmp/twice"; status=1; }
  for r in "${reports[@]}"; do
    hledger -I -f "$f" $r > "$tmp/a" 2>&1
    hledger -I -f "$out" $r > "$tmp/b" 2>&1
    sed -i "s|$(basename "$out")|$(basename "$f")|g" "$tmp/b"
    if ! cmp -s "$tmp/a" "$tmp/b"; then
      echo "FAIL $f: hledger $r"; diff "$tmp/a" "$tmp/b" | head -20; status=1
    fi
  done
  rm -f "$out"
done
exit $status
