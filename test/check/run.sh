#!/bin/sh
# `psx check` reports compiler errors in holes at their .psx positions.
#
# Compiles each test/check/<Name>.psx, which has a type error in a hole, as
# module Examples.Check<Name> of the Portico examples, and compares the
# positions psx check reports with test/check/expected.txt. Run from the
# workspace root after `spago build`.
set -u
for f in test/check/*.psx; do
  cp "$f" "examples/portico/src/Check$(basename "$f")"
done
node packages/psx-cli/bin/psx.mjs check examples/portico/src -- -p psx-examples-portico > test/check/actual.txt 2>&1
status=$?
rm -f examples/portico/src/Check*.psx examples/portico/src/Check*.purs
grep -E '^[^ ].*: (error|warning)' test/check/actual.txt | sed 's/:$//' | sort > test/check/positions.txt
if [ "$status" -eq 0 ]; then
  echo "psx check passed modules with type errors"
  cat test/check/actual.txt
  exit 1
fi
if ! diff -u test/check/expected.txt test/check/positions.txt; then
  cat test/check/actual.txt
  exit 1
fi
rm -f test/check/actual.txt test/check/positions.txt
echo "psx check reported every error at its .psx position"
