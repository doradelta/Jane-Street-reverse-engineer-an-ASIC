#!/usr/bin/env bash
# Build the OCaml port and run every check against the shipped files.
# Works from any directory; stops at the first failure.
set -euo pipefail
cd "$(dirname "$0")/.."
command -v z3 >/dev/null || { echo "z3 is not on the PATH (pip install z3-solver)" >&2; exit 1; }
(cd ocaml && dune build)
A=ocaml/_build/default/bin/asicre.exe
P=asic-puzzle-2026
out=$(mktemp)
trap 'rm -f "$out"' EXIT

echo "== extract puzzle.gds";        "$A" extract "$P/puzzle.gds" "$out"
echo "== check winning grid + z3";   "$A" check "$P/puzzle.gds" re/solution_fixed.txt --reset-state re/reset_state.json --unique
echo "== solve from the geometry";   "$A" solve "$P/puzzle.gds"
echo "== warm-up design";            "$A" warmup "$P/warmup/04_final.gds"
echo "== replay example_inputs.vcd"; "$A" vcd "$P/puzzle.gds" "$P/example_inputs.vcd"
echo "== replay winning_inputs.vcd"; "$A" vcd "$P/puzzle.gds" re/winning_inputs.vcd
echo "== warm-up DEF ground truth";  "$A" def "$P/warmup/03_post_place_and_route.def" "$P/warmup/04_final.gds"
echo "everything passed"
