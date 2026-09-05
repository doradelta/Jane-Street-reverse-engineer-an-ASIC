# The solve, in OCaml

A from-scratch port of the Python pipeline in [`re/`](../re/) to OCaml: GDS in, netlist
out, chip simulated, grid solved and proven unique. It depends on nothing but the OCaml
standard library (plus `unix` and `str`, which ship with the compiler) and the `z3` binary
for the two commands that need a solver.

```
src/gds.ml          GDSII stream reader (boundaries, paths, references, texts)
src/geom.ml         integer rectangles, rectilinear polygon decomposition, grid index, union find
src/extract.ml      geometry -> netlist, the port of re/extract.py
src/cells.ml        boolean models of the 73 sky130 cell types (66 logic, 7 physical-only),
                    written once against an abstract algebra
src/sim.ml          netlist construction and the cycle-accurate simulator
src/vcd.ml          VCD reader and waveform replay
src/smt.ml          unrolled circuit as SMT-LIB2, driven through the z3 binary
src/defcheck.ml     comparison against the warm-up DEF ground truth
src/reset_state.ml  reader for re/reset_state.json
bin/asicre.ml       command line
```

Every module has an `.mli` describing its interface.

## Build and run

Needs OCaml 4.13 or newer and dune 2.9 or newer (`sudo apt install ocaml dune` on
Debian/Ubuntu/Mint), and `z3` on the `PATH` for `solve` and `check --unique`
(`pip install z3-solver` puts one in `~/.local/bin`).

```bash
cd ocaml && dune build && cd ..
A=ocaml/_build/default/bin/asicre.exe

$A extract asic-puzzle-2026/puzzle.gds puzzle_netlist.json
$A check   asic-puzzle-2026/puzzle.gds re/solution_fixed.txt --reset-state re/reset_state.json --unique
$A solve   asic-puzzle-2026/puzzle.gds
$A warmup  asic-puzzle-2026/warmup/04_final.gds
$A vcd     asic-puzzle-2026/puzzle.gds asic-puzzle-2026/example_inputs.vcd
$A vcd     asic-puzzle-2026/puzzle.gds re/winning_inputs.vcd
$A def     asic-puzzle-2026/warmup/03_post_place_and_route.def asic-puzzle-2026/warmup/04_final.gds
```

[`check_all.sh`](check_all.sh) runs all of the above and stops at the first failure.

## What it reproduces

| Check | Result |
|---|---|
| Puzzle netlist vs. the Python extraction | byte-identical JSON: 1618 instances, 713 nets, same numbering |
| Warm-up netlist vs. Python | byte-identical: 230 instances, 86 nets |
| Warm-up netlist vs. the DEF ground truth | 79/79 components matched, 84/84 nets consistent |
| `example_inputs.vcd` replay | 624/624 comparisons (every timestamp with a definite output) |
| `winning_inputs.vcd` replay | 285/285 comparisons |
| Winning grid | `success` high, chip prints `(* TWO STARS *)`; easter eggs `EMPTY SKY`, `BIG BANG`, `TRY AGAIN` |
| `solve` | z3 finds the 121 bits from the geometry alone and proves them unique, about a second end to end |

Extraction of the full puzzle GDS takes about 50 ms. Unconnected pins (the `X` output of
the fifteen `clkbuf_4` instances) are serialised as `null`, as in the Python.

## Notes on the port

**Everything is an integer rectangle.** The layout is Manhattan through and through: every
routing path is a straight two-point segment (path types 0, 2 and 4 with custom end
extensions), every polygon is rectilinear, references only rotate by 0 or 180 degrees and
mirror. So each shape becomes a handful of axis-aligned rectangles in the file's own
database units (nanometres) and every intersection, area and distance test is exact
integer arithmetic. Intervals are closed, so shapes that merely touch count as connected,
which is what shapely's `intersects` did in the Python version.

**Assumptions are checked, not assumed.** Where the Python would quietly do something
different on an unusual file, the OCaml stops with a message: a database unit other than
1 nm, a reference with magnification, a rotation that is not a multiple of 90 degrees,
round-ended or multi-point paths, non-rectilinear polygons, array references, a truncated
GDS stream, several top cells, a flop whose asynchronous set or reset is not wired to
`rst_n`, a design without the puzzle's ports. None of these occur in the shipped files.

**One definition per cell, two interpretations.** [`cells.ml`](src/cells.ml) writes each
combinational cell once against a record of boolean operations (`and`, `or`, `not`, `xor`,
`ite`). The simulator instantiates it with plain integers; the SMT emitter instantiates it
with SMT-LIB terms. The `_N` pin rule (a pin whose name ends in `_N` enters the expression
inverted) lives in one place.

**The formula stays linear.** Each net of each unrolled cycle is a named `define-fun`, so
the 123-cycle unrolling of the whole chip is about a hundred thousand short lines rather
than an exponential tree, and z3 answers both the witness query and the blocking query in
well under a second each. The unrolling follows the uniqueness proof in `re/verify.py`
(success required after the first or second playback clock); the success flop is located
as the driver of the `success` port instead of by its instance number.
