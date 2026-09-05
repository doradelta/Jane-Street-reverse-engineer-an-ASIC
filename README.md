# Jane Street ASIC puzzle 2026, solved

Jane Street published [`puzzle.gds`](asic-puzzle-2026/puzzle.gds), a chip layout with every
label stripped, and asked you to make its `success` pin go high. The chip turns out to be a
validator for one specific Star Battle grid, 11×11. Feed it the right 121 bits and it raises
`success` and prints, one byte per clock:

```
(* TWO STARS *)
```

The writeup is [re/writeup.html](re/writeup.html), a plain page that reads in any
browser. Next to it sits [re/playground.html](re/playground.html), where the netlist
recovered from the GDS runs live, so you can place stars and watch the real circuit
judge them, and the die renders in 3D with every cell, wire and via at its true
coordinates.

## The answer

```
row  0   . . . . . . . ★ . ★ .      0000000101010000100000000000010101
row  1   ★ . . . . ★ . . . . .      0100000000000010100000010000010000
row  2   . . . . . . . ★ . ★ .      0010000010100001000000010000001000
row  3   ★ . ★ . . . . . . . .      0010010001010000000
row  4   . . . . ★ . ★ . . . .
row  5   . . ★ . . . . . ★ . .      121 bits in row major order, clocked
row  6   . . . . ★ . . . . . ★      in with enable high after a reset
row  7   . ★ . . . . ★ . . . .      pulse. Two stars per row and column,
row  8   . . . ★ . . . . . . ★      none touching, and z3 proves this is
row  9   . . . . . ★ . . ★ . .      the only input the circuit accepts.
row 10   . ★ . ★ . . . . . . .
```

All zeros prints `EMPTY SKY`, all ones prints `BIG BANG`, and any other grid prints
`TRY AGAIN`.

## How I solved it

1. **Geometry to netlist** ([re/extract.py](re/extract.py)). The standard cells keep their
   names in the GDS but the wiring is anonymous metal, so I union find every shape that
   touches on each layer, bridging through vias and the contacts inside each cell, until
   the netlist falls out of shape overlap. Checked on the warmup design: 79/79 cells and
   84/84 nets identical to the ground truth DEF.
2. **Gate level simulator** ([re/sim.py](re/sim.py), [re/cells.py](re/cells.py)). Boolean
   models for all 66 sky130 cell types used, flip flops with async set and reset. Replays
   the provided `example_inputs.vcd` bit exact, 624/624 timestamps.
3. **Purpose** ([re/analyze.py](re/analyze.py), [re/formula.py](re/formula.py)). Tracing
   back from `success`: a wide AND of pairwise conditions framed by a counter with period
   11. Part of it decodes into the Star Battle rules, the rest hardwires which puzzle this
   chip checks.
4. **Solve** ([re/solve.py](re/solve.py)). Unroll 121 clocks with symbolic inputs in z3 and
   ask for `success = 1`. Answered in under a second. Block that answer and z3 says unsat,
   so the input is unique.
5. **Read the message** ([re/solve_full.py](re/solve_full.py)). The output generator is an
   8 bit LFSR fed by the input stream, seeded by four flops that power up to 1, plus logic
   that turns its final value into characters. The winning grid plays back the string above.

One bug worth telling: those four seed flops power up as 1 through an async set. A typo in
my simulator left them at 0. `success` still rose, since the checker only uses flops that
reset to zero, but the message came out scrambled, and every regression stayed green because
the sample waveform never touches those flops.

## Check it yourself

Needs python3 with `gdstk`, `shapely`, `numpy` and the z3 bindings (`pip install z3-solver`).

```bash
cd re
python3 extract.py ../asic-puzzle-2026/puzzle.gds puzzle_netlist.json
python3 verify.py unique
```

`verify.py` checks the grid constraints, simulates the chip (winning grid, easter eggs,
reset state), functionally tests the warmup netlist (`A + B == 496` raises `S`), and
proves uniqueness with z3. The remaining ground truth checks:

```bash
python3 def_check.py ../asic-puzzle-2026/warmup/03_post_place_and_route.def \
                     ../asic-puzzle-2026/warmup/04_final.gds warmup_netlist.json
python3 vcd_check.py puzzle_netlist.json ../asic-puzzle-2026/example_inputs.vcd
python3 vcd_check.py puzzle_netlist.json winning_inputs.vcd
```

The last one replays [re/winning_inputs.vcd](re/winning_inputs.vcd), which records both
inputs and expected outputs, and matches 285/285 timestamps. Regenerate it with
[re/make_winning_vcd.py](re/make_winning_vcd.py).

## The same solve in OCaml

[ocaml/](ocaml/) is a from-scratch port of the whole pipeline to OCaml, standard library
only: its own GDSII reader, exact integer geometry, the simulator, the DEF and VCD checks,
and the z3 proof through SMT-LIB. It extracts the identical netlist (same 1618 instances
and 713 nets, pin for pin) in about 50 ms, and `asicre solve puzzle.gds` goes from raw
geometry to the unique winning grid in about a second.

```bash
cd ocaml && dune build && cd ..
ocaml/_build/default/bin/asicre.exe solve asic-puzzle-2026/puzzle.gds
ocaml/check_all.sh          # every check above, OCaml edition
```

## What is where

* The [puzzle folder](asic-puzzle-2026/) holds the official files: the GDS, the sample
  waveform and the warmup design.
* [re/](re/) holds the solve: extraction, simulator, solvers, verification and the
  writeup page.
* [ocaml/](ocaml/) holds the OCaml port of the solve, with its own [README](ocaml/README.md).
