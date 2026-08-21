#!/usr/bin/env python3
"""Regenerate winning_inputs.vcd with the design outputs recorded, so
`python3 vcd_check.py puzzle_netlist.json winning_inputs.vcd` replays it
and compares O[7:0] / success at every timestamp (a real self-check).

Timing: 10ns clock, inputs change on the falling-edge timestamp, so each
input bit is stable before the rising edge that samples it.
"""
from sim import Netlist

BITS = open("solution_fixed.txt").read().strip()
nl = Netlist("puzzle_netlist.json")

IDS = {"clk": "!", "rst_n": '"', "enable": "#", "I": "$", "O": "%", "success": "&"}
HALF = 5000  # ps

header = """$date
  Jane Street ASIC puzzle: winning inputs (with recorded outputs)
$end
$version
  re/make_winning_vcd.py
$end
$timescale
\t1ps
$end
$scope module puzzle $end
$var reg 1 ! clk $end
$var reg 1 " rst_n $end
$var reg 1 # enable $end
$var reg 1 $ I $end
$var wire 8 % O [7:0] $end
$var wire 1 & success $end
$upscope $end
$enddefinitions $end
"""

st = nl.new_state()
lines = [header.rstrip("\n")]
t = 0
prev = {}


def emit(inputs):
    """Step the sim with `inputs`, dump changed signals at time t."""
    global t
    nl.step(st, inputs)
    obits = nl.read_bus(st, [f"O[{b}]" for b in range(8)])
    oval = "".join(str(b) for b in reversed(obits))
    vals = {**{k: str(v) for k, v in inputs.items()}, "O": oval,
            "success": str(nl.read(st, "success"))}
    changes = [k for k, v in vals.items() if prev.get(k) != v]
    if changes:
        lines.append(f"#{t}")
        for k in changes:
            if k == "O":
                lines.append(f"b{vals[k]} {IDS[k]}")
            else:
                lines.append(f"{vals[k]}{IDS[k]}")
            prev[k] = vals[k]
    t += HALF


def cycle(rst_n, enable, i):
    emit({"clk": 0, "rst_n": rst_n, "enable": enable, "I": i})
    emit({"clk": 1, "rst_n": rst_n, "enable": enable, "I": i})


for _ in range(3):                      # reset
    cycle(0, 0, 0)
for b in BITS:                          # load the 121-bit grid
    cycle(1, 1, int(b))
msg = []
for _ in range(18):                     # playback
    cycle(1, 0, 0)
    byte = sum(v << i for i, v in enumerate(nl.read_bus(st, [f"O[{i}]" for i in range(8)])))
    if 32 <= byte < 127:
        msg.append(chr(byte))
emit({"clk": 0, "rst_n": 1, "enable": 0, "I": 0})

open("winning_inputs.vcd", "w").write("\n".join(lines) + "\n")
print(f"wrote winning_inputs.vcd: {len(lines)} lines, {t // HALF} half-cycles")
print("success:", nl.read(st, "success"), " message:", "".join(msg))
