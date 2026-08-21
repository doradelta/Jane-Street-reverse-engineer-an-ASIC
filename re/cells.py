"""Boolean models for the sky130_fd_sc_hd cells used in the puzzle.

Verified against the official functional Verilog. Universal rule confirmed by
inspection of a21bo/o21ba/o2bb2a/and4bb/nor3b: a pin whose name ends in `_N`
enters the base AND-OR / OR-AND expression as its logical inverse.
"""


def N(p, k):
    """logical value of pin k: invert if the pin name ends in _N."""
    return (p[k] ^ 1) if k.endswith("_N") else p[k]


# each entry: (output_pin, function(p)->0/1). p maps pin-name -> 0/1.
COMB = {}


def _add(name, out, fn):
    COMB[name] = (out, fn)


# --- buffers / inverter ---
_add("buf_2",     "X", lambda p: p["A"])
_add("clkbuf_4",  "X", lambda p: p["A"])
_add("clkbuf_8",  "X", lambda p: p["A"])
_add("clkbuf_16", "X", lambda p: p["A"])
_add("inv_2",     "Y", lambda p: p["A"] ^ 1)

# --- simple gates ---
_add("and2_2", "X", lambda p: p["A"] & p["B"])
_add("and3_2", "X", lambda p: p["A"] & p["B"] & p["C"])
_add("and4_2", "X", lambda p: p["A"] & p["B"] & p["C"] & p["D"])
_add("or2_2",  "X", lambda p: p["A"] | p["B"])
_add("or3_2",  "X", lambda p: p["A"] | p["B"] | p["C"])
_add("or4_2",  "X", lambda p: p["A"] | p["B"] | p["C"] | p["D"])
_add("nand2_2", "Y", lambda p: (p["A"] & p["B"]) ^ 1)
_add("nand3_2", "Y", lambda p: (p["A"] & p["B"] & p["C"]) ^ 1)
_add("nand4_2", "Y", lambda p: (p["A"] & p["B"] & p["C"] & p["D"]) ^ 1)
_add("nor2_2", "Y", lambda p: (p["A"] | p["B"]) ^ 1)
_add("nor3_2", "Y", lambda p: (p["A"] | p["B"] | p["C"]) ^ 1)
_add("nor4_2", "Y", lambda p: (p["A"] | p["B"] | p["C"] | p["D"]) ^ 1)
_add("xor2_2",  "X", lambda p: p["A"] ^ p["B"])
_add("xnor2_2", "Y", lambda p: (p["A"] ^ p["B"]) ^ 1)
_add("mux2_1",  "X", lambda p: p["A1"] if p["S"] else p["A0"])

# --- inverted-input simple gates ---
_add("and2b_2",  "X", lambda p: N(p, "A_N") & p["B"])
_add("and3b_2",  "X", lambda p: N(p, "A_N") & p["B"] & p["C"])
_add("and4b_2",  "X", lambda p: N(p, "A_N") & p["B"] & p["C"] & p["D"])
_add("and4bb_2", "X", lambda p: N(p, "A_N") & N(p, "B_N") & p["C"] & p["D"])
_add("nand2b_2", "Y", lambda p: (N(p, "A_N") & p["B"]) ^ 1)
_add("nand3b_2", "Y", lambda p: (N(p, "A_N") & p["B"] & p["C"]) ^ 1)
_add("nor3b_2",  "Y", lambda p: (p["A"] | p["B"] | N(p, "C_N")) ^ 1)
_add("nor4b_2",  "Y", lambda p: (p["A"] | p["B"] | p["C"] | N(p, "D_N")) ^ 1)
_add("or3b_2",   "X", lambda p: p["A"] | p["B"] | N(p, "C_N"))
_add("or4b_2",   "X", lambda p: p["A"] | p["B"] | p["C"] | N(p, "D_N"))
_add("or4bb_2",  "X", lambda p: p["A"] | p["B"] | N(p, "C_N") | N(p, "D_N"))

# --- AND-OR (a...) ---
_add("a21o_2",   "X", lambda p: (p["A1"] & p["A2"]) | p["B1"])
_add("a21oi_2",  "Y", lambda p: ((p["A1"] & p["A2"]) | p["B1"]) ^ 1)
_add("a211o_2",  "X", lambda p: (p["A1"] & p["A2"]) | p["B1"] | p["C1"])
_add("a211oi_2", "Y", lambda p: ((p["A1"] & p["A2"]) | p["B1"] | p["C1"]) ^ 1)
_add("a221o_2",  "X", lambda p: (p["A1"] & p["A2"]) | (p["B1"] & p["B2"]) | p["C1"])
_add("a221oi_2", "Y", lambda p: ((p["A1"] & p["A2"]) | (p["B1"] & p["B2"]) | p["C1"]) ^ 1)
_add("a22o_2",   "X", lambda p: (p["A1"] & p["A2"]) | (p["B1"] & p["B2"]))
_add("a22oi_2",  "Y", lambda p: ((p["A1"] & p["A2"]) | (p["B1"] & p["B2"])) ^ 1)
_add("a311o_2",  "X", lambda p: (p["A1"] & p["A2"] & p["A3"]) | p["B1"] | p["C1"])
_add("a31o_2",   "X", lambda p: (p["A1"] & p["A2"] & p["A3"]) | p["B1"])
_add("a31oi_2",  "Y", lambda p: ((p["A1"] & p["A2"] & p["A3"]) | p["B1"]) ^ 1)
_add("a32o_2",   "X", lambda p: (p["A1"] & p["A2"] & p["A3"]) | (p["B1"] & p["B2"]))
_add("a41oi_2",  "Y", lambda p: ((p["A1"] & p["A2"] & p["A3"] & p["A4"]) | p["B1"]) ^ 1)
_add("a2111oi_2","Y", lambda p: ((p["A1"] & p["A2"]) | p["B1"] | p["C1"] | p["D1"]) ^ 1)
# b-variants (verified): logical B1 = ~B1_N
_add("a21bo_2",  "X", lambda p: (p["A1"] & p["A2"]) | N(p, "B1_N"))
_add("a21boi_2", "Y", lambda p: ((p["A1"] & p["A2"]) | N(p, "B1_N")) ^ 1)

# --- OR-AND (o...) ---
_add("o21a_2",   "X", lambda p: (p["A1"] | p["A2"]) & p["B1"])
_add("o21ai_2",  "Y", lambda p: ((p["A1"] | p["A2"]) & p["B1"]) ^ 1)
_add("o211a_2",  "X", lambda p: (p["A1"] | p["A2"]) & p["B1"] & p["C1"])
_add("o211ai_2", "Y", lambda p: ((p["A1"] | p["A2"]) & p["B1"] & p["C1"]) ^ 1)
_add("o221a_2",  "X", lambda p: (p["A1"] | p["A2"]) & (p["B1"] | p["B2"]) & p["C1"])
_add("o22a_2",   "X", lambda p: (p["A1"] | p["A2"]) & (p["B1"] | p["B2"]))
_add("o22ai_2",  "Y", lambda p: ((p["A1"] | p["A2"]) & (p["B1"] | p["B2"])) ^ 1)
_add("o311a_2",  "X", lambda p: (p["A1"] | p["A2"] | p["A3"]) & p["B1"] & p["C1"])
_add("o31a_2",   "X", lambda p: (p["A1"] | p["A2"] | p["A3"]) & p["B1"])
_add("o31ai_2",  "Y", lambda p: ((p["A1"] | p["A2"] | p["A3"]) & p["B1"]) ^ 1)
_add("o32a_2",   "X", lambda p: (p["A1"] | p["A2"] | p["A3"]) & (p["B1"] | p["B2"]))
_add("o32ai_2",  "Y", lambda p: ((p["A1"] | p["A2"] | p["A3"]) & (p["B1"] | p["B2"])) ^ 1)
# b-variants (verified)
_add("o21ba_2",  "X", lambda p: (p["A1"] | p["A2"]) & N(p, "B1_N"))
_add("o21bai_2", "Y", lambda p: ((p["A1"] | p["A2"]) & N(p, "B1_N")) ^ 1)
_add("o2bb2a_2", "X", lambda p: (N(p, "A1_N") | N(p, "A2_N")) & (p["B1"] | p["B2"]))

# --- constants ---
# conb handled specially in the simulator (HI=1, LO=0)

# sequential cell -> (kind). handled in simulator.
DFF = {
    "dfrtp_2": "rtp",   # async active-low reset, posedge
    "dfstp_2": "stp",   # async active-low set,   posedge
    "dfxtp_2": "xtp",   # plain posedge
}

PHYS = {"tapvpwrvgnd_1", "decap_3", "diode_2"}
