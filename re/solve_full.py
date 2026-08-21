#!/usr/bin/env python3
"""Full unroll: feed 121 symbolic bits then play back the generator, constrain
success=1 AND every emitted byte printable-or-zero, then read the message."""
import json
import sys
import z3
from sim import Netlist
from cells import COMB

nl = Netlist("puzzle_netlist.json")
ND = len(nl.dffs)
id2dff = {dff["id"]: k for k, dff in enumerate(nl.dffs)}
SUCC = id2dff[94]
ONE, ZERO = z3.BitVecVal(1, 1), z3.BitVecVal(0, 1)
N = 121
PLAY = 18
Oid = [nl.ports[f"O[{b}]"] for b in range(8)]

# post-reset flop state (dfstp flops power up to 1) -- see reset_state.json
_rs = json.load(open("reset_state.json"))
RESET_Q = [0] * ND
for _fid, _v in zip(_rs["ids"], _rs["init"]):
    RESET_Q[id2dff[_fid]] = _v

const_expr = {net: (ONE if v else ZERO) for net, v in nl.const_nets.items()}


def eval_comb(qexpr, inp):
    val = dict(const_expr)
    for nm, net in nl.in_ports.items():
        if net is not None:
            val[net] = inp[nm]
    for di, dff in enumerate(nl.dffs):
        if dff["q"] is not None:
            val[dff["q"]] = qexpr[di]
    for ci in nl.order:
        iid, t, onet, ins = nl.comb[ci]
        if onet is None:
            continue
        out, fn = COMB[t]
        p = {k: val.get(v, ZERO) for k, v in ins.items()}
        val[onet] = z3.If(p["S"] == ONE, p["A1"], p["A0"]) if t == "mux2_1" else fn(p)
    return val


def build(require_printable=True, want_success=True):
    s = z3.Solver()
    I = [z3.BitVec(f"I_{t}", 1) for t in range(N)]
    q = [z3.BitVecVal(v, 1) for v in RESET_Q]
    play_bytes = []
    for cyc in range(N + PLAY):
        en = ONE if cyc < N else ZERO
        Ib = I[cyc] if cyc < N else ZERO
        inp = {"clk": ONE, "rst_n": ONE, "enable": en, "I": Ib}
        val = eval_comb(q, inp)
        nq = []
        for di, dff in enumerate(nl.dffs):
            dn = dff["d"]
            nx = val.get(dn, ZERO) if dn is not None else ZERO
            fv = z3.BitVec(f"q{di}_{cyc}", 1)
            s.add(fv == nx)
            nq.append(fv)
        q = nq
        if cyc >= N:
            # O byte for this playback cycle (recompute comb with settled q)
            v2 = eval_comb(q, {"clk": ONE, "rst_n": ONE, "enable": ZERO, "I": ZERO})
            byte = z3.Concat(*[v2[Oid[b]] for b in range(7, -1, -1)])  # bit7..bit0
            play_bytes.append(byte)
        if cyc == N + 1 and want_success:
            s.add(q[SUCC] == ONE)
    if require_printable:
        for bb in play_bytes:
            s.add(z3.Or(bb == 0, z3.And(bb >= 32, bb <= 126)))
    return s, I, play_bytes


def read(s, I, play_bytes):
    m = s.model()
    bits = [1 if m.eval(I[t]) == ONE else 0 for t in range(N)]
    vals = [m.eval(bb).as_long() for bb in play_bytes]
    return bits, vals


if __name__ == "__main__":
    mode = sys.argv[1] if len(sys.argv) > 1 else "both"
    s, I, pb = build(require_printable=True, want_success=(mode != "nosucc"))
    print(f"solving (success={mode!='nosucc'}, printable playback)...", flush=True)
    r = s.check()
    print("result:", r)
    if r == z3.sat:
        bits, vals = read(s, I, pb)
        msg = "".join(chr(v) if 32 <= v < 127 else ("." if v == 0 else f"[{v}]") for v in vals)
        print("playback:", msg)
        print("input:", "".join(map(str, bits)))
        open("solution_full.txt", "w").write("".join(map(str, bits)))
