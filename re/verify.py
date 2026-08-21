#!/usr/bin/env python3
"""One-command verification of the whole solve. Run from re/:

    python3 verify.py            # all checks except the z3 uniqueness proof
    python3 verify.py unique     # everything (adds ~1s for z3)

Checks: Star Battle constraints of the solution, simulated success + message,
the three easter-egg messages, post-reset flop state, and (optionally) that
z3 re-derives the same 121 bits and proves them unique.
"""
import json
import sys

from sim import Netlist

N = 11
BITS = open("solution_fixed.txt").read().strip()
assert len(BITS) == N * N

failures = []


def check(name, ok, detail=""):
    print(f"  {'ok' if ok else 'FAIL'}  {name}" + (f"  ({detail})" if detail else ""))
    if not ok:
        failures.append(name)


# --- 1. grid constraints ---------------------------------------------------
g = [[int(BITS[r * N + c]) for c in range(N)] for r in range(N)]
rows = [sum(r) for r in g]
cols = [sum(g[r][c] for r in range(N)) for c in range(N)]
touching = sum(
    1
    for r in range(N) for c in range(N) if g[r][c]
    for dr, dc in ((0, 1), (1, -1), (1, 0), (1, 1))
    if 0 <= r + dr < N and 0 <= c + dc < N and g[r + dr][c + dc]
)
print("grid constraints:")
check("two stars in every row", rows == [2] * N)
check("two stars in every column", cols == [2] * N)
check("no two stars adjacent (incl. diagonal)", touching == 0)

# --- 2. simulate the chip --------------------------------------------------
nl = Netlist("puzzle_netlist.json")


def clock(st, inputs):
    nl.step(st, {**inputs, "clk": 0})
    nl.step(st, {**inputs, "clk": 1})


def run(bitstr):
    st = nl.new_state()
    for _ in range(3):
        clock(st, {"rst_n": 0, "enable": 0, "I": 0})
    post_reset = list(st["q"])
    for b in bitstr:
        clock(st, {"rst_n": 1, "enable": 1, "I": int(b)})
    succ, msg = 0, []
    for _ in range(18):
        clock(st, {"rst_n": 1, "enable": 0, "I": 0})
        succ = max(succ, nl.read(st, "success"))
        byte = sum(v << i for i, v in enumerate(nl.read_bus(st, [f"O[{i}]" for i in range(8)])))
        if byte:
            msg.append(chr(byte) if 32 <= byte < 127 else "?")
    return succ, "".join(msg), post_reset


print("chip simulation:")
succ, msg, post_reset = run(BITS)
check("success = 1 on the solution", succ == 1)
check('chip prints "(* TWO STARS *)"', msg == "(* TWO STARS *)", repr(msg))
s0, m0, _ = run("0" * 121)
s1, m1, _ = run("1" * 121)
wrong = "1" + BITS[1:]
s2, m2, _ = run(wrong)
check('all-zeros grid: success=0, "EMPTY SKY"', (s0, m0) == (0, "EMPTY SKY"), f"{s0} {m0!r}")
check('all-ones grid: success=0, "BIG BANG"', (s1, m1) == (0, "BIG BANG"), f"{s1} {m1!r}")
check('near-miss grid: success=0, "TRY AGAIN"', (s2, m2) == (0, "TRY AGAIN"), f"{s2} {m2!r}")

rs = json.load(open("reset_state.json"))
id2idx = {d["id"]: i for i, d in enumerate(nl.dffs)}
match = all(post_reset[id2idx[fid]] == v for fid, v in zip(rs["ids"], rs["init"]))
check("post-reset flop state matches reset_state.json", match)

# --- 2b. warmup netlist, functional (A + B == 496 raises S) ----------------
wu = Netlist("warmup_netlist.json")


def warmup_s(a, b):
    st = wu.new_state()
    for _ in range(2):
        wu.step(st, {"clk": 0, "rst_n": 0, "en": 0, "A": 0, "B": 0})
        wu.step(st, {"clk": 1, "rst_n": 0, "en": 0, "A": 0, "B": 0})
    for i in range(7, -1, -1):
        bits_in = {"rst_n": 1, "en": 1, "A": (a >> i) & 1, "B": (b >> i) & 1}
        wu.step(st, {**bits_in, "clk": 0})
        wu.step(st, {**bits_in, "clk": 1})
    return wu.read(st, "S")


print("warmup netlist (functional):")
check("248 + 248 = 496 raises S", warmup_s(0xF8, 0xF8) == 1)
check("255 + 241 = 496 raises S", warmup_s(0xFF, 0xF1) == 1)
check("1 + 2 keeps S low", warmup_s(0x01, 0x02) == 0)

# --- 3. uniqueness (z3) ----------------------------------------------------
if "unique" in sys.argv or "--unique" in sys.argv:
    import z3
    from cells import COMB

    ND = len(nl.dffs)
    SUCC = id2idx[94]
    ONE, ZERO = z3.BitVecVal(1, 1), z3.BitVecVal(0, 1)
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

    s = z3.Solver()
    I = [z3.BitVec(f"I_{t}", 1) for t in range(121)]
    q = [z3.BitVecVal(post_reset[di], 1) for di in range(ND)]
    succ_exprs = []
    for cyc in range(123):
        en = ONE if cyc < 121 else ZERO
        Ib = I[cyc] if cyc < 121 else ZERO
        val = eval_comb(q, {"clk": ONE, "rst_n": ONE, "enable": en, "I": Ib})
        nq = []
        for di, dff in enumerate(nl.dffs):
            dn = dff["d"]
            fv = z3.BitVec(f"q{di}_{cyc}", 1)
            s.add(fv == (val.get(dn, ZERO) if dn is not None else ZERO))
            nq.append(fv)
        q = nq
        if cyc >= 121:
            succ_exprs.append(q[SUCC])
    s.add(z3.Or([e == ONE for e in succ_exprs]))
    print("uniqueness (z3):")
    check("solver finds a witness", s.check() == z3.sat)
    m = s.model()
    bits = "".join("1" if m.eval(I[t]) == ONE else "0" for t in range(121))
    check("witness equals solution_fixed.txt", bits == BITS)
    s.add(z3.Or([I[t] != m.eval(I[t]) for t in range(121)]))
    check("no second solution exists (unsat)", s.check() == z3.unsat)

print()
if failures:
    print(f"{len(failures)} CHECK(S) FAILED: {failures}")
    sys.exit(1)
print("all checks passed")
