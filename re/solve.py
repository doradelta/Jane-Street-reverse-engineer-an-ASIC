#!/usr/bin/env python3
"""Bounded model checking: unroll the netlist over the input-feeding cycles
with symbolic input bits and solve for success==1 using z3.

Cell boolean lambdas from cells.COMB operate with &,|,^ which z3 BitVec(1)
overloads identically; only mux2's python `if` needs a symbolic override."""
import sys
import z3
from sim import Netlist
from cells import COMB, DFF


def solve(path, n_bits=121, eval_cycles=(119, 120, 121, 122, 123), reset_cycles=4):
    nl = Netlist(path)
    ND = len(nl.dffs)
    id2dff = {dff["id"]: k for k, dff in enumerate(nl.dffs)}
    SUCC = id2dff[94]

    def bv(name):
        return z3.BitVec(name, 1)

    ONE, ZERO = z3.BitVecVal(1, 1), z3.BitVecVal(0, 1)

    s = z3.Solver()
    # input bits
    I = [bv(f"I_{t}") for t in range(n_bits)]

    # symbolic net evaluation given current flop-Q exprs and this-cycle inputs
    const_expr = {}
    for net, v in nl.const_nets.items():
        const_expr[net] = ONE if v else ZERO

    def eval_comb(qexpr, inputs):
        """inputs: dict portname->expr. returns net->expr for all nets."""
        val = {}
        for net, e in const_expr.items():
            val[net] = e
        for name, net in nl.in_ports.items():
            if net is not None:
                val[net] = inputs[name]
        for di, dff in enumerate(nl.dffs):
            if dff["q"] is not None:
                val[dff["q"]] = qexpr[di]
        for ci in nl.order:
            iid, t, onet, ins = nl.comb[ci]
            if onet is None:
                continue
            out, fn = COMB[t]
            p = {k: val.get(v, ZERO) for k, v in ins.items()}
            if t == "mux2_1":
                val[onet] = z3.If(p["S"] == ONE, p["A1"], p["A0"])
            else:
                val[onet] = fn(p)
        return val

    # initial state after reset: dfrtp flops are 0, the four dfstp flops
    # (message-generator seed) are 1 -- see reset_state.json. The success
    # cone only contains dfrtp flops, but keep the true state anyway.
    import json
    rs = json.load(open("reset_state.json"))
    init = dict(zip(rs["ids"], rs["init"]))
    q = [z3.BitVecVal(init.get(dff["id"], 0), 1) for dff in nl.dffs]
    succ_at = {}
    T = n_bits + max(eval_cycles) - n_bits + 2
    total = max(max(eval_cycles) + 1, n_bits + 2)
    for cyc in range(total):
        en = ONE if cyc < n_bits else ZERO
        Ibit = I[cyc] if cyc < n_bits else ZERO
        inputs = {"clk": ONE, "rst_n": ONE, "enable": en, "I": Ibit}
        val = eval_comb(q, inputs)
        # next flop values (rst_n=1 so no async reset; posedge each cycle)
        nq = []
        for di, dff in enumerate(nl.dffs):
            dnet = dff["d"]
            nxt = val.get(dnet, ZERO) if dnet is not None else ZERO
            # fresh var to keep formula small
            fv = bv(f"q{di}_{cyc}")
            s.add(fv == nxt)
            nq.append(fv)
        q = nq
        succ_at[cyc + 1] = q[SUCC]  # state index after this update

    # want success high at one of eval_cycles
    s.add(z3.Or([succ_at[c] == ONE for c in eval_cycles if c in succ_at]))
    print("solving...", flush=True)
    r = s.check()
    print("result:", r)
    if r != z3.sat:
        return None
    m = s.model()
    bits = [1 if m.eval(I[t]) == ONE else 0 for t in range(n_bits)]
    # which eval cycle fired
    fired = [c for c in eval_cycles if c in succ_at and m.eval(succ_at[c]) == ONE]
    print("success high at cycles:", fired)
    print("input bits:", "".join(map(str, bits)))
    return bits


if __name__ == "__main__":
    bits = solve("puzzle_netlist.json")
    if bits:
        with open("solution_bits.txt", "w") as f:
            f.write("".join(map(str, bits)))
        print("saved solution_bits.txt")
