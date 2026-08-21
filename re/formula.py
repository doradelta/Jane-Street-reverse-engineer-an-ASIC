#!/usr/bin/env python3
"""Expand a net into a boolean expression over flops/inputs/consts (depth-limited)."""
import sys
from collections import Counter
from analyze import load, out_pin, in_pins
from cells import COMB, DFF

d, insts, ports, driver, net_loads, net2port = load(sys.argv[1] if len(sys.argv) > 1 else "puzzle_netlist.json")

OPSTR = {
    "and2_2": "&", "and3_2": "&", "and4_2": "&",
    "or2_2": "|", "or3_2": "|", "or4_2": "|",
    "nand2_2": "nand", "nor2_2": "nor", "xor2_2": "^", "xnor2_2": "xnor",
}


def name_net(net):
    if net in net2port:
        return net2port[net]
    drv = driver.get(net)
    if drv is None:
        return f"net{net}(float)"
    iid, pin = drv
    t = insts[iid]["type"]
    if t in DFF:
        return f"FF{iid}"
    if t == "conb_1":
        return "1" if pin == "HI" else "0"
    return None  # comb, expand


def expr(net, depth, seen):
    nm = name_net(net)
    if nm is not None:
        return nm
    iid, pin = driver[net]
    t = insts[iid]["type"]
    st = t.replace("sky130_fd_sc_hd__", "")
    ip = in_pins(insts[iid])
    if depth <= 0:
        return f"g{iid}:{st}(...)"
    args = {k: expr(v, depth - 1, seen) for k, v in ip.items()}
    op = st
    # pretty a few
    def a(k): return args.get(k, "?")
    if st in ("and2_2", "and3_2", "and4_2"):
        return "(" + " & ".join(a(k) for k in ("A", "B", "C", "D") if k in ip) + ")"
    if st in ("or2_2", "or3_2", "or4_2"):
        return "(" + " | ".join(a(k) for k in ("A", "B", "C", "D") if k in ip) + ")"
    if st == "nand2_2":
        return f"!({a('A')} & {a('B')})"
    if st == "nor2_2":
        return f"!({a('A')} | {a('B')})"
    if st == "xor2_2":
        return f"({a('A')} ^ {a('B')})"
    if st == "xnor2_2":
        return f"({a('A')} == {a('B')})"
    if st == "inv_2":
        return f"!{a('A')}"
    if st in ("buf_2", "clkbuf_4", "clkbuf_8", "clkbuf_16"):
        return a("A")
    return f"{st}({', '.join(f'{k}={v}' for k,v in args.items())})"


if __name__ == "__main__":
    net = ports["success"]
    iid, pin = driver[net]
    dnet = insts[iid]["pins"]["D"]
    print("success = FF", iid, " D-net:", dnet)
    for dep in (2, 3, 4):
        print(f"\n--- depth {dep} ---")
        print("success.D =", expr(dnet, dep, set()))
