#!/usr/bin/env python3
"""Structural analysis helpers: backward cones, flop fan-in, subcircuit ID."""
import json
import sys
from collections import defaultdict, deque

from cells import COMB, DFF, PHYS


def load(path):
    d = json.load(open(path))
    insts = {i["id"]: i for i in d["insts"] if i["type"] not in PHYS}
    ports = d["ports"]
    # driver of each net
    driver = {}
    net_loads = defaultdict(list)  # net -> list of (inst_id, pin)
    for i in insts.values():
        t = i["type"]
        if t == "conb_1":
            for pin in ("HI", "LO"):
                if i["pins"].get(pin) is not None:
                    driver[i["pins"][pin]] = (i["id"], pin)
        elif t in DFF:
            if i["pins"].get("Q") is not None:
                driver[i["pins"]["Q"]] = (i["id"], "Q")
        elif t in COMB:
            out = COMB[t][0]
            if i["pins"].get(out) is not None:
                driver[i["pins"][out]] = (i["id"], out)
        for pin, net in i["pins"].items():
            if net is not None:
                net_loads[net].append((i["id"], pin))
    # invert ports
    net2port = {v: k for k, v in ports.items()}
    return d, insts, ports, driver, net_loads, net2port


def out_pin(t):
    if t == "conb_1":
        return None
    if t in DFF:
        return "Q"
    return COMB[t][0]


def in_pins(inst):
    t = inst["type"]
    op = out_pin(t)
    return {p: n for p, n in inst["pins"].items() if p != op}


def cone(start_nets, insts, ports, driver, net2port, stop_at_flops=True):
    """Backward reachable instances from start_nets. Returns (comb_ids, flop_ids, input_ports, const_ids)."""
    seen_nets = set()
    comb_ids, flop_ids, inports, const_ids = set(), set(), set(), set()
    q = deque(start_nets)
    while q:
        net = q.popleft()
        if net is None or net in seen_nets:
            continue
        seen_nets.add(net)
        if net in net2port and net2port[net] in ("clk", "rst_n", "enable", "I"):
            inports.add(net2port[net])
            continue
        drv = driver.get(net)
        if drv is None:
            continue
        iid, pin = drv
        t = insts[iid]["type"]
        if t == "conb_1":
            const_ids.add(iid); continue
        if t in DFF:
            flop_ids.add(iid)
            if not stop_at_flops:
                for p, n in in_pins(insts[iid]).items():
                    q.append(n)
            continue
        comb_ids.add(iid)
        for p, n in in_pins(insts[iid]).items():
            q.append(n)
    return comb_ids, flop_ids, inports, const_ids


if __name__ == "__main__":
    path = sys.argv[1]
    d, insts, ports, driver, net_loads, net2port = load(path)
    targets = sys.argv[2:] or ["success"] + [f"O[{i}]" for i in range(8)]
    for tgt in targets:
        net = ports.get(tgt)
        c, f, ip, k = cone([net], insts, ports, driver, net2port)
        print(f"{tgt}: comb={len(c)} flops={len(f)} inputs={sorted(ip)} consts={len(k)}")
