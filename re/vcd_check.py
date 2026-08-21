#!/usr/bin/env python3
"""Replay example_inputs.vcd through the extracted netlist and compare the
design outputs (O[7:0], success) against the golden VCD at every timestamp.

Timing convention: all changes at a timestamp are applied before the step, so
an input changing on the same timestamp as a rising clk edge is sampled at its
NEW value (zero-delay). Both shipped VCDs drive inputs on falling-edge
timestamps only, where the two conventions agree."""
import re
import sys
from sim import Netlist


def parse_vcd(path):
    id2sig = {}
    width = {}
    with open(path) as f:
        lines = f.read().splitlines()
    i = 0
    while i < len(lines):
        L = lines[i].strip()
        if L.startswith("$var"):
            # $var reg 1 ! clk $end   /  $var wire 8 % O [7:0] $end
            parts = L.split()
            w = int(parts[2]); ident = parts[3]; name = parts[4]
            id2sig[ident] = name
            width[ident] = w
        if L.startswith("$enddefinitions"):
            i += 1
            break
        i += 1
    events = []  # (time, {ident: value})
    cur_t = None
    cur = {}
    for L in lines[i:]:
        L = L.strip()
        if not L or L.startswith("$"):
            continue
        if L[0] == "#":
            if cur_t is not None:
                events.append((cur_t, cur))
            cur_t = int(L[1:]); cur = {}
        elif L[0] == "b":
            val, ident = L[1:].split()
            cur[ident] = val
        else:
            # scalar change: value char followed by ident
            cur[L[1:]] = L[0]
    if cur_t is not None:
        events.append((cur_t, cur))
    return id2sig, width, events


def intval(vec):
    """binary vector string -> int, or None if it holds x/z bits."""
    vec = vec.lower()
    if "x" in vec or "z" in vec:
        return None
    return int(vec, 2)


def main(netpath, vcdpath):
    nl = Netlist(netpath)
    id2sig, width, events = parse_vcd(vcdpath)
    sig2id = {v: k for k, v in id2sig.items()}
    st = nl.new_state()
    inputs = {"clk": 0, "rst_n": 0, "enable": 0, "I": 0}

    total = ok = 0
    mism = []
    cur = {}
    for t, ev in events:
        cur.update(ev)
        # set inputs
        for name in ("clk", "rst_n", "I"):
            if name in sig2id and sig2id[name] in cur:
                v = cur[sig2id[name]]
                if v in "01":
                    inputs[name] = int(v)
        # enable ident is '#', signal name 'enable'
        if "enable" in sig2id and sig2id["enable"] in cur:
            v = cur[sig2id["enable"]]
            if v in "01":
                inputs["enable"] = int(v)
        nl.step(st, inputs)
        # compare outputs
        # O bus
        exp_O = cur.get(sig2id["O"]) if "O" in sig2id else None
        got_O = nl.read_bus(st, [f"O[{b}]" for b in range(8)])  # LSB..MSB
        got_val = None
        if all(g is not None for g in got_O):
            got_val = sum(g << b for b, g in enumerate(got_O))
        exp_val = intval(exp_O) if exp_O is not None else None
        exp_s = cur.get(sig2id["success"]) if "success" in sig2id else None
        got_s = nl.read(st, "success")

        if exp_val is not None:
            total += 1
            match_o = (got_val == exp_val)
            match_s = (got_s == int(exp_s)) if exp_s in ("0", "1") else True
            if match_o and match_s:
                ok += 1
            else:
                mism.append((t, exp_val, got_val, exp_s, got_s))
    print(f"comparisons: {total}, matched: {ok}, mismatched: {len(mism)}")
    for t, ev, gv, es, gs in mism[:30]:
        print(f"  t={t}: O expected {ev:08b}({ev}) got {gv if gv is None else format(gv,'08b')+f'({gv})'}  success exp {es} got {gs}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
