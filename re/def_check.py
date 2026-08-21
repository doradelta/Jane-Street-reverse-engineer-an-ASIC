#!/usr/bin/env python3
"""Validate extracted netlist against the warmup DEF (ground truth)."""
import json
import re
import sys
from collections import defaultdict

import gdstk


def parse_def(path):
    txt = open(path).read()
    comps = {}
    m = re.search(r"COMPONENTS \d+ ;(.*?)END COMPONENTS", txt, re.S)
    for stmt in m.group(1).split(";"):
        cm = re.search(r"-\s+(\S+)\s+(\S+)\s+.*?PLACED\s+\(\s*(-?\d+)\s+(-?\d+)\s*\)\s+(\w+)", stmt, re.S)
        if cm:
            name, cell, x, y, orient = cm.groups()
            comps[name] = (cell, int(x) / 1000.0, int(y) / 1000.0, orient)
    nets = {}
    m = re.search(r"\nNETS \d+ ;(.*?)END NETS", txt, re.S)
    for stmt in m.group(1).split(";"):
        nm = re.search(r"-\s+(\S+)", stmt)
        if not nm:
            continue
        name = nm.group(1)
        conns = re.findall(r"\(\s*(\S+)\s+(\S+)\s*\)", stmt)
        nets[name] = [(c, p) for c, p in conns]
    return comps, nets


def main(def_path, gds_path, json_path):
    comps, nets = parse_def(def_path)
    ext = json.load(open(json_path))

    # cell sizes from GDS boundary layer (236/0)
    lib = gdstk.read_gds(gds_path)
    size = {}
    for c in lib.cells:
        if c.name.startswith("sky130"):
            bnd = [p for p in c.polygons if p.layer == 236 and p.datatype == 0]
            if bnd:
                pts = bnd[0].points
                size[c.name.split("__")[-1]] = (pts[:, 0].max() - pts[:, 0].min(),
                                                pts[:, 1].max() - pts[:, 1].min())

    # index extracted instances by (type, origin, rot, refl)
    by_key = {}
    for inst in ext["insts"]:
        key = (inst["type"], round(inst["x"], 3), round(inst["y"], 3), inst["rot"], inst["refl"])
        by_key[key] = inst

    def def2gds(cell, x, y, orient):
        w, h = size[cell]
        return {
            "N":  (x, y, 0, False),
            "S":  (x + w, y + h, 2, False),
            "FS": (x, y + h, 0, True),
            "FN": (x + w, y, 2, True),
        }[orient]

    comp2inst = {}
    missing = []
    for name, (cell, x, y, orient) in comps.items():
        st = cell.split("__")[-1]
        gx, gy, rot, refl = def2gds(st, x, y, orient)
        inst = by_key.get((st, round(gx, 3), round(gy, 3), rot, refl))
        if inst is None:
            missing.append((name, cell, x, y, orient))
        else:
            comp2inst[name] = inst
    print(f"DEF components: {len(comps)}, matched to GDS instances: {len(comp2inst)}, missing: {len(missing)}")
    for mrec in missing[:10]:
        print("  MISSING", mrec)

    # compare net partitions over signal pins
    ok = bad = 0
    mynet2defnet = {}
    for dnet, conns in nets.items():
        mine = set()
        for cname, pin in conns:
            if cname == "PIN":
                n = ext["ports"].get(pin)
                mine.add(("PORT" if n is None else n))
            else:
                inst = comp2inst.get(cname)
                if inst is None:
                    continue
                n = inst["pins"].get(pin, "ABSENT")
                mine.add(n)
        if len(mine) == 1:
            n = mine.pop()
            if n in mynet2defnet and mynet2defnet[n] != dnet:
                print(f"COLLISION: my net {n} maps to {mynet2defnet[n]} and {dnet}")
                bad += 1
            else:
                mynet2defnet[n] = dnet
                ok += 1
        else:
            print(f"SPLIT: DEF net {dnet} maps to my nets {mine}: {conns[:6]}")
            bad += 1
    print(f"nets consistent: {ok}, broken: {bad}")


if __name__ == "__main__":
    main(*sys.argv[1:4])
