#!/usr/bin/env python3
"""First-pass exploration of the puzzle/warmup GDS files."""
import sys
from collections import Counter

import gdstk


def explore(path):
    print(f"##### {path}")
    lib = gdstk.read_gds(path)
    print(f"library: {lib.name!r}  unit={lib.unit}  precision={lib.precision}")
    cells = {c.name: c for c in lib.cells}
    print(f"total structures: {len(cells)}")

    top = lib.top_level()
    print(f"top-level structures: {[c.name for c in top]}")

    for t in top:
        print(f"\n=== top cell {t.name!r} ===")
        inst = Counter()
        for ref in t.references:
            name = ref.cell.name if hasattr(ref.cell, "name") else str(ref.cell)
            inst[name] += 1
        print(f"instances: {sum(inst.values())} total, {len(inst)} distinct")
        for name, n in inst.most_common():
            print(f"  {n:5d}  {name}")

        lay = Counter()
        for p in t.polygons:
            lay[(p.layer, p.datatype)] += 1
        for p in t.paths:
            for l, d in zip(p.layers, p.datatypes):
                lay[(l, d)] += 1
        print("top-cell own polygons/paths per (layer,datatype):")
        for (l, d), n in sorted(lay.items()):
            print(f"  layer {l:3d}/{d:<2d}: {n}")

        print("labels in top cell:")
        for lb in t.labels:
            print(f"  layer {lb.layer}/{lb.texttype} at {lb.origin}: {lb.text!r}")

        (x0, y0), (x1, y1) = t.bounding_box()
        print(f"bbox: ({x0:.3f},{y0:.3f}) - ({x1:.3f},{y1:.3f}) um")


if __name__ == "__main__":
    for p in sys.argv[1:]:
        explore(p)
