#!/usr/bin/env python3
"""GDS -> netlist extractor for the Jane Street ASIC puzzle.

Approach: the GDS keeps standard cells as named references and routing as
top-level polygons plus via cells. Signal pins of every sky130 cell are the
labeled li1 shapes inside the cell definition. So connectivity is pure
geometry:
  - collect all top-level metal shapes (li1..met5, drawing+pin datatypes)
  - collect via-cell instances (each contributes pads on two layers which are
    trivially connected through the via)
  - collect per-instance transformed pin shapes on li1
  - union-find everything that touches on the same layer
"""
import json
import math
import sys
from collections import Counter, defaultdict

import numpy as np
import gdstk
from shapely.geometry import Polygon as SPoly, Point
from shapely.strtree import STRtree

LI1 = 67
METALS = (67, 68, 69, 70, 71, 72)  # li1, met1..met5
DRAW = 20
PIN = 16
LABEL = 5

# cells with no signal function
PHYS_CELLS = {"tapvpwrvgnd_1", "decap_3", "fill_1", "fill_2", "fill_4", "fill_8"}
POWER_PINS = {"VPWR", "VGND", "VPB", "VNB"}


def short_type(name):
    return name.split("__")[-1] if "__" in name else name


class DSU:
    def __init__(self):
        self.p = {}

    def find(self, x):
        p = self.p
        while p.setdefault(x, x) != x:
            p[x] = p[p[x]]
            x = p[x]
        return x

    def union(self, a, b):
        ra, rb = self.find(a), self.find(b)
        if ra != rb:
            self.p[ra] = rb


def cell_polys(cell, layer, dts):
    """polygons of `cell` on (layer, dt in dts) as numpy point arrays."""
    out = [p.points for p in cell.polygons if p.layer == layer and p.datatype in dts]
    for path in cell.paths:
        for pp in path.to_polygons():
            if pp.layer == layer and pp.datatype in dts:
                out.append(pp.points)
    return out


POLY, MET1 = 66, 68
LICON, MCON = (66, 44), (67, 44)


def build_pinmap(cell):
    """Extract per-pin routable geometry from a stdcell definition.

    A signal pin's routable copper can appear on BOTH li1 (67/20) and met1
    (68/20), and a single pin's li1 may be several islands joined only through
    the poly gate (li1->licon->poly->licon->li1) or through met1 (li1->mcon->
    met1->mcon->li1). Model the intra-cell graph over li1/met1/poly nodes with
    mcon and poly-licon contacts, find the component carrying each pin label,
    and return that component's shapes keyed by layer. The router may land on
    either layer, so emitting both is what makes top-level connectivity exact.
    """
    li = cell_polys(cell, LI1, (DRAW, PIN))
    m1 = cell_polys(cell, MET1, (DRAW, PIN))
    po = cell_polys(cell, POLY, (DRAW,))
    li_g = [SPoly(p) for p in li]
    m1_g = [SPoly(p) for p in m1]
    po_g = [SPoly(p) for p in po]
    licons = [SPoly(p) for p in cell_polys(cell, LICON[0], (LICON[1],))]
    mcons = [SPoly(p) for p in cell_polys(cell, MCON[0], (MCON[1],))]

    dsu = DSU()
    li_t = STRtree(li_g) if li_g else None
    m1_t = STRtree(m1_g) if m1_g else None
    po_t = STRtree(po_g) if po_g else None

    def within_layer(geoms, tree, tag):
        for i, g in enumerate(geoms):
            for j in tree.query(g, predicate="intersects"):
                dsu.union((tag, i), (tag, int(j)))
    if li_t: within_layer(li_g, li_t, "li")
    if m1_t: within_layer(m1_g, m1_t, "m1")
    if po_t: within_layer(po_g, po_t, "po")

    # mcon: li1 <-> met1
    for c in mcons:
        a = [int(j) for j in li_t.query(c, predicate="intersects")] if li_t else []
        b = [int(j) for j in m1_t.query(c, predicate="intersects")] if m1_t else []
        nodes = [("li", j) for j in a] + [("m1", j) for j in b]
        for n in nodes[1:]:
            dsu.union(nodes[0], n)
    # licon on poly: li1 <-> poly (skip diffusion contacts)
    for c in licons:
        on_poly = [int(j) for j in po_t.query(c, predicate="intersects")
                   if po_g[int(j)].intersection(c).area > 0.4 * c.area] if po_t else []
        if not on_poly:
            continue
        a = [int(j) for j in li_t.query(c, predicate="intersects")] if li_t else []
        nodes = [("po", j) for j in on_poly] + [("li", j) for j in a]
        for n in nodes[1:]:
            dsu.union(nodes[0], n)

    # attach labels (li1 label points)
    labels = [(lb.text, Point(lb.origin)) for lb in cell.labels
              if lb.layer == LI1 and lb.texttype == LABEL]
    root_name = {}
    for text, pt in labels:
        hit = None
        if li_t:
            for j in li_t.query(pt.buffer(0.01), predicate="intersects"):
                if li_g[int(j)].distance(pt) < 0.02:
                    hit = ("li", int(j)); break
        if hit is None:
            continue
        r = dsu.find(hit)
        if r in root_name and root_name[r] != text:
            raise RuntimeError(f"{cell.name}: component holds {root_name[r]} and {text}")
        root_name[r] = text

    pins = defaultdict(lambda: {LI1: [], MET1: []})
    for i, g in enumerate(li_g):
        r = dsu.find(("li", i))
        if r in root_name:
            pins[root_name[r]][LI1].append(li[i])
    for i, g in enumerate(m1_g):
        r = dsu.find(("m1", i))
        if r in root_name:
            pins[root_name[r]][MET1].append(m1[i])
    return {k: v for k, v in pins.items()}


def ref_transform(ref):
    """Return (matrix2x2, offset) mapping cell coords to top coords."""
    rot = ref.rotation or 0.0
    k = round(rot / (math.pi / 2)) % 4
    c, s = [(1, 0), (0, 1), (-1, 0), (0, -1)][k]
    R = np.array([[c, -s], [s, c]], dtype=float)
    S = np.array([[1, 0], [0, -1]], dtype=float) if ref.x_reflection else np.eye(2)
    M = R @ S
    if ref.magnification not in (None, 1.0):
        M = M * ref.magnification
    return M, np.asarray(ref.origin, dtype=float)


def extract(gds_path, out_json):
    lib = gdstk.read_gds(gds_path)
    cells = {c.name: c for c in lib.cells}
    top = lib.top_level()[0]
    print(f"top cell: {top.name}")

    # --- per-celltype pin maps ---
    pinmaps = {}
    via_defs = {}
    for name, cell in cells.items():
        if name.startswith("VIA_"):
            via_defs[name] = {
                ly: cell_polys(cell, ly, (DRAW,)) for ly in METALS
            }
        elif name.startswith("sky130"):
            pinmaps[name] = build_pinmap(cell)

    # --- collect shapes per layer: (points, owner) ---
    layer_shapes = defaultdict(list)  # layer -> list of (SPoly, owner)

    # top-level routing (drawing + pin purpose)
    for p in top.polygons:
        if p.layer in METALS and p.datatype in (DRAW, PIN):
            layer_shapes[p.layer].append((SPoly(p.points), ("w", id(p))))
    for path in top.paths:
        for pp in path.to_polygons():
            if pp.layer in METALS and pp.datatype in (DRAW, PIN):
                layer_shapes[pp.layer].append((SPoly(pp.points), ("w", id(pp))))

    # instances
    insts = []
    n_via = 0
    for ref in top.references:
        cname = ref.cell.name
        M, o = ref_transform(ref)
        if cname.startswith("VIA_"):
            owner = ("v", n_via)
            n_via += 1
            for ly, polys in via_defs[cname].items():
                for pts in polys:
                    layer_shapes[ly].append((SPoly(pts @ M.T + o), owner))
        elif cname.startswith("sky130"):
            st = short_type(cname)
            iid = len(insts)
            insts.append({
                "id": iid, "type": st,
                "x": round(float(o[0]), 4), "y": round(float(o[1]), 4),
                "rot": round((ref.rotation or 0) / (math.pi / 2)) % 4,
                "refl": bool(ref.x_reflection),
                "pins": {},
            })
            for pin, bylayer in pinmaps[cname].items():
                if pin in POWER_PINS:
                    continue
                owner = ("p", iid, pin)
                for ly, polys in bylayer.items():
                    for pts in polys:
                        layer_shapes[ly].append((SPoly(pts @ M.T + o), owner))
        # INTERNAL_* cells: no geometry on real layers -> skip

    # --- union-find over touching shapes per layer ---
    dsu = DSU()
    all_owners = set()
    for ly, shapes in layer_shapes.items():
        geoms = [g for g, _ in shapes]
        tree = STRtree(geoms)
        for i, (g, owner) in enumerate(shapes):
            all_owners.add(owner)
            for j in tree.query(g, predicate="intersects"):
                dsu.union(owner, shapes[int(j)][1])
        print(f"layer {ly}: {len(shapes)} shapes")
    group_size = Counter(dsu.find(o) for o in all_owners)

    # --- assign nets ---
    netid = {}

    def net_of(owner):
        r = dsu.find(owner)
        if r not in netid:
            netid[r] = len(netid)
        return netid[r]

    for inst in insts:
        cname = "sky130_fd_sc_hd__" + inst["type"]
        for pin in pinmaps[cname]:
            if pin in POWER_PINS:
                continue
            owner = ("p", inst["id"], pin)
            if group_size[dsu.find(owner)] <= 1:
                # never touched anything: dangling pin
                inst["pins"][pin] = None
            else:
                inst["pins"][pin] = net_of(owner)

    # --- ports from top-level labels ---
    ports = {}
    for lb in top.labels:
        if lb.texttype != LABEL or lb.layer not in METALS:
            continue
        pt = Point(lb.origin)
        best = None
        for g, owner in layer_shapes[lb.layer]:
            if g.distance(pt) < 0.005:
                best = owner
                break
        if best is None:
            print(f"WARNING: no shape for label {lb.text}")
            continue
        if lb.text in ("VPWR", "VGND"):
            ports.setdefault(lb.text, net_of(best))
        else:
            ports[lb.text] = net_of(best)

    result = {
        "top": top.name,
        "insts": insts,
        "ports": ports,
        "n_nets": len(netid),
    }
    with open(out_json, "w") as f:
        json.dump(result, f)
    logic = [i for i in insts if i["type"] not in PHYS_CELLS]
    print(f"instances: {len(insts)} ({len(logic)} with signal pins)")
    print(f"nets: {len(netid)}  ports: {ports}")
    return result


if __name__ == "__main__":
    extract(sys.argv[1], sys.argv[2])
