#!/usr/bin/env python3
"""Gate-level simulator for the extracted netlist. Cycle-accurate, with
async-reset/set flops and combinational settling by topological order."""
import json
import sys
from collections import defaultdict, deque

from cells import COMB, DFF, PHYS


class Netlist:
    def __init__(self, path):
        d = json.load(open(path))
        self.ports = d["ports"]
        self.insts = [i for i in d["insts"] if i["type"] not in PHYS]
        self.n_nets = d["n_nets"]

        # driver_of[net] -> tag
        self.driver = {}
        self.const_nets = {}
        self.comb = []     # (inst_id, type, outpin, inputs{pin:net})
        self.dffs = []     # dict: id,type,clk_net,d_net,q_net,rst_net,set_net
        self.conb = []
        for inst in self.insts:
            t = inst["type"]
            pins = inst["pins"]
            iid = inst["id"]
            if t == "conb_1":
                self.conb.append(inst)
                if pins.get("HI") is not None:
                    self.const_nets[pins["HI"]] = 1
                    self.driver[pins["HI"]] = ("const", 1)
                if pins.get("LO") is not None:
                    self.const_nets[pins["LO"]] = 0
                    self.driver[pins["LO"]] = ("const", 0)
            elif t in DFF:
                self.dffs.append({
                    "id": iid, "type": t, "kind": DFF[t],
                    "clk": pins.get("CLK"), "d": pins.get("D"), "q": pins.get("Q"),
                    "rst": pins.get("RESET_B"), "set": pins.get("SET_B"),
                })
                if pins.get("Q") is not None:
                    self.driver[pins["Q"]] = ("dff", len(self.dffs) - 1)
            elif t in COMB:
                out, fn = COMB[t]
                onet = pins.get(out)
                ins = {k: v for k, v in pins.items() if k != out}
                self.comb.append((iid, t, onet, ins))
                if onet is not None:
                    self.driver[onet] = ("comb", len(self.comb) - 1)
            else:
                raise RuntimeError(f"unknown cell type {t}")

        # ports whose nets nothing drives are primary inputs (clk, rst_n,
        # enable, I on the puzzle; A, B, en, ... on the warmup)
        self.in_ports = {}
        for name, net in self.ports.items():
            if name in ("VPWR", "VGND") or net is None:
                continue
            if net not in self.driver:
                self.in_ports[name] = net
                self.driver[net] = ("in", name)

        self._topo()

    def _topo(self):
        """order combinational gate indices so drivers precede loads."""
        deps = defaultdict(set)   # comb idx -> set of comb idx it depends on
        indeg = defaultdict(int)
        for ci, (iid, t, onet, ins) in enumerate(self.comb):
            for net in ins.values():
                d = self.driver.get(net)
                if d and d[0] == "comb":
                    deps[ci].add(d[1])
        loads = defaultdict(list)
        for ci, ds in deps.items():
            for d in ds:
                loads[d].append(ci)
        for ci in range(len(self.comb)):
            indeg[ci] = len(deps[ci])
        q = deque([ci for ci in range(len(self.comb)) if indeg[ci] == 0])
        order = []
        while q:
            ci = q.popleft()
            order.append(ci)
            for l in loads[ci]:
                indeg[l] -= 1
                if indeg[l] == 0:
                    q.append(l)
        self.cyclic = len(order) != len(self.comb)
        if self.cyclic:
            rest = [ci for ci in range(len(self.comb)) if ci not in set(order)]
            order += rest
            print(f"WARNING: combinational cycle among {len(rest)} gates", file=sys.stderr)
        self.order = order

    # ---- simulation ----
    def new_state(self):
        return {"nets": [0] * self.n_nets,
                "q": [0] * len(self.dffs),
                "clkprev": [0] * len(self.dffs)}

    def eval_comb(self, st, inputs):
        nets = st["nets"]
        for net, v in self.const_nets.items():
            nets[net] = v
        for name, net in self.in_ports.items():
            if net is not None and name in inputs:
                nets[net] = inputs[name]
        for di, dff in enumerate(self.dffs):
            if dff["q"] is not None:
                nets[dff["q"]] = st["q"][di]
        passes = 1 if not self.cyclic else 8
        for _ in range(passes):
            for ci in self.order:
                iid, t, onet, ins = self.comb[ci]
                if onet is None:
                    continue
                out, fn = COMB[t]
                p = {k: (nets[v] if v is not None else 0) for k, v in ins.items()}
                nets[onet] = fn(p)

    def step(self, st, inputs):
        """apply one set of input values; do edge + async updates; settle."""
        self.eval_comb(st, inputs)
        nets = st["nets"]
        newq = list(st["q"])
        for di, dff in enumerate(self.dffs):
            clk = nets[dff["clk"]] if dff["clk"] is not None else 0
            rising = (st["clkprev"][di] == 0 and clk == 1)
            st["clkprev"][di] = clk
            if dff["kind"] == "rtp" and dff["rst"] is not None and nets[dff["rst"]] == 0:
                newq[di] = 0
            elif dff["kind"] == "stp" and dff["set"] is not None and nets[dff["set"]] == 0:
                newq[di] = 1
            elif rising:
                newq[di] = nets[dff["d"]] if dff["d"] is not None else 0
        st["q"] = newq
        self.eval_comb(st, inputs)  # re-settle with new Q
        return st

    def read(self, st, name):
        net = self.ports.get(name)
        return st["nets"][net] if net is not None else None

    def read_bus(self, st, names):
        return [self.read(st, n) for n in names]


if __name__ == "__main__":
    nl = Netlist(sys.argv[1])
    print(f"comb gates: {len(nl.comb)}  dffs: {len(nl.dffs)}  conb: {len(nl.conb)}  cyclic={nl.cyclic}")
