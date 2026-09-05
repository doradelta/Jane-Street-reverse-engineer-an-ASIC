(** Geometry to netlist: union-find over every metal shape, via pad and placed
    cell-pin shape that touches on the same layer. *)

val short_type : string -> string
(** ["sky130_fd_sc_hd__nand2_2"] -> ["nand2_2"]. *)

val is_power : string -> bool
(** VPWR, VGND, VPB, VNB: cell pins with no signal function. *)

val is_rail : string -> bool
(** VPWR, VGND: the top-level power ports. *)

type 'pins inst = {
  id : int;
  typ : string;  (** short cell type *)
  x : int;       (** origin, nanometres *)
  y : int;
  rot : int;     (** quarter turns *)
  refl : bool;
  pins : 'pins;
}

type netlist = {
  top : string;
  insts : (string * int option) list inst list;  (** pin -> net, [None] when dangling *)
  ports : (string * int) list;
  n_nets : int;
}

val extract : ?log:(string -> unit) -> Gds.library -> netlist
(** Requires a 1 nm database unit, a single top cell and unit magnification. *)

val to_json : netlist -> string
(** The schema written by the Python extractor (coordinates in micrometres). *)
