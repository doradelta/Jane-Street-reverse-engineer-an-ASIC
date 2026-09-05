(** Comparison of an extracted netlist against a DEF file (place-and-route
    ground truth). *)

type result = {
  components : int;      (** DEF components *)
  matched : int;         (** of which found as GDS instances at the same place *)
  missing : string list;
  consistent : int;      (** DEF nets that map to exactly one of ours, one to one *)
  broken : int;
  problems : string list;
}

val check : def:string -> lib:Gds.library -> Extract.netlist -> result
