(** Gate-level netlist and cycle-accurate simulator. *)

module Dff : sig
  type t = {
    id : int;  (** instance id in the extracted netlist *)
    kind : Cells.dff_kind;
    clk : int option;
    d : int option;
    q : int option;
    rst : int option;  (** RESET_B *)
    set : int option;  (** SET_B *)
  }
end

module Comb : sig
  type t = {
    id : int;
    typ : string;
    out_net : int option;
    ins : (string * int option) list;
    fn : Cells.fn;
  }
end

type t = {
  n_nets : int;
  ports : (string * int) list;
  in_ports : (string * int) list;  (** ports nothing drives: the primary inputs *)
  consts : (int * int) list;       (** net, value *)
  comb : Comb.t array;
  dffs : Dff.t array;
  order : int array;               (** gate indices, drivers before loads *)
  cyclic : bool;
}

val of_extract : ?warn:(string -> unit) -> Extract.netlist -> t

type state = { nets : int array; q : int array; clkprev : int array }

val new_state : t -> state

val input_net : Comb.t -> string -> int option
(** The net on an input pin; fails if the netlist has no such pin. *)

val eval_comb : t -> state -> (string * int) list -> unit
(** Settle the combinational logic. Inputs are (port, value); a port not
    mentioned keeps its previous value. *)

val step : t -> state -> (string * int) list -> unit
(** Apply one set of input values: settle, clock edges and async set/reset,
    settle again with the new flop outputs. *)

val read : t -> state -> string -> int option
(** Value of a port, [None] if the design has no such port. *)

val read_byte : t -> state -> int option
(** O\[7:0\] as a byte, O\[0\] least significant; [None] if a bit is missing. *)

val find_dff : t -> id:int -> int option
(** Index in [dffs] of the flop with this instance id. *)

val flop_driving : t -> string -> int option
(** Index in [dffs] of the flop whose Q drives a port. *)
