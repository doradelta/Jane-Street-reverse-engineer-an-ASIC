(** Boolean models of the sky130_fd_sc_hd cells, written once against an
    abstract algebra so that simulation and SMT emission share them. *)

type 'a alg = {
  const : bool -> 'a;
  not_ : 'a -> 'a;
  and_ : 'a -> 'a -> 'a;
  or_ : 'a -> 'a -> 'a;
  xor_ : 'a -> 'a -> 'a;
  ite : 'a -> 'a -> 'a -> 'a;  (** select, if-true, if-false *)
}

type fn = { eval : 'a. 'a alg -> (string -> 'a) -> 'a }
(** A cell function; the callback yields the value of an input pin. *)

type dff_kind =
  | Rtp  (** async active-low reset, posedge *)
  | Stp  (** async active-low set, posedge *)
  | Xtp  (** plain posedge *)

type kind =
  | Comb of string * fn  (** output pin, function *)
  | Dff of dff_kind
  | Conb                 (** constant tie cell: HI = 1, LO = 0 *)
  | Phys                 (** no signal function *)

val table : (string * kind) list
(** Every cell type by its short name: 62 gates, the tie cell, 3 flops and
    7 physical-only cells. *)

val kind : string -> kind
(** Fails on a cell type that is not in the table. *)

val ints : int alg
(** Plain 0/1 integers. *)
