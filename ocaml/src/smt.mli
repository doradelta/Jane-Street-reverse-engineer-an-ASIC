(** Bounded model checking of the netlist through SMT-LIB2 and the [z3]
    binary. *)

val playback_clocks : int
(** Clocks unrolled after the last input bit (2). *)

val formula : Sim.t -> n_bits:int -> post_reset:int array -> success:int -> string
(** The unrolled circuit: [n_bits] symbolic input clocks followed by two
    playback clocks, asserting that flop [success] (an index in [dffs]) is
    high after either of them. *)

val solve_unique : Sim.t -> n_bits:int -> post_reset:int array -> (string * bool, string) result
(** Find the input bits that raise success, then show no others exist:
    [Ok (bits, unique)]. Fails with a message when z3 is missing, the design
    lacks the puzzle's ports, or a flop's set/reset is not wired to rst_n. *)
