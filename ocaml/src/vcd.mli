(** Minimal VCD reader and waveform replay against the simulator. *)

type t = {
  id_of_name : (string * string) list;  (** signal name -> identifier, last declaration first *)
  events : (int * (string * string) list) list;  (** time, (identifier, value) *)
}

val parse : string -> t
(** Fails on a file without a [$enddefinitions] section. *)

type mismatch = {
  time : int;
  expected_o : int;
  got_o : int option;
  expected_success : string option;
  got_success : int option;
}

type replay = { total : int; matched : int; mismatches : mismatch list }

val replay : Sim.t -> t -> replay
(** Drive clk, rst_n, enable and I from the waveform and compare O\[7:0\] and
    success wherever the waveform records a definite O value. *)
