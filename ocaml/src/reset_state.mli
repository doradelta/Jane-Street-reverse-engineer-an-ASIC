(** The post-reset flop state recorded by the Python solve. *)

type t = { ids : int list; init : int list }
(** Parallel lists: flop instance id, value after reset. *)

val read : string -> t
