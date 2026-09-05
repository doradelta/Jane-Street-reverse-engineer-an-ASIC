(** Small file and string helpers shared by the other modules. *)

val read_file : string -> string
(** Whole file as a string; the channel is closed even on error. *)

val write_file : string -> string -> unit

val lines : string -> string list
(** Lines, each trimmed (so Windows line ends are harmless). *)

val words : string -> string list
(** Split on runs of blanks, tabs and line ends. *)

val fail : ('a, unit, string, 'b) format4 -> 'a
(** [failwith] with a format string. *)
