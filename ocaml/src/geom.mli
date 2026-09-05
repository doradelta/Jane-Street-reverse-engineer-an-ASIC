(** Exact integer geometry on axis-aligned rectangles. Intervals are closed:
    rectangles that merely touch intersect. *)

type rect = { x0 : int; y0 : int; x1 : int; y1 : int }

val make : int -> int -> int -> int -> rect
(** [make xa ya xb yb], corners in any order. *)

val intersects : rect -> rect -> bool
val area : rect -> int
val inter_area : rect -> rect -> int

val dist2 : rect -> Gds.point -> int
(** Squared distance from a point to a rectangle, 0 inside. *)

val dist2_to_rects : rect list -> Gds.point -> int

val of_path : Gds.Path.t -> rect
(** A straight two-point Manhattan path with its end extensions. Fails on
    anything else, including round-ended (pathtype 1) paths. *)

val of_polygon : Gds.point array -> rect list
(** A rectilinear polygon as rectangles with disjoint interiors. *)

val transform : reflect:bool -> turns:int -> origin:Gds.point -> rect -> rect
(** Mirror about the x axis (if [reflect]), rotate by [turns] quarter turns,
    then translate: the placement of a cell reference. *)

(** Uniform grid over a fixed set of rectangles. *)
type index

val build : pitch:int -> rect list -> index
val query : index -> rect -> (int -> unit) -> unit
(** Calls [f i] once for every stored rectangle [i] intersecting the query. *)

module Dsu : sig
  type t

  val create : int -> t
  val find : t -> int -> int
  val union : t -> int -> int -> unit
end
