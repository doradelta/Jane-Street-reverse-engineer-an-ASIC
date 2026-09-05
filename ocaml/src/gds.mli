(** GDSII stream reader: cells with boundaries, paths, references and labels,
    in the integer database units of the file. *)

type point = int * int

module Polygon : sig
  type t = { layer : int; datatype : int; points : point array }
  (** [points] does not repeat the first vertex at the end. *)
end

module Path : sig
  type t = {
    layer : int;
    datatype : int;
    width : int;
    pathtype : int;  (** 0 flush ends, 1 round, 2 square extended by width/2, 4 custom *)
    bgn_extn : int;
    end_extn : int;
    points : point array;
  }
end

module Reference : sig
  type t = {
    cell : string;
    origin : point;
    reflect : bool;  (** mirror about the x axis, applied before the rotation *)
    angle : float;   (** degrees, counter-clockwise *)
    mag : float;
  }
end

module Text : sig
  type t = { layer : int; texttype : int; text : string; origin : point }
end

type cell = {
  name : string;
  polygons : Polygon.t list;
  paths : Path.t list;
  references : Reference.t list;
  texts : Text.t list;
}

type library = { db_unit_um : float; cells : cell list }

val read : string -> library
(** Fails (with [Failure]) on a file that is not a GDSII stream, is truncated,
    or uses AREF or BOX elements. *)

val top_cells : library -> cell list
(** Cells no other cell references, in file order. *)

val quarter_turns : Reference.t -> int
(** Rotation as 0..3 quarter turns; fails on angles that are not multiples of 90. *)
