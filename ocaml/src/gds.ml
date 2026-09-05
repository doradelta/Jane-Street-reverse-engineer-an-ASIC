(* GDSII stream reader.

   Reads what a layout extractor needs: cells with their boundaries, paths,
   cell references and text labels. Coordinates stay in the integer database
   units of the file (1 nm for the puzzle), so later geometry is exact. Array
   references (AREF) and BOX elements are rejected rather than silently
   dropped; the puzzle files contain neither. *)

type point = int * int

module Polygon = struct
  type t = { layer : int; datatype : int; points : point array }
end

module Path = struct
  type t = {
    layer : int;
    datatype : int;
    width : int;
    pathtype : int;  (* 0 flush ends, 1 round, 2 square extended by width/2, 4 custom *)
    bgn_extn : int;
    end_extn : int;
    points : point array;
  }
end

module Reference = struct
  type t = {
    cell : string;
    origin : point;
    reflect : bool;  (* mirror about the x axis, applied before the rotation *)
    angle : float;   (* degrees, counter-clockwise *)
    mag : float;
  }
end

module Text = struct
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

(* --- record decoding --------------------------------------------------- *)

let u8 s i = Char.code (String.get s i)
let u16 s i = (u8 s i lsl 8) lor u8 s (i + 1)

let i16 s i =
  let v = u16 s i in
  if v >= 0x8000 then v - 0x10000 else v

let i32 s i =
  let v = (u16 s i lsl 16) lor u16 s (i + 2) in
  if v >= 0x8000_0000 then v - 0x1_0000_0000 else v

(* GDSII 8-byte real: sign bit, 7-bit excess-64 exponent (base 16), 56-bit
   mantissa with the binary point on the left *)
let real8 s i =
  let b0 = u8 s i in
  let e = (b0 land 0x7f) - 64 in
  let m = ref 0.0 in
  for k = 1 to 7 do
    m := (!m *. 256.0) +. float_of_int (u8 s (i + k))
  done;
  let v = !m /. (2.0 ** 56.0) *. (16.0 ** float_of_int e) in
  if b0 land 0x80 <> 0 then -.v else v

let ascii s i len =
  let str = String.sub s i len in
  match String.index_opt str '\000' with Some k -> String.sub str 0 k | None -> str

let points_of_xy s i len =
  Array.init (len / 8) (fun k -> (i32 s (i + (8 * k)), i32 s (i + (8 * k) + 4)))

(* the element being read; one record cleared in place *)
type element = No_element | Boundary | Path_element | Sref | Text_element

type pending = {
  mutable element : element;
  mutable layer : int;
  mutable datatype : int;
  mutable texttype : int;
  mutable width : int;
  mutable pathtype : int;
  mutable bgn_extn : int;
  mutable end_extn : int;
  mutable xy : point array;
  mutable sname : string;
  mutable string : string;
  mutable reflect : bool;
  mutable mag : float;
  mutable angle : float;
}

let clear e =
  e.element <- No_element;
  e.layer <- 0;
  e.datatype <- 0;
  e.texttype <- 0;
  e.width <- 0;
  e.pathtype <- 0;
  e.bgn_extn <- 0;
  e.end_extn <- 0;
  e.xy <- [||];
  e.sname <- "";
  e.string <- "";
  e.reflect <- false;
  e.mag <- 1.0;
  e.angle <- 0.0

let read path : library =
  let s = Util.read_file path in
  let n = String.length s in
  if n < 4 || u16 s 0 < 4 || u8 s 2 <> 0 then
    Util.fail "%s: not a GDSII stream (no HEADER record)" path;
  let db_unit_um = ref 0.001 in
  let cells = ref [] in
  let name = ref "" and polygons = ref [] and paths = ref [] in
  let references = ref [] and texts = ref [] in
  let e =
    { element = No_element; layer = 0; datatype = 0; texttype = 0; width = 0; pathtype = 0;
      bgn_extn = 0; end_extn = 0; xy = [||]; sname = ""; string = ""; reflect = false;
      mag = 1.0; angle = 0.0 }
  in
  let single_point what =
    if Array.length e.xy <> 1 then Util.fail "%s: %s without a single XY point" path what;
    e.xy.(0)
  in
  let finish_element () =
    (match e.element with
    | No_element -> ()
    | Boundary ->
        let pts = e.xy in
        let k = Array.length pts in
        (* GDS repeats the first vertex at the end; drop it *)
        let points = if k > 1 && pts.(0) = pts.(k - 1) then Array.sub pts 0 (k - 1) else pts in
        polygons := { Polygon.layer = e.layer; datatype = e.datatype; points } :: !polygons
    | Path_element ->
        paths :=
          { Path.layer = e.layer; datatype = e.datatype; width = e.width; pathtype = e.pathtype;
            bgn_extn = e.bgn_extn; end_extn = e.end_extn; points = e.xy }
          :: !paths
    | Sref ->
        references :=
          { Reference.cell = e.sname; origin = single_point "SREF"; reflect = e.reflect;
            angle = e.angle; mag = e.mag }
          :: !references
    | Text_element ->
        texts :=
          { Text.layer = e.layer; texttype = e.texttype; text = e.string;
            origin = single_point "TEXT" }
          :: !texts);
    clear e
  in
  let pos = ref 0 and ended = ref false in
  while not !ended do
    if !pos + 4 > n then Util.fail "%s: truncated at offset %d (no ENDLIB)" path !pos;
    let len = u16 s !pos in
    if len < 4 || !pos + len > n then
      Util.fail "%s: bad record length %d at offset %d" path len !pos;
    let tag = u8 s (!pos + 2) and body = !pos + 4 and blen = len - 4 in
    (match tag with
    | 3 (* UNITS: user unit, then database unit in metres *) ->
        db_unit_um := real8 s (body + 8) *. 1e6
    | 4 (* ENDLIB *) -> ended := true
    | 5 (* BGNSTR *) ->
        name := "";
        polygons := [];
        paths := [];
        references := [];
        texts := []
    | 6 (* STRNAME *) -> name := ascii s body blen
    | 7 (* ENDSTR *) ->
        cells :=
          { name = !name; polygons = List.rev !polygons; paths = List.rev !paths;
            references = List.rev !references; texts = List.rev !texts }
          :: !cells
    | 8 (* BOUNDARY *) -> clear e; e.element <- Boundary
    | 9 (* PATH *) -> clear e; e.element <- Path_element
    | 10 (* SREF *) -> clear e; e.element <- Sref
    | 12 (* TEXT *) -> clear e; e.element <- Text_element
    | 11 (* AREF *) -> Util.fail "%s: AREF elements are not supported" path
    | 45 (* BOX *) -> Util.fail "%s: BOX elements are not supported" path
    | 13 (* LAYER *) -> e.layer <- i16 s body
    | 14 (* DATATYPE *) -> e.datatype <- i16 s body
    | 15 (* WIDTH *) -> e.width <- i32 s body
    | 16 (* XY *) -> e.xy <- points_of_xy s body blen
    | 17 (* ENDEL *) -> finish_element ()
    | 18 (* SNAME *) -> e.sname <- ascii s body blen
    | 22 (* TEXTTYPE *) -> e.texttype <- i16 s body
    | 25 (* STRING *) -> e.string <- ascii s body blen
    | 26 (* STRANS *) -> e.reflect <- u16 s body land 0x8000 <> 0
    | 27 (* MAG *) -> e.mag <- real8 s body
    | 28 (* ANGLE *) -> e.angle <- real8 s body
    | 33 (* PATHTYPE *) -> e.pathtype <- i16 s body
    | 48 (* BGNEXTN *) -> e.bgn_extn <- i32 s body
    | 49 (* ENDEXTN *) -> e.end_extn <- i32 s body
    | _ -> () (* HEADER, BGNLIB, LIBNAME, PRESENTATION, properties, ... *));
    pos := !pos + len
  done;
  { db_unit_um = !db_unit_um; cells = List.rev !cells }

let top_cells lib =
  let referenced = Hashtbl.create 64 in
  List.iter
    (fun c -> List.iter (fun (r : Reference.t) -> Hashtbl.replace referenced r.cell ()) c.references)
    lib.cells;
  List.filter (fun c -> not (Hashtbl.mem referenced c.name)) lib.cells

let quarter_turns (r : Reference.t) =
  let q = r.angle /. 90.0 in
  if Float.abs (q -. Float.round q) > 1e-9 then
    Util.fail "reference to %s rotated by %g degrees; only multiples of 90 are supported" r.cell
      r.angle;
  int_of_float (Float.round q) land 3
