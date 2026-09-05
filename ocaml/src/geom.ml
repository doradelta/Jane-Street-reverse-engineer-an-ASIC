(* Integer rectangle geometry.

   Everything in the puzzle layout is Manhattan: routing paths are straight
   two-point segments and every polygon is rectilinear, so each shape becomes
   a small set of axis-aligned rectangles and every test below is exact
   integer arithmetic. Intervals are closed: shapes that merely touch count as
   intersecting, matching shapely's `intersects` in the Python version. *)

type rect = { x0 : int; y0 : int; x1 : int; y1 : int }

let make xa ya xb yb = { x0 = min xa xb; y0 = min ya yb; x1 = max xa xb; y1 = max ya yb }
let intersects a b = a.x0 <= b.x1 && b.x0 <= a.x1 && a.y0 <= b.y1 && b.y0 <= a.y1
let area r = (r.x1 - r.x0) * (r.y1 - r.y0)

let inter_area a b =
  let w = min a.x1 b.x1 - max a.x0 b.x0 and h = min a.y1 b.y1 - max a.y0 b.y0 in
  if w > 0 && h > 0 then w * h else 0

let dist2 r (px, py) =
  let dx = if px < r.x0 then r.x0 - px else if px > r.x1 then px - r.x1 else 0 in
  let dy = if py < r.y0 then r.y0 - py else if py > r.y1 then py - r.y1 else 0 in
  (dx * dx) + (dy * dy)

let dist2_to_rects rects p = List.fold_left (fun best r -> min best (dist2 r p)) max_int rects

let of_path (p : Gds.Path.t) =
  let k = Array.length p.points in
  if k <> 2 then Util.fail "path with %d points; only straight two-point paths are supported" k;
  if p.width land 1 <> 0 then Util.fail "path of odd width %d cannot be centred on the grid" p.width;
  let xa, ya = p.points.(0) and xb, yb = p.points.(1) in
  let hw = p.width / 2 in
  let b, e =
    match p.pathtype with
    | 0 -> (0, 0)
    | 2 -> (hw, hw)
    | 4 -> (p.bgn_extn, p.end_extn)
    | 1 -> Util.fail "round-ended paths (pathtype 1) are not supported"
    | t -> Util.fail "unknown pathtype %d" t
  in
  if ya = yb then
    let d = if xb >= xa then 1 else -1 in
    make (xa - (d * b)) (ya - hw) (xb + (d * e)) (ya + hw)
  else if xa = xb then
    let d = if yb >= ya then 1 else -1 in
    make (xa - hw) (ya - (d * b)) (xa + hw) (yb + (d * e))
  else Util.fail "non-Manhattan path from (%d, %d) to (%d, %d)" xa ya xb yb

(* Rectilinear polygon -> rectangles with disjoint interiors. Vertical slabs
   between consecutive distinct x coordinates; inside each slab the covering
   horizontal edges, sorted by y and paired, give the filled intervals. *)
let of_polygon (pts : Gds.point array) =
  let n = Array.length pts in
  if n < 4 then Util.fail "polygon with %d vertices" n;
  let hedges = ref [] in
  for i = 0 to n - 1 do
    let xa, ya = pts.(i) and xb, yb = pts.((i + 1) mod n) in
    if ya = yb then (if xa <> xb then hedges := (min xa xb, max xa xb, ya) :: !hedges)
    else if xa <> xb then Util.fail "non-Manhattan polygon edge (%d, %d)-(%d, %d)" xa ya xb yb
  done;
  let xs = Array.to_list (Array.map fst pts) |> List.sort_uniq compare in
  let rec slabs acc = function
    | xa :: (xb :: _ as rest) ->
        let ys =
          List.filter_map (fun (lo, hi, y) -> if lo <= xa && hi >= xb then Some y else None) !hedges
          |> List.sort compare
        in
        let rec pair acc = function
          | y0 :: y1 :: tl -> pair (make xa y0 xb y1 :: acc) tl
          | _ -> acc
        in
        slabs (pair acc ys) rest
    | _ -> acc
  in
  slabs [] xs

(* Reference placement: optional mirror about the x axis, then a rotation by
   quarter turns, then translation. Rectangles stay rectangles. *)
let transform ~reflect ~turns ~origin:(ox, oy) r =
  let f (x, y) =
    let x, y = if reflect then (x, -y) else (x, y) in
    let x, y =
      match turns land 3 with
      | 0 -> (x, y)
      | 1 -> (-y, x)
      | 2 -> (-x, -y)
      | _ -> (y, -x)
    in
    (x + ox, y + oy)
  in
  let ax, ay = f (r.x0, r.y0) and bx, by = f (r.x1, r.y1) in
  make ax ay bx by

(* --- uniform grid index -------------------------------------------------- *)

type index = {
  pitch : int;
  rects : rect array;
  buckets : (int * int, int list) Hashtbl.t;
  stamp : int array;  (* dedup marks for queries *)
  mutable epoch : int;
}

let fdiv a b = if a >= 0 then a / b else -((-a + b - 1) / b)

let build ~pitch rects =
  let rects = Array.of_list rects in
  let buckets = Hashtbl.create ((2 * Array.length rects) + 1) in
  Array.iteri
    (fun i r ->
      for gx = fdiv r.x0 pitch to fdiv r.x1 pitch do
        for gy = fdiv r.y0 pitch to fdiv r.y1 pitch do
          let k = (gx, gy) in
          Hashtbl.replace buckets k (i :: Option.value (Hashtbl.find_opt buckets k) ~default:[])
        done
      done)
    rects;
  { pitch; rects; buckets; stamp = Array.make (Array.length rects) 0; epoch = 0 }

let query idx r f =
  idx.epoch <- idx.epoch + 1;
  let ep = idx.epoch in
  for gx = fdiv r.x0 idx.pitch to fdiv r.x1 idx.pitch do
    for gy = fdiv r.y0 idx.pitch to fdiv r.y1 idx.pitch do
      List.iter
        (fun j ->
          if idx.stamp.(j) <> ep then begin
            idx.stamp.(j) <- ep;
            if intersects idx.rects.(j) r then f j
          end)
        (Option.value (Hashtbl.find_opt idx.buckets (gx, gy)) ~default:[])
    done
  done

(* --- union find ---------------------------------------------------------- *)

module Dsu = struct
  type t = { parent : int array }

  let create n = { parent = Array.init n (fun i -> i) }

  let rec find d x =
    let p = d.parent.(x) in
    if p = x then x
    else begin
      let r = find d p in
      d.parent.(x) <- r;
      r
    end

  let union d a b =
    let ra = find d a and rb = find d b in
    if ra <> rb then d.parent.(ra) <- rb
end
