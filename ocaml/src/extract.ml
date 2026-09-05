(* GDS -> netlist extraction, a port of re/extract.py.

   The standard cells keep their names in the GDS but the routing is anonymous
   metal, so connectivity is pure geometry: every top-level metal shape, every
   via pad and every placed cell-pin shape is union-found with whatever it
   touches on the same layer. *)

let li1 = 67
let met1 = 68
let poly = 66
let metals = [ 67; 68; 69; 70; 71; 72 ] (* li1, met1 .. met5 *)
let draw = 20
let pin_dt = 16
let label_tt = 5
let licon_layer, licon_dt = (66, 44)
let mcon_layer, mcon_dt = (67, 44)

(* Distances are in nanometres, which the extractor requires the file to use
   as its database unit. *)
let db_unit_um = 0.001
let label_snap_nm = 10  (* a pin label may sit this far from its li1 shape (Python: 0.01 um) *)
let port_snap_nm = 5    (* a port label from its top-level shape (Python: 0.005 um) *)
let index_pitch_nm = 2000

let is_power pin = List.mem pin [ "VPWR"; "VGND"; "VPB"; "VNB" ]
let is_rail name = name = "VPWR" || name = "VGND"

(* "sky130_fd_sc_hd__nand2_2" -> "nand2_2", scanning for the separator left to
   right like Python's name.split("__")[-1] *)
let short_type name =
  let n = String.length name in
  let rec go i start =
    if i + 1 >= n then start
    else if name.[i] = '_' && name.[i + 1] = '_' then go (i + 2) (i + 2)
    else go (i + 1) start
  in
  let start = go 0 0 in
  String.sub name start (n - start)

(* shapes of a cell on (layer, datatype in dts); one rectangle list per shape *)
let cell_shapes (cell : Gds.cell) layer dts =
  let polys =
    List.filter_map
      (fun (p : Gds.Polygon.t) ->
        if p.layer = layer && List.mem p.datatype dts then Some (Geom.of_polygon p.points)
        else None)
      cell.polygons
  in
  let paths =
    List.filter_map
      (fun (p : Gds.Path.t) ->
        if p.layer = layer && List.mem p.datatype dts then Some [ Geom.of_path p ] else None)
      cell.paths
  in
  polys @ paths

let shapes_touch a b = List.exists (fun ra -> List.exists (Geom.intersects ra) b) a

(* Per-pin routable copper of a standard cell, on li1 and met1.

   A pin's li1 may be several islands joined only through the poly gate
   (li1 -> licon -> poly -> licon -> li1) or through met1 (li1 -> mcon -> met1
   -> mcon -> li1), so the intra-cell graph over li1/met1/poly shapes is
   union-found through mcon and poly-landing licon contacts, and the component
   carrying each pin label is the pin. *)
let build_pinmap (cell : Gds.cell) : (string * (Geom.rect list * Geom.rect list)) list =
  let li = Array.of_list (cell_shapes cell li1 [ draw; pin_dt ]) in
  let m1 = Array.of_list (cell_shapes cell met1 [ draw; pin_dt ]) in
  let po = Array.of_list (cell_shapes cell poly [ draw ]) in
  let licons = List.concat (cell_shapes cell licon_layer [ licon_dt ]) in
  let mcons = List.concat (cell_shapes cell mcon_layer [ mcon_dt ]) in
  let nli = Array.length li and nm1 = Array.length m1 and npo = Array.length po in
  let dsu = Geom.Dsu.create (nli + nm1 + npo) in
  let node_li i = i and node_m1 j = nli + j and node_po k = nli + nm1 + k in
  let within arr node =
    Array.iteri
      (fun i a ->
        for j = i + 1 to Array.length arr - 1 do
          if shapes_touch a arr.(j) then Geom.Dsu.union dsu (node i) (node j)
        done)
      arr
  in
  within li node_li;
  within m1 node_m1;
  within po node_po;
  let touching arr node c =
    let acc = ref [] in
    Array.iteri (fun i s -> if List.exists (Geom.intersects c) s then acc := node i :: !acc) arr;
    List.rev !acc
  in
  let union_all = function
    | [] -> ()
    | first :: rest -> List.iter (fun n -> Geom.Dsu.union dsu first n) rest
  in
  (* mcon: li1 <-> met1 *)
  List.iter (fun c -> union_all (touching li node_li c @ touching m1 node_m1 c)) mcons;
  (* licon landing on poly (more than 40% of its area): li1 <-> poly. Contacts
     on diffusion are skipped. *)
  List.iter
    (fun c ->
      let on_poly = ref [] in
      Array.iteri
        (fun k s ->
          let a = List.fold_left (fun acc r -> acc + Geom.inter_area r c) 0 s in
          if a * 10 > 4 * Geom.area c then on_poly := node_po k :: !on_poly)
        po;
      if !on_poly <> [] then union_all (List.rev !on_poly @ touching li node_li c))
    licons;
  (* Pin labels sit on li1 shapes. The nearest shape within the snap distance
     is taken; the Python takes the first spatial-index hit, which is the same
     shape whenever the candidates touch each other, as they do in every
     shipped cell. *)
  let root_name = Hashtbl.create 16 in
  let snap2 = label_snap_nm * label_snap_nm in
  List.iter
    (fun (t : Gds.Text.t) ->
      if t.layer = li1 && t.texttype = label_tt then begin
        let best = ref None in
        Array.iteri
          (fun i s ->
            let d = Geom.dist2_to_rects s t.origin in
            if d <= snap2 then
              match !best with Some (_, bd) when bd <= d -> () | _ -> best := Some (i, d))
          li;
        match !best with
        | None -> ()
        | Some (i, _) -> (
            let r = Geom.Dsu.find dsu (node_li i) in
            match Hashtbl.find_opt root_name r with
            | Some other when other <> t.text ->
                Util.fail "%s: one component holds pins %s and %s" cell.name other t.text
            | _ -> Hashtbl.replace root_name r t.text)
      end)
    cell.texts;
  (* collect, keeping pins in order of first appearance *)
  let order = ref [] and pins = Hashtbl.create 8 in
  let add name layer rects =
    if not (Hashtbl.mem pins name) then begin
      order := name :: !order;
      Hashtbl.replace pins name (ref [], ref [])
    end;
    let lir, m1r = Hashtbl.find pins name in
    if layer = li1 then lir := !lir @ rects else m1r := !m1r @ rects
  in
  let name_of node = Hashtbl.find_opt root_name (Geom.Dsu.find dsu node) in
  Array.iteri (fun i s -> Option.iter (fun name -> add name li1 s) (name_of (node_li i))) li;
  Array.iteri (fun j s -> Option.iter (fun name -> add name met1 s) (name_of (node_m1 j))) m1;
  List.rev_map
    (fun name ->
      let lir, m1r = Hashtbl.find pins name in
      (name, (!lir, !m1r)))
    !order

type 'pins inst = {
  id : int;
  typ : string;  (* short cell type, e.g. "nand2_2" *)
  x : int;       (* origin, database units *)
  y : int;
  rot : int;     (* quarter turns *)
  refl : bool;
  pins : 'pins;
}

type netlist = {
  top : string;
  insts : (string * int option) list inst list;  (* pin -> net, None when dangling *)
  ports : (string * int) list;
  n_nets : int;
}

let extract ?(log = ignore) (lib : Gds.library) : netlist =
  if Float.abs (lib.db_unit_um -. db_unit_um) > 1e-12 then
    Util.fail "database unit is %g um; this extractor assumes %g um (1 nm)" lib.db_unit_um
      db_unit_um;
  let top =
    match Gds.top_cells lib with
    | [ c ] -> c
    | [] -> failwith "no top cell"
    | cs -> Util.fail "several top cells: %s" (String.concat ", " (List.map (fun c -> c.Gds.name) cs))
  in
  log (Printf.sprintf "top cell: %s" top.name);
  let pinmaps = Hashtbl.create 64 and via_defs = Hashtbl.create 16 in
  List.iter
    (fun (c : Gds.cell) ->
      if String.starts_with ~prefix:"VIA_" c.name then
        Hashtbl.replace via_defs c.name
          (List.map (fun ly -> (ly, List.concat (cell_shapes c ly [ draw ]))) metals)
      else if String.starts_with ~prefix:"sky130" c.name then
        Hashtbl.replace pinmaps c.name (build_pinmap c))
    lib.cells;
  (* owners: one id per routing shape, via instance, or (instance, pin) *)
  let n_owner = ref 0 in
  let new_owner () =
    let o = !n_owner in
    incr n_owner;
    o
  in
  let layer_shapes = Hashtbl.create 8 in
  List.iter (fun ly -> Hashtbl.replace layer_shapes ly (ref [])) metals;
  let add ly r o =
    let l = Hashtbl.find layer_shapes ly in
    l := (r, o) :: !l
  in
  let routable ly dt = List.mem ly metals && (dt = draw || dt = pin_dt) in
  List.iter
    (fun (p : Gds.Polygon.t) ->
      if routable p.layer p.datatype then begin
        let o = new_owner () in
        List.iter (fun r -> add p.layer r o) (Geom.of_polygon p.points)
      end)
    top.polygons;
  List.iter
    (fun (p : Gds.Path.t) ->
      if routable p.layer p.datatype then add p.layer (Geom.of_path p) (new_owner ()))
    top.paths;
  let placed = ref [] and n_inst = ref 0 in
  List.iter
    (fun (r : Gds.Reference.t) ->
      if r.mag <> 1.0 then
        Util.fail "reference to %s has magnification %g; only 1 is supported" r.cell r.mag;
      let place = Geom.transform ~reflect:r.reflect ~turns:(Gds.quarter_turns r) ~origin:r.origin in
      if String.starts_with ~prefix:"VIA_" r.cell then begin
        let o = new_owner () in
        List.iter
          (fun (ly, rects) -> List.iter (fun rc -> add ly (place rc) o) rects)
          (Hashtbl.find via_defs r.cell)
      end
      else if String.starts_with ~prefix:"sky130" r.cell then begin
        let id = !n_inst in
        incr n_inst;
        let pins =
          List.filter_map
            (fun (pin, (lir, m1r)) ->
              if is_power pin then None
              else begin
                let o = new_owner () in
                List.iter (fun rc -> add li1 (place rc) o) lir;
                List.iter (fun rc -> add met1 (place rc) o) m1r;
                Some (pin, o)
              end)
            (Hashtbl.find pinmaps r.cell)
        in
        let x, y = r.origin in
        placed :=
          { id; typ = short_type r.cell; x; y; rot = Gds.quarter_turns r; refl = r.reflect; pins }
          :: !placed
      end
      (* INTERNAL_* cells carry no geometry on real layers *))
    top.references;
  (* union-find owners that touch on any layer *)
  let dsu = Geom.Dsu.create !n_owner in
  List.iter
    (fun ly ->
      let shapes = Array.of_list (List.rev !(Hashtbl.find layer_shapes ly)) in
      let owners = Array.map snd shapes in
      let idx = Geom.build ~pitch:index_pitch_nm (Array.to_list (Array.map fst shapes)) in
      Array.iteri
        (fun i (r, _) ->
          Geom.query idx r (fun j -> if j > i then Geom.Dsu.union dsu owners.(i) owners.(j)))
        shapes;
      log (Printf.sprintf "layer %d: %d shapes" ly (Array.length shapes)))
    metals;
  let group_size = Array.make !n_owner 0 in
  for o = 0 to !n_owner - 1 do
    let r = Geom.Dsu.find dsu o in
    group_size.(r) <- group_size.(r) + 1
  done;
  let netid = Hashtbl.create 1024 in
  let net_of o =
    let r = Geom.Dsu.find dsu o in
    match Hashtbl.find_opt netid r with
    | Some n -> n
    | None ->
        let n = Hashtbl.length netid in
        Hashtbl.replace netid r n;
        n
  in
  (* nets are numbered in instance order, which keeps the JSON byte-identical
     to the Python extractor's *)
  let insts =
    List.map
      (fun inst ->
        let pins =
          List.map
            (fun (pin, o) ->
              (* a pin that never touched anything is dangling *)
              if group_size.(Geom.Dsu.find dsu o) <= 1 then (pin, None) else (pin, Some (net_of o)))
            inst.pins
        in
        { inst with pins })
      (List.rev !placed)
  in
  (* ports from top-level labels: for the power rails the first label wins,
     for signals the last one does *)
  let ports = ref [] in
  let snap2 = port_snap_nm * port_snap_nm in
  List.iter
    (fun (t : Gds.Text.t) ->
      if t.texttype = label_tt && List.mem t.layer metals then begin
        let shapes = List.rev !(Hashtbl.find layer_shapes t.layer) in
        match List.find_opt (fun (r, _) -> Geom.dist2 r t.origin < snap2) shapes with
        | None -> log (Printf.sprintf "WARNING: no shape for label %s" t.text)
        | Some (_, o) ->
            let net = net_of o in
            if not (List.mem_assoc t.text !ports) then ports := !ports @ [ (t.text, net) ]
            else if not (is_rail t.text) then
              ports := List.map (fun (k, v) -> if k = t.text then (k, net) else (k, v)) !ports
      end)
    top.texts;
  let n_nets = Hashtbl.length netid in
  log
    (Printf.sprintf "instances: %d  nets: %d  ports: %d" (List.length insts) n_nets
       (List.length !ports));
  { top = top.name; insts; ports = !ports; n_nets }

(* --- JSON in the same schema as the Python extractor ---------------------- *)

let json_string s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\t' -> Buffer.add_string b "\\t"
      | c when Char.code c < 0x20 -> Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

(* nanometres as micrometres, shortest decimal *)
let um_of_nm v =
  let s = Printf.sprintf "%d.%03d" (abs v / 1000) (abs v mod 1000) in
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = '0' do decr n done;
  let s = String.sub s 0 !n in
  let s = if s.[String.length s - 1] = '.' then s ^ "0" else s in
  if v < 0 then "-" ^ s else s

let to_json (nl : netlist) =
  let b = Buffer.create (1 lsl 16) in
  let add = Buffer.add_string b in
  add (Printf.sprintf "{\"top\": %s, \"insts\": [" (json_string nl.top));
  List.iteri
    (fun i inst ->
      if i > 0 then add ", ";
      add
        (Printf.sprintf
           "{\"id\": %d, \"type\": %s, \"x\": %s, \"y\": %s, \"rot\": %d, \"refl\": %b, \"pins\": {"
           inst.id (json_string inst.typ) (um_of_nm inst.x) (um_of_nm inst.y) inst.rot inst.refl);
      List.iteri
        (fun j (pin, net) ->
          if j > 0 then add ", ";
          add
            (Printf.sprintf "%s: %s" (json_string pin)
               (match net with Some n -> string_of_int n | None -> "null")))
        inst.pins;
      add "}}")
    nl.insts;
  add "], \"ports\": {";
  List.iteri
    (fun i (k, v) ->
      if i > 0 then add ", ";
      add (Printf.sprintf "%s: %d" (json_string k) v))
    nl.ports;
  add (Printf.sprintf "}, \"n_nets\": %d}" nl.n_nets);
  Buffer.contents b
