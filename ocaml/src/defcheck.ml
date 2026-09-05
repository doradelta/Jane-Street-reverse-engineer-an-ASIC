(* Validate an extracted netlist against a DEF file, the place-and-route
   ground truth that ships with the warm-up design (port of re/def_check.py). *)

type result = {
  components : int;
  matched : int;
  missing : string list;
  consistent : int;
  broken : int;
  problems : string list;
}

(* what one connection of a DEF net resolves to on our side *)
type endpoint = Net of int | Dangling | Absent_pin of string | Unknown_port of string

let endpoint_to_string = function
  | Net n -> Printf.sprintf "net %d" n
  | Dangling -> "dangling"
  | Absent_pin p -> "no pin " ^ p
  | Unknown_port p -> "unknown port " ^ p

(* text of the section between [start] (a regexp) and [stop] *)
let section txt ~start ~stop =
  match Str.search_forward (Str.regexp start) txt 0 with
  | exception Not_found -> Util.fail "DEF: no %s section" stop
  | _ ->
      let a = Str.match_end () in
      let b = Str.search_forward (Str.regexp_string stop) txt a in
      String.sub txt a (b - a)

let statements txt = List.map Util.words (String.split_on_char ';' txt)

(* UNITS DISTANCE MICRONS k: DEF coordinates are in 1/k micrometres *)
let units txt =
  match Str.search_forward (Str.regexp "UNITS +DISTANCE +MICRONS +\\([0-9]+\\)") txt 0 with
  | exception Not_found -> failwith "DEF: no UNITS DISTANCE MICRONS statement"
  | _ -> int_of_string (Str.matched_group 1 txt)

(* "- name cell ... + PLACED ( x y ) ORIENT" *)
let parse_components txt =
  section txt ~start:"COMPONENTS [0-9]+ ;" ~stop:"END COMPONENTS"
  |> statements
  |> List.filter_map (fun stmt ->
         let rec placed = function
           | "PLACED" :: "(" :: x :: y :: ")" :: orient :: _ ->
               Some (int_of_string x, int_of_string y, orient)
           | _ :: rest -> placed rest
           | [] -> None
         in
         match stmt with
         | "-" :: name :: cell :: rest ->
             Option.map (fun (x, y, orient) -> (name, cell, x, y, orient)) (placed rest)
         | _ -> None)

(* "- net ( comp pin ) ( comp pin ) ..." *)
let parse_nets txt =
  section txt ~start:"\nNETS [0-9]+ ;" ~stop:"END NETS"
  |> statements
  |> List.filter_map (fun stmt ->
         match stmt with
         | "-" :: name :: rest ->
             let rec conns acc = function
               | "(" :: comp :: pin :: ")" :: tl -> conns ((comp, pin) :: acc) tl
               | _ :: tl -> conns acc tl
               | [] -> List.rev acc
             in
             Some (name, conns [] rest)
         | _ -> None)

let check ~def ~(lib : Gds.library) (nl : Extract.netlist) : result =
  let txt = Util.read_file def in
  let per_um = units txt in
  if 1000 mod per_um <> 0 then Util.fail "DEF: cannot convert %d units per micron to nanometres" per_um;
  let nm v = v * (1000 / per_um) in
  let comps = parse_components txt and nets = parse_nets txt in
  (* cell sizes from the GDS boundary layer 236/0 *)
  let size = Hashtbl.create 32 in
  List.iter
    (fun (c : Gds.cell) ->
      if String.starts_with ~prefix:"sky130" c.name then
        List.find_opt (fun (p : Gds.Polygon.t) -> p.layer = 236 && p.datatype = 0) c.polygons
        |> Option.iter (fun (p : Gds.Polygon.t) ->
               let extent f =
                 let vs = Array.map f p.points in
                 Array.fold_left max min_int vs - Array.fold_left min max_int vs
               in
               Hashtbl.replace size (Extract.short_type c.name) (extent fst, extent snd)))
    lib.cells;
  let by_key = Hashtbl.create 256 in
  List.iter
    (fun (i : _ Extract.inst) -> Hashtbl.replace by_key (i.typ, i.x, i.y, i.rot, i.refl) i)
    nl.insts;
  (* DEF orientation -> GDS origin, rotation and reflection *)
  let def2gds cell x y orient =
    match Hashtbl.find_opt size cell with
    | None -> Util.fail "DEF: no GDS cell %s" cell
    | Some (w, h) -> (
        match orient with
        | "N" -> (x, y, 0, false)
        | "S" -> (x + w, y + h, 2, false)
        | "FS" -> (x, y + h, 0, true)
        | "FN" -> (x + w, y, 2, true)
        | o -> Util.fail "DEF: unsupported orientation %s" o)
  in
  let comp2inst = Hashtbl.create 256 and missing = ref [] in
  List.iter
    (fun (name, cell, x, y, orient) ->
      let st = Extract.short_type cell in
      let gx, gy, rot, refl = def2gds st (nm x) (nm y) orient in
      match Hashtbl.find_opt by_key (st, gx, gy, rot, refl) with
      | Some inst -> Hashtbl.replace comp2inst name inst
      | None -> missing := Printf.sprintf "%s %s (%d, %d) %s" name cell x y orient :: !missing)
    comps;
  (* every DEF net must land on exactly one of ours, and ours on one of theirs *)
  let ok = ref 0 and bad = ref 0 and problems = ref [] in
  let mine2def = Hashtbl.create 256 in
  List.iter
    (fun (dnet, conns) ->
      let mine =
        List.filter_map
          (fun (comp, pin) ->
            if comp = "PIN" then
              Some (match List.assoc_opt pin nl.ports with Some n -> Net n | None -> Unknown_port pin)
            else
              Option.map
                (fun (inst : _ Extract.inst) ->
                  match List.assoc_opt pin inst.pins with
                  | Some (Some n) -> Net n
                  | Some None -> Dangling
                  | None -> Absent_pin pin)
                (Hashtbl.find_opt comp2inst comp))
          conns
        |> List.sort_uniq compare
      in
      match mine with
      | [ e ] -> (
          match Hashtbl.find_opt mine2def e with
          | Some other when other <> dnet ->
              problems :=
                Printf.sprintf "COLLISION: my %s maps to DEF nets %s and %s" (endpoint_to_string e)
                  other dnet
                :: !problems;
              incr bad
          | _ ->
              Hashtbl.replace mine2def e dnet;
              incr ok)
      | es ->
          problems :=
            Printf.sprintf "SPLIT: DEF net %s maps to [%s]" dnet
              (String.concat "; " (List.map endpoint_to_string es))
            :: !problems;
          incr bad)
    nets;
  { components = List.length comps; matched = Hashtbl.length comp2inst; missing = List.rev !missing;
    consistent = !ok; broken = !bad; problems = List.rev !problems }
