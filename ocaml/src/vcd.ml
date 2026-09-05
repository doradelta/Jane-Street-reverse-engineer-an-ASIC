(* Minimal VCD reader and the replay check of re/vcd_check.py.

   Timing convention: all changes at a timestamp are applied before the step,
   so an input changing on the same timestamp as a rising clock edge is
   sampled at its new value. Both shipped VCDs drive inputs on falling-edge
   timestamps only, where the conventions agree. *)

type t = {
  id_of_name : (string * string) list;
      (* later definitions first, so a name declared twice resolves to the
         last $var, as in the Python *)
  events : (int * (string * string) list) list;  (* time, (identifier, value) *)
}

let parse path : t =
  let lines = Util.lines (Util.read_file path) in
  let names = ref [] in
  let rec header = function
    | [] -> Util.fail "%s: not a VCD file (no $enddefinitions)" path
    | l :: rest when String.starts_with ~prefix:"$enddefinitions" l -> rest
    | l :: rest ->
        (* $var reg 1 ! clk $end   /   $var wire 8 % O [7:0] $end *)
        (match Util.words l with
        | "$var" :: _ :: _ :: ident :: name :: _ -> names := (name, ident) :: !names
        | _ -> ());
        header rest
  in
  let body = header lines in
  let events = ref [] and cur_t = ref None and cur = ref [] in
  let flush () = Option.iter (fun t -> events := (t, List.rev !cur) :: !events) !cur_t in
  List.iter
    (fun l ->
      if l = "" || l.[0] = '$' then ()
      else if l.[0] = '#' then begin
        flush ();
        cur_t := Some (int_of_string (String.sub l 1 (String.length l - 1)));
        cur := []
      end
      else if l.[0] = 'b' then begin
        match Util.words (String.sub l 1 (String.length l - 1)) with
        | [ value; ident ] -> cur := (ident, value) :: !cur
        | _ -> ()
      end
      else cur := (String.sub l 1 (String.length l - 1), String.make 1 l.[0]) :: !cur)
    body;
  flush ();
  { id_of_name = !names; events = List.rev !events }

(* binary vector -> int, None when it holds x/z bits *)
let int_of_vector s =
  let s = String.lowercase_ascii s in
  if String.contains s 'x' || String.contains s 'z' then None
  else Some (String.fold_left (fun acc c -> (acc * 2) + if c = '1' then 1 else 0) 0 s)

type mismatch = {
  time : int;
  expected_o : int;
  got_o : int option;
  expected_success : string option;
  got_success : int option;
}

type replay = { total : int; matched : int; mismatches : mismatch list }

let replay (nl : Sim.t) (v : t) : replay =
  let st = Sim.new_state nl in
  let inputs = ref [ ("clk", 0); ("rst_n", 0); ("enable", 0); ("I", 0) ] in
  let cur = Hashtbl.create 8 in
  let total = ref 0 and matched = ref 0 and mism = ref [] in
  let ident name = List.assoc_opt name v.id_of_name in
  let current name = Option.bind (ident name) (Hashtbl.find_opt cur) in
  List.iter
    (fun (time, ev) ->
      List.iter (fun (id, value) -> Hashtbl.replace cur id value) ev;
      List.iter
        (fun name ->
          match current name with
          | Some (("0" | "1") as b) -> inputs := (name, int_of_string b) :: List.remove_assoc name !inputs
          | _ -> ())
        [ "clk"; "rst_n"; "enable"; "I" ];
      Sim.step nl st !inputs;
      match Option.bind (current "O") int_of_vector with
      | None -> ()
      | Some expected_o ->
          incr total;
          let got_o = Sim.read_byte nl st and got_success = Sim.read nl st "success" in
          let expected_success = current "success" in
          let success_ok =
            match expected_success with
            | Some (("0" | "1") as b) -> got_success = Some (int_of_string b)
            | _ -> true
          in
          if got_o = Some expected_o && success_ok then incr matched
          else mism := { time; expected_o; got_o; expected_success; got_success } :: !mism)
    v.events;
  { total = !total; matched = !matched; mismatches = List.rev !mism }
