(* Command line driver for the OCaml port of the ASIC puzzle solve. *)

open Asic

let usage =
  {|usage: asicre <command> [args]

  extract <gds> <out.json>          geometry -> netlist, same JSON schema as re/extract.py
  check   <gds> <bits.txt> [opts]   grid constraints, chip simulation, easter eggs,
                                    post-reset state; --unique adds the z3 proof and
                                    --reset-state re/reset_state.json the exact flop comparison
  solve   <gds>                     find the 121 input bits with z3, prove them unique
  warmup  <warmup.gds>              functional check of the warm-up design (A + B == 496)
  vcd     <gds> <file.vcd>          replay a waveform and compare O[7:0] / success
  def     <def> <warmup.gds>        compare extraction against the warm-up DEF ground truth
|}

let usage_exit () =
  prerr_string usage;
  exit 2

(* --- the puzzle protocol -------------------------------------------------- *)

let n = 11
let n_bits = n * n
let reset_clocks = 3      (* clocks with rst_n low before loading the grid *)
let playback_clocks = 18  (* clocks after loading, during which the message plays *)

let load gds = Gds.read gds |> Extract.extract ~log:prerr_endline
let netlist gds = Sim.of_extract (load gds)

(* the grid as 121 characters of 0/1; an 11-line layout is accepted too *)
let read_bits path =
  let s = String.concat "" (Util.words (Util.read_file path)) in
  if String.length s <> n_bits then
    Util.fail "%s: expected %d bits, found %d characters" path n_bits (String.length s);
  String.iteri
    (fun i c -> if c <> '0' && c <> '1' then Util.fail "%s: unexpected character %C at position %d" path c i)
    s;
  s

let clock nl st ~rst_n ~enable ~i =
  let inputs clk = [ ("clk", clk); ("rst_n", rst_n); ("enable", enable); ("I", i) ] in
  Sim.step nl st (inputs 0);
  Sim.step nl st (inputs 1)

let reset nl =
  let st = Sim.new_state nl in
  for _ = 1 to reset_clocks do
    clock nl st ~rst_n:0 ~enable:0 ~i:0
  done;
  st

let port nl st name =
  match Sim.read nl st name with
  | Some v -> v
  | None -> Util.fail "the design has no %s port; is this the puzzle GDS?" name

let byte nl st =
  match Sim.read_byte nl st with
  | Some b -> b
  | None -> failwith "the design has no O[7:0] ports; is this the puzzle GDS?"

(* load the grid bits after a reset, then play back: (success, message) *)
let run nl bits =
  let st = reset nl in
  String.iter (fun b -> clock nl st ~rst_n:1 ~enable:1 ~i:(if b = '1' then 1 else 0)) bits;
  let succ = ref 0 and msg = Buffer.create 16 in
  for _ = 1 to playback_clocks do
    clock nl st ~rst_n:1 ~enable:0 ~i:0;
    succ := max !succ (port nl st "success");
    let b = byte nl st in
    if b <> 0 then Buffer.add_char msg (if b >= 32 && b < 127 then Char.chr b else '?')
  done;
  (!succ, Buffer.contents msg)

let grid_of_bits bits =
  Array.init n (fun r -> Array.init n (fun c -> if bits.[(r * n) + c] = '1' then 1 else 0))

let show_grid bits =
  Array.iteri
    (fun r row ->
      Printf.printf "  row %2d   %s\n" r
        (String.concat " " (Array.to_list (Array.map (fun v -> if v = 1 then "*" else ".") row))))
    (grid_of_bits bits)

(* --- commands --------------------------------------------------------------- *)

let failures = ref []

let check name ok detail =
  Printf.printf "  %s  %s%s\n" (if ok then "ok  " else "FAIL") name
    (if detail = "" then "" else "  (" ^ detail ^ ")");
  if not ok then failures := name :: !failures

let finish () =
  print_newline ();
  match !failures with
  | [] -> print_endline "all checks passed"
  | fs ->
      Printf.printf "%d CHECK(S) FAILED: %s\n" (List.length fs) (String.concat ", " (List.rev fs));
      exit 1

let cmd_extract gds out =
  let nl = load gds in
  Util.write_file out (Extract.to_json nl);
  Printf.printf "wrote %s: %d instances, %d nets\n" out (List.length nl.insts) nl.n_nets

let cmd_check gds bits_path ~unique ~reset_state =
  let bits = read_bits bits_path in
  let g = grid_of_bits bits in
  let sum = Array.fold_left ( + ) 0 in
  let rows = Array.map sum g and cols = Array.init n (fun c -> sum (Array.map (fun row -> row.(c)) g)) in
  let touching = ref 0 in
  for r = 0 to n - 1 do
    for c = 0 to n - 1 do
      if g.(r).(c) = 1 then
        List.iter
          (fun (dr, dc) ->
            let r' = r + dr and c' = c + dc in
            if r' >= 0 && r' < n && c' >= 0 && c' < n && g.(r').(c') = 1 then incr touching)
          [ (0, 1); (1, -1); (1, 0); (1, 1) ]
    done
  done;
  print_endline "grid constraints:";
  check "two stars in every row" (Array.for_all (( = ) 2) rows) "";
  check "two stars in every column" (Array.for_all (( = ) 2) cols) "";
  check "no two stars adjacent (incl. diagonal)" (!touching = 0) "";
  let nl = netlist gds in
  print_endline "chip simulation:";
  let succ, msg = run nl bits in
  check "success = 1 on the solution" (succ = 1) "";
  check "chip prints \"(* TWO STARS *)\"" (msg = "(* TWO STARS *)") (Printf.sprintf "%S" msg);
  let s0, m0 = run nl (String.make n_bits '0') and s1, m1 = run nl (String.make n_bits '1') in
  let flipped = String.mapi (fun i c -> if i > 0 then c else if c = '1' then '0' else '1') bits in
  let s2, m2 = run nl flipped in
  check "all-zeros grid: success=0, \"EMPTY SKY\"" (s0 = 0 && m0 = "EMPTY SKY") (Printf.sprintf "%d %S" s0 m0);
  check "all-ones grid: success=0, \"BIG BANG\"" (s1 = 0 && m1 = "BIG BANG") (Printf.sprintf "%d %S" s1 m1);
  check "near-miss grid (first bit flipped): success=0, \"TRY AGAIN\"" (s2 = 0 && m2 = "TRY AGAIN")
    (Printf.sprintf "%d %S" s2 m2);
  (* after reset the async-set flops (dfstp) read 1, everything else 0 *)
  let post_reset = Array.copy (reset nl).Sim.q in
  let expected (d : Sim.Dff.t) = if d.kind = Cells.Stp then 1 else 0 in
  check "post-reset state: dfstp flops 1, all others 0"
    (Array.for_all Fun.id (Array.mapi (fun di d -> post_reset.(di) = expected d) nl.Sim.dffs))
    "";
  Option.iter
    (fun path ->
      let rs = Reset_state.read path in
      let ok =
        List.for_all2
          (fun id v ->
            match Sim.find_dff nl ~id with
            | Some di -> post_reset.(di) = v
            | None -> Util.fail "%s names flop %d, which the netlist does not have" path id)
          rs.Reset_state.ids rs.init
      in
      check "post-reset flop state matches reset_state.json" ok "")
    reset_state;
  if unique then begin
    print_endline "uniqueness (z3):";
    match Smt.solve_unique nl ~n_bits ~post_reset with
    | Error e -> check "uniqueness proof (z3)" false e
    | Ok (witness, unique) ->
        check "solver finds a witness" true "";
        check "witness equals the given bits" (witness = bits) "";
        check "no second solution exists (unsat)" unique ""
  end;
  finish ()

let cmd_solve gds =
  let nl = netlist gds in
  let post_reset = Array.copy (reset nl).Sim.q in
  Printf.printf "unrolling %d clocks with symbolic inputs, asking z3 for success = 1 ...\n"
    (n_bits + Smt.playback_clocks);
  match Smt.solve_unique nl ~n_bits ~post_reset with
  | Error e -> failwith e
  | Ok (bits, unique) ->
      Printf.printf "input bits: %s\n" bits;
      show_grid bits;
      let succ, msg = run nl bits in
      Printf.printf "simulated: success=%d  message=%S\n" succ msg;
      Printf.printf "unique: %s\n" (if unique then "yes (blocking the witness is unsat)" else "NO");
      if not unique then exit 1

let cmd_warmup gds =
  let nl = netlist gds in
  let s a b =
    let st = Sim.new_state nl in
    let step ~clk ~rst_n ~en ~a ~b =
      Sim.step nl st [ ("clk", clk); ("rst_n", rst_n); ("en", en); ("A", a); ("B", b) ]
    in
    for _ = 1 to 2 do
      step ~clk:0 ~rst_n:0 ~en:0 ~a:0 ~b:0;
      step ~clk:1 ~rst_n:0 ~en:0 ~a:0 ~b:0
    done;
    (* operands are shifted in most significant bit first *)
    for i = 7 downto 0 do
      let a = (a lsr i) land 1 and b = (b lsr i) land 1 in
      step ~clk:0 ~rst_n:1 ~en:1 ~a ~b;
      step ~clk:1 ~rst_n:1 ~en:1 ~a ~b
    done;
    port nl st "S"
  in
  print_endline "warmup netlist (functional):";
  check "248 + 248 = 496 raises S" (s 0xF8 0xF8 = 1) "";
  check "255 + 241 = 496 raises S" (s 0xFF 0xF1 = 1) "";
  check "1 + 2 keeps S low" (s 0x01 0x02 = 0) "";
  finish ()

let cmd_vcd gds vcd =
  let nl = netlist gds in
  let r = Vcd.replay nl (Vcd.parse vcd) in
  Printf.printf "comparisons: %d, matched: %d, mismatched: %d\n" r.total r.matched
    (List.length r.mismatches);
  List.iteri
    (fun i (m : Vcd.mismatch) ->
      let opt = function Some v -> string_of_int v | None -> "missing" in
      if i < 30 then
        Printf.printf "  t=%d: O expected %d got %s  success expected %s got %s\n" m.time m.expected_o
          (opt m.got_o) (Option.value m.expected_success ~default:"?") (opt m.got_success))
    r.mismatches;
  if r.total = 0 then Util.fail "%s records no O values to compare against" vcd;
  if r.mismatches <> [] then exit 1

let cmd_def def gds =
  let lib = Gds.read gds in
  let nl = Extract.extract ~log:prerr_endline lib in
  let r = Defcheck.check ~def ~lib nl in
  Printf.printf "DEF components: %d, matched to GDS instances: %d, missing: %d\n" r.components
    r.matched (List.length r.missing);
  List.iteri (fun i m -> if i < 10 then Printf.printf "  MISSING %s\n" m) r.missing;
  List.iter print_endline r.problems;
  Printf.printf "nets consistent: %d, broken: %d\n" r.consistent r.broken;
  if r.missing <> [] || r.broken > 0 then exit 1

let () =
  let rec parse pos ~unique ~reset_state = function
    | [] -> (List.rev pos, unique, reset_state)
    | "--unique" :: rest -> parse pos ~unique:true ~reset_state rest
    | "--reset-state" :: path :: rest when not (String.starts_with ~prefix:"--" path) ->
        parse pos ~unique ~reset_state:(Some path) rest
    | o :: _ when String.starts_with ~prefix:"--" o ->
        Printf.eprintf "asicre: unknown or incomplete option %s\n\n" o;
        usage_exit ()
    | a :: rest -> parse (a :: pos) ~unique ~reset_state rest
  in
  let pos, unique, reset_state =
    parse [] ~unique:false ~reset_state:None (List.tl (Array.to_list Sys.argv))
  in
  let no_options cmd =
    if unique || reset_state <> None then begin
      Printf.eprintf "asicre: %s takes no options\n\n" cmd;
      usage_exit ()
    end
  in
  try
    match pos with
    | [ "extract"; gds; out ] -> no_options "extract"; cmd_extract gds out
    | [ "check"; gds; bits ] -> cmd_check gds bits ~unique ~reset_state
    | [ "solve"; gds ] -> no_options "solve"; cmd_solve gds
    | [ "warmup"; gds ] -> no_options "warmup"; cmd_warmup gds
    | [ "vcd"; gds; vcd ] -> no_options "vcd"; cmd_vcd gds vcd
    | [ "def"; def; gds ] -> no_options "def"; cmd_def def gds
    | _ -> usage_exit ()
  with
  | Failure m | Sys_error m ->
      Printf.eprintf "asicre: %s\n" m;
      exit 1
  | Stack_overflow | Out_of_memory as e -> raise e
  | e ->
      Printf.eprintf "asicre: internal error: %s\n" (Printexc.to_string e);
      exit 2
