(* Bounded model checking through SMT-LIB2 and the z3 binary.

   The netlist is unrolled over the input clocks plus two playback clocks with
   the input bits symbolic, like the uniqueness section of re/verify.py:
   success must be high after the first or the second playback clock. Every
   net of every cycle becomes a named definition, so the formula stays linear
   in the circuit size.

   The unrolling holds rst_n high and does not model the flops' asynchronous
   set/reset pins at all, which is only sound when every such pin is wired to
   rst_n; that is checked before anything is emitted. *)

let terms : string Cells.alg =
  {
    const = (fun b -> if b then "true" else "false");
    not_ = (fun x -> "(not " ^ x ^ ")");
    and_ = (fun a b -> "(and " ^ a ^ " " ^ b ^ ")");
    or_ = (fun a b -> "(or " ^ a ^ " " ^ b ^ ")");
    xor_ = (fun a b -> "(xor " ^ a ^ " " ^ b ^ ")");
    ite = (fun s a b -> "(ite " ^ s ^ " " ^ a ^ " " ^ b ^ ")");
  }

let playback_clocks = 2

let formula (nl : Sim.t) ~n_bits ~post_reset ~success =
  let b = Buffer.create (1 lsl 22) in
  let line s =
    Buffer.add_string b s;
    Buffer.add_char b '\n'
  in
  for t = 0 to n_bits - 1 do
    line (Printf.sprintf "(declare-const I_%d Bool)" t)
  done;
  let q = Array.map (fun v -> if v = 1 then "true" else "false") post_reset in
  let succ_terms = ref [] in
  let value = Hashtbl.create 1024 in
  for cyc = 0 to n_bits + playback_clocks - 1 do
    Hashtbl.reset value;
    List.iter (fun (net, v) -> Hashtbl.replace value net (terms.const (v = 1))) nl.consts;
    List.iter
      (fun (name, net) ->
        let v =
          match name with
          | "clk" | "rst_n" -> "true"
          | "enable" -> terms.const (cyc < n_bits)
          | "I" -> if cyc < n_bits then Printf.sprintf "I_%d" cyc else "false"
          | other -> Util.fail "unexpected primary input %s" other
        in
        Hashtbl.replace value net v)
      nl.in_ports;
    Array.iteri
      (fun di (d : Sim.Dff.t) -> Option.iter (fun n -> Hashtbl.replace value n q.(di)) d.q)
      nl.dffs;
    let lookup = function
      | Some net -> Option.value (Hashtbl.find_opt value net) ~default:"false"
      | None -> "false"
    in
    Array.iter
      (fun ci ->
        let c = nl.comb.(ci) in
        Option.iter
          (fun o ->
            let name = Printf.sprintf "n%d_%d" o cyc in
            let term = c.fn.eval terms (fun k -> lookup (Sim.input_net c k)) in
            line (Printf.sprintf "(define-fun %s () Bool %s)" name term);
            Hashtbl.replace value o name)
          c.out_net)
      nl.order;
    Array.iteri
      (fun di (d : Sim.Dff.t) ->
        let name = Printf.sprintf "q%d_%d" di cyc in
        line (Printf.sprintf "(declare-const %s Bool)" name);
        line (Printf.sprintf "(assert (= %s %s))" name (lookup d.d));
        q.(di) <- name)
      nl.dffs;
    if cyc >= n_bits then succ_terms := q.(success) :: !succ_terms
  done;
  line (Printf.sprintf "(assert (or %s))" (String.concat " " !succ_terms));
  Buffer.contents b

let z3_available () =
  match Unix.system "z3 -version > /dev/null 2>&1" with Unix.WEXITED 0 -> true | _ -> false

(* run a script through z3: (exit status, stdout, stderr) *)
let run_z3 script =
  let path = Filename.temp_file "asicre" ".smt2" and err = Filename.temp_file "asicre" ".err" in
  Fun.protect
    ~finally:(fun () ->
      Sys.remove path;
      Sys.remove err)
    (fun () ->
      Util.write_file path script;
      let cmd = Printf.sprintf "z3 -smt2 %s 2>%s" (Filename.quote path) (Filename.quote err) in
      let ic = Unix.open_process_in cmd in
      let out = Buffer.create 4096 in
      (try
         while true do
           Buffer.add_string out (input_line ic);
           Buffer.add_char out '\n'
         done
       with End_of_file -> ());
      let status = Unix.close_process_in ic in
      (status, Buffer.contents out, Util.read_file err))

let verdict (status, out, err) =
  match List.find_opt (fun l -> l = "sat" || l = "unsat" || l = "unknown") (Util.lines out) with
  | Some v when status = Unix.WEXITED 0 -> Ok (v, out)
  | _ ->
      Error
        (Printf.sprintf "z3 gave no verdict%s\n%s%s"
           (match status with Unix.WEXITED c -> Printf.sprintf " (exit %d)" c | _ -> " (killed)")
           out err)

(* the model's input bits, from the (get-value ...) answer *)
let bits_of_model n_bits out =
  let bits = Bytes.make n_bits '?' in
  let re = Str.regexp "(I_\\([0-9]+\\) \\(true\\|false\\))" in
  let pos = ref 0 in
  (try
     while true do
       ignore (Str.search_forward re out !pos);
       let t = int_of_string (Str.matched_group 1 out) in
       if t < n_bits then Bytes.set bits t (if Str.matched_group 2 out = "true" then '1' else '0');
       pos := Str.match_end ()
     done
   with Not_found -> ());
  let s = Bytes.to_string bits in
  if String.contains s '?' then Error ("could not read the model:\n" ^ out) else Ok s

let ( let* ) = Result.bind

let solve_unique (nl : Sim.t) ~n_bits ~post_reset : (string * bool, string) result =
  let* success =
    Option.to_result (Sim.flop_driving nl "success")
      ~none:"the design has no success port driven by a flop; is this the puzzle GDS?"
  in
  let rst_n = List.assoc_opt "rst_n" nl.ports in
  let* () =
    Array.fold_left
      (fun acc (d : Sim.Dff.t) ->
        let* () = acc in
        let wired pin = pin = None || pin = rst_n in
        if wired d.rst && wired d.set then Ok ()
        else
          Error
            (Printf.sprintf
               "flop %d has an asynchronous set/reset not wired to rst_n; \
                the unrolling assumes reset is held off"
               d.id))
      (Ok ()) nl.dffs
  in
  if not (z3_available ()) then
    Error "z3 not found in PATH (pip install z3-solver, or apt install z3)"
  else begin
    let base = formula nl ~n_bits ~post_reset ~success in
    let inputs = String.concat " " (List.init n_bits (fun t -> Printf.sprintf "I_%d" t)) in
    let* v, out = verdict (run_z3 (base ^ "(check-sat)\n(get-value (" ^ inputs ^ "))\n")) in
    match v with
    | "sat" ->
        let* bits = bits_of_model n_bits out in
        let block =
          String.concat " "
            (List.init n_bits (fun t ->
                 Printf.sprintf "(= I_%d %s)" t (if bits.[t] = '1' then "true" else "false")))
        in
        let* v2, _ = verdict (run_z3 (base ^ "(assert (not (and " ^ block ^ ")))\n(check-sat)\n")) in
        Ok (bits, v2 = "unsat")
    | other -> Error ("z3 answered " ^ other ^ " on the witness query")
  end
