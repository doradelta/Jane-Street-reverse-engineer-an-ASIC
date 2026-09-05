(* Gate-level netlist and cycle-accurate simulator, a port of re/sim.py:
   async set/reset flops, combinational settling in topological order. *)

module Dff = struct
  type t = {
    id : int;
    kind : Cells.dff_kind;
    clk : int option;
    d : int option;
    q : int option;
    rst : int option;
    set : int option;
  }
end

module Comb = struct
  type t = {
    id : int;
    typ : string;
    out_net : int option;
    ins : (string * int option) list;
    fn : Cells.fn;
  }
end

type driver = Const of int | Dff_q of int | Comb_out of int | Input of string

type t = {
  n_nets : int;
  ports : (string * int) list;
  in_ports : (string * int) list;
  consts : (int * int) list;
  comb : Comb.t array;
  dffs : Dff.t array;
  order : int array;
  cyclic : bool;
}

let settle_passes = 8

let of_extract ?(warn = prerr_endline) (nl : Extract.netlist) : t =
  let driver = Hashtbl.create 1024 in
  let set_driver net d =
    match Hashtbl.find_opt driver net with
    | None -> Hashtbl.replace driver net d
    | Some (Const v) when d = Const v -> () (* tie cells in parallel *)
    | Some _ -> warn (Printf.sprintf "WARNING: net %d has more than one driver" net)
  in
  let consts = ref [] and comb = ref [] and dffs = ref [] in
  let n_comb = ref 0 and n_dff = ref 0 in
  List.iter
    (fun (inst : _ Extract.inst) ->
      let pin k = Option.join (List.assoc_opt k inst.pins) in
      match Cells.kind inst.typ with
      | Cells.Phys -> ()
      | Cells.Conb ->
          List.iter
            (fun (k, v) ->
              Option.iter
                (fun net ->
                  if not (List.mem_assoc net !consts) then consts := (net, v) :: !consts;
                  set_driver net (Const v))
                (pin k))
            [ ("HI", 1); ("LO", 0) ]
      | Cells.Dff kind ->
          let dff =
            { Dff.id = inst.id; kind; clk = pin "CLK"; d = pin "D"; q = pin "Q";
              rst = pin "RESET_B"; set = pin "SET_B" }
          in
          Option.iter (fun q -> set_driver q (Dff_q !n_dff)) dff.q;
          dffs := dff :: !dffs;
          incr n_dff
      | Cells.Comb (out, fn) ->
          let gate =
            { Comb.id = inst.id; typ = inst.typ; out_net = pin out;
              ins = List.filter (fun (k, _) -> k <> out) inst.pins; fn }
          in
          Option.iter (fun o -> set_driver o (Comb_out !n_comb)) gate.out_net;
          comb := gate :: !comb;
          incr n_comb)
    nl.insts;
  let comb = Array.of_list (List.rev !comb) and dffs = Array.of_list (List.rev !dffs) in
  (* ports nothing drives are the primary inputs *)
  let in_ports =
    List.fold_left
      (fun acc (name, net) ->
        if Extract.is_rail name || Hashtbl.mem driver net then acc
        else begin
          Hashtbl.replace driver net (Input name);
          (name, net) :: acc
        end)
      [] nl.ports
    |> List.rev
  in
  (* topological order of the gates (Kahn), loads visited in ascending gate
     index so the order equals the Python one even in the cyclic fallback *)
  let n = Array.length comb in
  let loads = Array.make n [] and indeg = Array.make n 0 in
  Array.iteri
    (fun ci (c : Comb.t) ->
      let seen = ref [] in
      List.iter
        (fun (_, net) ->
          match Option.bind net (Hashtbl.find_opt driver) with
          | Some (Comb_out d) when not (List.mem d !seen) ->
              seen := d :: !seen;
              loads.(d) <- ci :: loads.(d);
              indeg.(ci) <- indeg.(ci) + 1
          | _ -> ())
        c.ins)
    comb;
  let loads = Array.map List.rev loads in
  let queue = Queue.create () in
  Array.iteri (fun ci k -> if k = 0 then Queue.add ci queue) indeg;
  let order = ref [] in
  while not (Queue.is_empty queue) do
    let ci = Queue.pop queue in
    order := ci :: !order;
    List.iter
      (fun l ->
        indeg.(l) <- indeg.(l) - 1;
        if indeg.(l) = 0 then Queue.add l queue)
      loads.(ci)
  done;
  let order = List.rev !order in
  let cyclic = List.length order <> n in
  let order =
    if not cyclic then order
    else begin
      let placed = Array.make n false in
      List.iter (fun ci -> placed.(ci) <- true) order;
      let rest = List.filter (fun ci -> not placed.(ci)) (List.init n Fun.id) in
      warn (Printf.sprintf "WARNING: combinational cycle among %d gates" (List.length rest));
      order @ rest
    end
  in
  { n_nets = nl.n_nets; ports = nl.ports; in_ports; consts = List.rev !consts; comb; dffs;
    order = Array.of_list order; cyclic }

(* --- simulation ------------------------------------------------------------ *)

type state = { nets : int array; q : int array; clkprev : int array }

let new_state t =
  let nd = Array.length t.dffs in
  { nets = Array.make t.n_nets 0; q = Array.make nd 0; clkprev = Array.make nd 0 }

let net_val nets = function Some n -> nets.(n) | None -> 0

(* the net on input pin [k] of a gate: a pin missing from the netlist is a
   hard error (the Python raises KeyError), a dangling pin reads 0 *)
let input_net (c : Comb.t) k =
  match List.assoc_opt k c.ins with
  | Some net -> net
  | None -> Util.fail "instance %d (%s) has no pin %s" c.id c.typ k

let eval_comb t st inputs =
  let nets = st.nets in
  List.iter (fun (net, v) -> nets.(net) <- v) t.consts;
  List.iter
    (fun (name, net) -> Option.iter (fun v -> nets.(net) <- v) (List.assoc_opt name inputs))
    t.in_ports;
  Array.iteri
    (fun di (dff : Dff.t) -> Option.iter (fun q -> nets.(q) <- st.q.(di)) dff.q)
    t.dffs;
  for _ = 1 to if t.cyclic then settle_passes else 1 do
    Array.iter
      (fun ci ->
        let c = t.comb.(ci) in
        Option.iter
          (fun o -> nets.(o) <- c.fn.eval Cells.ints (fun k -> net_val nets (input_net c k)))
          c.out_net)
      t.order
  done

let step t st inputs =
  eval_comb t st inputs;
  let nets = st.nets in
  let newq = Array.copy st.q in
  Array.iteri
    (fun di (dff : Dff.t) ->
      let clk = net_val nets dff.clk in
      let rising = st.clkprev.(di) = 0 && clk = 1 in
      st.clkprev.(di) <- clk;
      match dff.kind with
      | Cells.Rtp when dff.rst <> None && net_val nets dff.rst = 0 -> newq.(di) <- 0
      | Cells.Stp when dff.set <> None && net_val nets dff.set = 0 -> newq.(di) <- 1
      | _ -> if rising then newq.(di) <- net_val nets dff.d)
    t.dffs;
  Array.blit newq 0 st.q 0 (Array.length newq);
  eval_comb t st inputs

let read t st name = Option.map (fun net -> st.nets.(net)) (List.assoc_opt name t.ports)

(* O[0] is the least significant bit *)
let read_byte t st =
  let rec go b acc =
    if b = 8 then Some acc
    else
      match read t st (Printf.sprintf "O[%d]" b) with
      | None -> None
      | Some v -> go (b + 1) (acc lor (v lsl b))
  in
  go 0 0

let find_dff t ~id =
  let r = ref None in
  Array.iteri (fun i (d : Dff.t) -> if d.id = id then r := Some i) t.dffs;
  !r

let flop_driving t port =
  match List.assoc_opt port t.ports with
  | None -> None
  | Some net ->
      let r = ref None in
      Array.iteri (fun i (d : Dff.t) -> if d.q = Some net then r := Some i) t.dffs;
      !r
