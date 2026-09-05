(* Boolean models of the sky130_fd_sc_hd cells used by the puzzle.

   Each combinational cell is written once against an abstract boolean
   algebra, so the same definition drives the integer simulator and the
   SMT-LIB emitter. The universal sky130 rule holds: a pin whose name ends in
   `_N` enters the base AND-OR / OR-AND expression inverted. *)

type 'a alg = {
  const : bool -> 'a;
  not_ : 'a -> 'a;
  and_ : 'a -> 'a -> 'a;
  or_ : 'a -> 'a -> 'a;
  xor_ : 'a -> 'a -> 'a;
  ite : 'a -> 'a -> 'a -> 'a;  (* select, if-true, if-false *)
}

type fn = { eval : 'a. 'a alg -> (string -> 'a) -> 'a }

type dff_kind =
  | Rtp  (* async active-low reset, posedge *)
  | Stp  (* async active-low set, posedge *)
  | Xtp  (* plain posedge *)

type kind =
  | Comb of string * fn  (* output pin, function *)
  | Dff of dff_kind
  | Conb                 (* constant tie cell: HI = 1, LO = 0 *)
  | Phys                 (* no signal function *)

let table : (string * kind) list =
  (* logical value of pin k, inverted when its name ends in _N *)
  let pin g p k = if String.ends_with ~suffix:"_N" k then g.not_ (p k) else p k in
  let pins g p ks = List.map (pin g p) ks in
  let all g = function [] -> g.const true | x :: xs -> List.fold_left g.and_ x xs in
  let any g = function [] -> g.const false | x :: xs -> List.fold_left g.or_ x xs in
  let and_of ks = Comb ("X", { eval = (fun g p -> all g (pins g p ks)) }) in
  let nand_of ks = Comb ("Y", { eval = (fun g p -> g.not_ (all g (pins g p ks))) }) in
  let or_of ks = Comb ("X", { eval = (fun g p -> any g (pins g p ks)) }) in
  let nor_of ks = Comb ("Y", { eval = (fun g p -> g.not_ (any g (pins g p ks))) }) in
  (* AND-OR: groups of pins ANDed, then ORed; [inv] for the inverting variant *)
  let ao ~inv groups =
    Comb
      ( (if inv then "Y" else "X"),
        { eval = (fun g p ->
            let e = any g (List.map (fun ks -> all g (pins g p ks)) groups) in
            if inv then g.not_ e else e) } )
  in
  (* OR-AND: groups of pins ORed, then ANDed *)
  let oa ~inv groups =
    Comb
      ( (if inv then "Y" else "X"),
        { eval = (fun g p ->
            let e = all g (List.map (fun ks -> any g (pins g p ks)) groups) in
            if inv then g.not_ e else e) } )
  in
  let buffer = Comb ("X", { eval = (fun _ p -> p "A") }) in
  [
    (* buffers / inverter *)
    ("buf_2", buffer);
    ("clkbuf_4", buffer);
    ("clkbuf_8", buffer);
    ("clkbuf_16", buffer);
    ("inv_2", Comb ("Y", { eval = (fun g p -> g.not_ (p "A")) }));
    (* simple gates *)
    ("and2_2", and_of [ "A"; "B" ]);
    ("and3_2", and_of [ "A"; "B"; "C" ]);
    ("and4_2", and_of [ "A"; "B"; "C"; "D" ]);
    ("or2_2", or_of [ "A"; "B" ]);
    ("or3_2", or_of [ "A"; "B"; "C" ]);
    ("or4_2", or_of [ "A"; "B"; "C"; "D" ]);
    ("nand2_2", nand_of [ "A"; "B" ]);
    ("nand3_2", nand_of [ "A"; "B"; "C" ]);
    ("nand4_2", nand_of [ "A"; "B"; "C"; "D" ]);
    ("nor2_2", nor_of [ "A"; "B" ]);
    ("nor3_2", nor_of [ "A"; "B"; "C" ]);
    ("nor4_2", nor_of [ "A"; "B"; "C"; "D" ]);
    ("xor2_2", Comb ("X", { eval = (fun g p -> g.xor_ (p "A") (p "B")) }));
    ("xnor2_2", Comb ("Y", { eval = (fun g p -> g.not_ (g.xor_ (p "A") (p "B"))) }));
    ("mux2_1", Comb ("X", { eval = (fun g p -> g.ite (p "S") (p "A1") (p "A0")) }));
    (* inverted-input simple gates *)
    ("and2b_2", and_of [ "A_N"; "B" ]);
    ("and3b_2", and_of [ "A_N"; "B"; "C" ]);
    ("and4b_2", and_of [ "A_N"; "B"; "C"; "D" ]);
    ("and4bb_2", and_of [ "A_N"; "B_N"; "C"; "D" ]);
    ("nand2b_2", nand_of [ "A_N"; "B" ]);
    ("nand3b_2", nand_of [ "A_N"; "B"; "C" ]);
    ("nor3b_2", nor_of [ "A"; "B"; "C_N" ]);
    ("nor4b_2", nor_of [ "A"; "B"; "C"; "D_N" ]);
    ("or3b_2", or_of [ "A"; "B"; "C_N" ]);
    ("or4b_2", or_of [ "A"; "B"; "C"; "D_N" ]);
    ("or4bb_2", or_of [ "A"; "B"; "C_N"; "D_N" ]);
    (* AND-OR *)
    ("a21o_2", ao ~inv:false [ [ "A1"; "A2" ]; [ "B1" ] ]);
    ("a21oi_2", ao ~inv:true [ [ "A1"; "A2" ]; [ "B1" ] ]);
    ("a211o_2", ao ~inv:false [ [ "A1"; "A2" ]; [ "B1" ]; [ "C1" ] ]);
    ("a211oi_2", ao ~inv:true [ [ "A1"; "A2" ]; [ "B1" ]; [ "C1" ] ]);
    ("a221o_2", ao ~inv:false [ [ "A1"; "A2" ]; [ "B1"; "B2" ]; [ "C1" ] ]);
    ("a221oi_2", ao ~inv:true [ [ "A1"; "A2" ]; [ "B1"; "B2" ]; [ "C1" ] ]);
    ("a22o_2", ao ~inv:false [ [ "A1"; "A2" ]; [ "B1"; "B2" ] ]);
    ("a22oi_2", ao ~inv:true [ [ "A1"; "A2" ]; [ "B1"; "B2" ] ]);
    ("a311o_2", ao ~inv:false [ [ "A1"; "A2"; "A3" ]; [ "B1" ]; [ "C1" ] ]);
    ("a31o_2", ao ~inv:false [ [ "A1"; "A2"; "A3" ]; [ "B1" ] ]);
    ("a31oi_2", ao ~inv:true [ [ "A1"; "A2"; "A3" ]; [ "B1" ] ]);
    ("a32o_2", ao ~inv:false [ [ "A1"; "A2"; "A3" ]; [ "B1"; "B2" ] ]);
    ("a41oi_2", ao ~inv:true [ [ "A1"; "A2"; "A3"; "A4" ]; [ "B1" ] ]);
    ("a2111oi_2", ao ~inv:true [ [ "A1"; "A2" ]; [ "B1" ]; [ "C1" ]; [ "D1" ] ]);
    ("a21bo_2", ao ~inv:false [ [ "A1"; "A2" ]; [ "B1_N" ] ]);
    ("a21boi_2", ao ~inv:true [ [ "A1"; "A2" ]; [ "B1_N" ] ]);
    (* OR-AND *)
    ("o21a_2", oa ~inv:false [ [ "A1"; "A2" ]; [ "B1" ] ]);
    ("o21ai_2", oa ~inv:true [ [ "A1"; "A2" ]; [ "B1" ] ]);
    ("o211a_2", oa ~inv:false [ [ "A1"; "A2" ]; [ "B1" ]; [ "C1" ] ]);
    ("o211ai_2", oa ~inv:true [ [ "A1"; "A2" ]; [ "B1" ]; [ "C1" ] ]);
    ("o221a_2", oa ~inv:false [ [ "A1"; "A2" ]; [ "B1"; "B2" ]; [ "C1" ] ]);
    ("o22a_2", oa ~inv:false [ [ "A1"; "A2" ]; [ "B1"; "B2" ] ]);
    ("o22ai_2", oa ~inv:true [ [ "A1"; "A2" ]; [ "B1"; "B2" ] ]);
    ("o311a_2", oa ~inv:false [ [ "A1"; "A2"; "A3" ]; [ "B1" ]; [ "C1" ] ]);
    ("o31a_2", oa ~inv:false [ [ "A1"; "A2"; "A3" ]; [ "B1" ] ]);
    ("o31ai_2", oa ~inv:true [ [ "A1"; "A2"; "A3" ]; [ "B1" ] ]);
    ("o32a_2", oa ~inv:false [ [ "A1"; "A2"; "A3" ]; [ "B1"; "B2" ] ]);
    ("o32ai_2", oa ~inv:true [ [ "A1"; "A2"; "A3" ]; [ "B1"; "B2" ] ]);
    ("o21ba_2", oa ~inv:false [ [ "A1"; "A2" ]; [ "B1_N" ] ]);
    ("o21bai_2", oa ~inv:true [ [ "A1"; "A2" ]; [ "B1_N" ] ]);
    ("o2bb2a_2", oa ~inv:false [ [ "A1_N"; "A2_N" ]; [ "B1"; "B2" ] ]);
    (* constants and flops *)
    ("conb_1", Conb);
    ("dfrtp_2", Dff Rtp);
    ("dfstp_2", Dff Stp);
    ("dfxtp_2", Dff Xtp);
    (* no signal function; the fill cells have no signal pins either, so
       unlike the Python they are accepted rather than rejected *)
    ("tapvpwrvgnd_1", Phys);
    ("decap_3", Phys);
    ("diode_2", Phys);
    ("fill_1", Phys);
    ("fill_2", Phys);
    ("fill_4", Phys);
    ("fill_8", Phys);
  ]

let by_name = Hashtbl.of_seq (List.to_seq table)

let kind typ =
  match Hashtbl.find_opt by_name typ with
  | Some k -> k
  | None -> Util.fail "unknown cell type %s" typ

(* the algebra of plain 0/1 integers, for simulation *)
let ints : int alg =
  {
    const = (fun b -> if b then 1 else 0);
    not_ = (fun x -> x lxor 1);
    and_ = ( land );
    or_ = ( lor );
    xor_ = ( lxor );
    ite = (fun s a b -> if s = 1 then a else b);
  }
