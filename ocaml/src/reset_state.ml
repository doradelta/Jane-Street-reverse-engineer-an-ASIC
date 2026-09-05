(* re/reset_state.json: {"init": [0, 1, ...], "ids": [92, 93, ...]}, the value
   of every flop after reset keyed by instance id *)

type t = { ids : int list; init : int list }

let read path =
  let s = Util.read_file path in
  let ints key =
    let re = Str.regexp ("\"" ^ key ^ "\"[ \t\n]*:[ \t\n]*\\[") in
    match Str.search_forward re s 0 with
    | exception Not_found -> Util.fail "%s: no \"%s\" list" path key
    | _ -> (
        let start = Str.match_end () in
        match String.index_from_opt s start ']' with
        | None -> Util.fail "%s: unterminated \"%s\" list" path key
        | Some stop ->
            String.sub s start (stop - start)
            |> String.split_on_char ','
            |> List.filter_map (fun x ->
                   let x = String.trim x in
                   if x = "" then None
                   else
                     match int_of_string_opt x with
                     | Some v -> Some v
                     | None -> Util.fail "%s: %S is not an integer" path x))
  in
  let t = { ids = ints "ids"; init = ints "init" } in
  if List.length t.ids <> List.length t.init then
    Util.fail "%s: ids and init have different lengths" path;
  t
