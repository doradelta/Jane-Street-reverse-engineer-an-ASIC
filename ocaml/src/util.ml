let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let write_file path contents =
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc contents)

let lines s = List.map String.trim (String.split_on_char '\n' s)

let is_space c = c = ' ' || c = '\t' || c = '\r' || c = '\n'

let words s =
  let n = String.length s in
  let rec go i acc =
    if i >= n then List.rev acc
    else if is_space s.[i] then go (i + 1) acc
    else begin
      let j = ref i in
      while !j < n && not (is_space s.[!j]) do incr j done;
      go !j (String.sub s i (!j - i) :: acc)
    end
  in
  go 0 []

let fail fmt = Printf.ksprintf failwith fmt
