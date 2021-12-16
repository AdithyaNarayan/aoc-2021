let maybe_read_line () = try Some(read_line()) with End_of_file -> None

let read_all () =
  let rec f acc =
    match maybe_read_line () with
    | Some (line) -> f (line :: acc)
    | None -> List.rev acc
  in
  f []

let str_to_char_list s = List.init (String.length s) (String.get s)

let rec take n l = if n <= 0 then [] else
  match l with
  | [] -> []
  | hd :: tl -> hd :: take (n - 1) tl

let rec drop n l = if n == 0 then l else
  match l with
  | [] -> []
  | _ :: tl -> drop (n - 1) tl

let split_at n l = (take n l, drop n l)

let hex_to_bin = function
| '0' -> "0000" | '1' -> "0001" | '2' -> "0010" | '3' -> "0011" 
| '4' -> "0100" | '5' -> "0101" | '6' -> "0110" | '7' -> "0111" 
| '8' -> "1000" | '9' -> "1001" | 'A' -> "1010" | 'B' -> "1011"
| 'C' -> "1100" | 'D' -> "1101" | 'E' -> "1110" | 'F' -> "1111"
| _ -> ""

let bin_char_list_to_dec = 
  let rec helper acc = function
  | [] -> acc
  | hd :: tl -> helper (acc * 2 + (int_of_char hd) - 48) tl
  in
  helper 0

let parse_input l =
  List.hd l
  |> str_to_char_list
  |> List.map (hex_to_bin)
  |> List.fold_left (^) ""
  |> str_to_char_list

type packet = 
  | OperatorPacket of {
    version: int;
    type_id: int;
    sub_packets: packet list;
  }
  | LiteralPacket of {
    version: int;
    value: int;
  }

let [@warning "-8"] rec parse_literal num l =
  let check :: bits, rest = split_at 5 l 
  in
  if check = '1' then
    parse_literal (num @ bits) rest
  else
    bin_char_list_to_dec (num @ bits), rest

let [@warning "-8"] rec parse_operator sub_list l =
  let length_type :: [], rest = split_at 1 l
  in
  if length_type = '0' then
    let length, rest = split_at 15 rest |> fun (x, y) -> bin_char_list_to_dec x, y in
    let cur, rest = split_at length rest in
    let rec helper acc cur = 
      if cur = [] then acc 
      else
        let (parsed, rest) = parse_and_rest cur
        in
        helper (acc @ [parsed]) rest
    in
    helper [] cur, rest
  else
    let num, rest = split_at 11 rest |> fun (x, y) -> bin_char_list_to_dec x, y in
    let rec helper acc n cur = 
      if n = 0 then (acc, cur) 
      else
        let (parsed, rest) = parse_and_rest cur
        in
        helper (acc @ [parsed]) (n - 1) rest
    in
    helper [] num rest

and

parse_and_rest l =
  let (version, rest) = 
    split_at 3 l
    |> fun (x, y) -> bin_char_list_to_dec x, y in
  let (type_id, rest) =
    split_at 3 rest
    |> fun (x, y) -> bin_char_list_to_dec x, y
  in
  if type_id = 4 then
    let value, rest = parse_literal [] rest
    in
    LiteralPacket { version; value}, rest
  else
    let sub_packets, rest = parse_operator [] rest
    in
    OperatorPacket { version; type_id; sub_packets}, rest

let rec value = function
| LiteralPacket {value} -> value
| OperatorPacket {type_id; sub_packets} ->
  match type_id with
  | n when 0 <= n && n <= 3 ->
    let f, init =
      match n with
      | 0 -> (fun sum x -> sum + value x), 0
      | 1 -> (fun prod x -> prod * value x), 1
      | 2 -> (fun min x -> let v = value x in if v < min then v else min), max_int
      | _ -> (fun max x -> let v = value x in if v > max then v else max), 0
    in
    List.fold_left f init sub_packets
  | n when 5 <= n && n <= 7 -> 
    let first = value @@ List.hd sub_packets in
    let second = value @@ List.nth sub_packets 1 in
    let comp = match n with
      | 5 -> (>)
      | 6 -> (<)
      | _ -> (=)
    in 
    if comp first second then 1 else 0    
  | _ -> 0

let f x = 
  let parsed, _ = parse_and_rest x 
  in
  value parsed

let _ =
  read_all ()
  |> parse_input
  |> f
  |> print_int
  |> print_newline
