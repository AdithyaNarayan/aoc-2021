let maybe_read_line () = try Some(read_line()) with End_of_file -> None

let read_all () =
  let rec f acc =
    match maybe_read_line () with
    | Some (line) -> f (line :: acc)
    | None -> List.rev acc
  in
  f []

let str_to_char_list s = List.init (String.length s) (String.get s)

type snail =
  | BaseSnail of int
  | Snail of snail * snail

let parse_input l =
  let find_comma s =
    str_to_char_list s
    |> List.mapi (fun i c -> (i, c))
    |> List.fold_left (
      fun (count, i) (cur_index, c) ->
        match c with
        | '[' -> (count + 1, i)
        | ']' -> (count - 1, i)
        | ',' -> if count = 0 && i = -1 then (count, cur_index) else (count, i)
        | _ -> (count, i)
    ) (0, -1)
    |> fun (_, y) -> y in
  let rec parse_snail s =
    match String.length s with
    | 1 -> BaseSnail (int_of_string s)
    | n ->
      String.sub s 1 (n - 2)
      |> fun s ->
        let i = find_comma s in
        let first = String.sub s 0 i in
        let second = String.sub s (i + 1) (String.length s - i - 1)
        in
        Snail (parse_snail first, parse_snail second)
  in
  l
  |> List.map parse_snail

let rec eq s1 s2 = 
  match s1 with
  | BaseSnail n1 ->
    (
      match s2 with
      | BaseSnail n2 -> n1 = n2
      | _ -> false
    )
  | Snail (s11, s12) ->
    (
      match s2 with
      | BaseSnail _ -> false
      | Snail (s21, s22) -> eq s11 s21 && eq s12 s22
    )

let rec explode num s =
  let rec add_to_first_base num =
    function
    | BaseSnail x -> BaseSnail (x + num)
    | Snail (s1, s2) -> Snail (add_to_first_base num s1, s2) in
  let rec add_to_last_base num =
    function
    | BaseSnail x -> BaseSnail (x + num)
    | Snail (s1, s2) -> Snail (s1, add_to_last_base num s2) 
  in
  match num with
  | 0 ->
    (
      match s with
      | Snail(BaseSnail x, BaseSnail y) ->
        (x, BaseSnail 0, y)
      | _ -> (0, s, 0)
    )
  | _ ->
    (
      match s with
      | Snail(s1, s2) ->
        let (left, new_s1, right) = explode (num - 1) s1
        in
        if eq s1 new_s1 then 
          let (left, new_s2, right) = explode (num - 1) s2
          in
          (0, Snail(add_to_last_base left s1, new_s2), right)
        else 
          (left, Snail(new_s1, add_to_first_base right s2), 0)
      | _ -> (0, s, 0) 
    )

let rec split s =
  match s with
  | BaseSnail(n) when n < 10 -> s
  | BaseSnail(n) when n mod 2 = 0 -> Snail(BaseSnail(n / 2), BaseSnail(n / 2))
  | BaseSnail(n) -> Snail(BaseSnail(n / 2), BaseSnail((n / 2) + 1))
  | Snail(s1, s2) ->
    let new_s1 = split s1
    in
    if eq s1 new_s1 then
      Snail(s1, split s2)
    else
      Snail(new_s1, s2)

let reduce s =
  let rec helper s =
    let (_, e, _) = explode 4 s
    in
    if eq s e then
      let sp = split s
      in
      if eq sp s then
        s
      else
        helper sp
    else
      helper e
  in
  helper s

let add s1 s2 = reduce @@ Snail (s1, s2)

let rec magnitude =
  function
  | BaseSnail x -> x
  | Snail(s1, s2) -> 3 * magnitude s1 + 2 * magnitude s2

let uncurry f (x, y) = f x y

let f l = 
  List.concat_map (fun s1 -> List.map (fun s2 -> (s1, s2)) l) l
  |> List.map (uncurry @@ add)
  |> List.map magnitude
  |> List.fold_left (fun max e -> if e > max then e else max) 0

let _ =
  read_all ()
  |> parse_input
  |> f
  |> print_int
  |> print_newline
