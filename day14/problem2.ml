let maybe_read_line () = try Some(read_line()) with End_of_file -> None

let read_all () =
  let rec f acc =
    match maybe_read_line () with
    | Some (line) -> f (line :: acc)
    | None -> List.rev acc
  in
  f []

let findi = 
  let rec helper i pred l = 
    match l with 
    | [] -> raise Not_found 
    | hd :: _ when pred hd -> i 
    | _ :: tl -> helper (i + 1) pred tl
  in
  helper 0

let rec take n l = if n <= 0 then [] else
  match l with
  | [] -> []
  | hd :: tl -> hd :: take (n - 1) tl

let rec drop n l = if n == 0 then l else
  match l with
  | [] -> []
  | _ :: tl -> drop (n - 1) tl

module Pairs = Map.Make(String)

let str_to_char_list s = List.init (String.length s) (String.get s)

let parse_input l = 
  let i = findi ((=) "") l in
  let template = str_to_char_list @@ List.hd @@ take i l in
  let pairs =
    drop (i + 1) l
    |> List.map (fun s -> Scanf.sscanf s "%s -> %s" (fun x y -> (x, y)))
    |> List.fold_left (fun acc (x, y) -> Pairs.add x y acc) Pairs.empty 
  in
  (template, pairs)

let uncurry f (x, y) = f x y

let two_window x =
  List.combine x (List.tl x @ [' '])
  |> take (List.length x - 1)
  |> List.map (fun (x, y) -> (String.make 1 x) ^ (String.make 1 y))

let change_to_freq template pairs =
  let freq = List.fold_left 
  (
    fun acc k -> match Pairs.find_opt k acc with
    | Some(i) -> Pairs.add k (i + 1) acc
    | None -> Pairs.add k 1 acc
  ) 
  (Pairs.map (fun v -> 0) pairs)
  (two_window template)
  in
  (freq, pairs)

let f freq pairs =
  let step freq pairs =
    Pairs.fold
    (
      fun pair count acc -> match Pairs.find_opt pair pairs with
      | Some(c) ->
        let new_pair_1 = (String.sub pair 0 1) ^ c in
        let new_pair_2 = c ^ (String.sub pair 1 1)
        in
        acc
        |> Pairs.add new_pair_1 (Pairs.find new_pair_1 acc + count)
        |> Pairs.add new_pair_2 (Pairs.find new_pair_2 acc + count)
      | None -> acc
    )
    freq
    (Pairs.map (fun v -> 0) pairs)
    in
  let rec step_runner freq pairs = function
  | 0 -> freq
  | i -> step_runner (step freq pairs) pairs (i - 1)
  in
  step_runner freq pairs 40
  |> (fun x ->
    Pairs.fold (
      fun pair count acc ->
        let c1 = String.sub pair 0 1 in
        let c2 = String.sub pair 1 1
        in
        acc
        |> Pairs.add c1 (
          match Pairs.find_opt c1 acc with
          | Some(i) -> i + count
          | None -> count
        )
        |> fun x -> Pairs.add c2 (
          match Pairs.find_opt c2 x with
          | Some(i) -> i + count
          | None -> count
        ) x
    ) x Pairs.empty
    |> Pairs.map (fun x -> if x mod 2 = 0 then x / 2 else x / 2 + 1)
  )
  |> (
      fun x ->
      Pairs.fold 
        (
          fun _ i (max, min) -> 
            if i < min && i > max then (i, i)
            else if i < min then (max, i) 
            else if i > max then (i, min) 
            else (max, min)
        ) 
        x
        (0, max_int)
    )
    |> uncurry @@ (-)

let _ =
  read_all ()
  |> parse_input
  |> uncurry @@ change_to_freq
  |> uncurry @@ f
  |> print_int
  |> print_newline
