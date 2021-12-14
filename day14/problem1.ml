(* naive method for part 1 and better solution for part 2 *)
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
  
let tee f x = f x; x

module FreqMap = Map.Make(Char)

let f template pairs = 
  let step template pairs = 
    template
    |> two_window
    |> List.map (fun x -> String.sub x 0 1 ^ Pairs.find x pairs ^ String.sub x 1 1)
    |> List.mapi (fun i x -> if i < List.length template - 2 then String.sub x 0 2 else x)
    |> List.fold_left (^) ""
    |> str_to_char_list in
  let rec step_runner template pairs = function
  | 0 -> template
  | i -> step_runner (step template pairs) pairs (i - 1)
  in
  step_runner template pairs 10
  |> List.fold_left (
    fun m c -> match (FreqMap.find_opt c m) with
    | Some(i) -> FreqMap.add c (i + 1) m
    | None -> FreqMap.add c 1 m
  ) FreqMap.empty
  |> (
    fun x ->
    FreqMap.fold 
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
  |> uncurry @@ f
  |> print_int
  |> print_newline
