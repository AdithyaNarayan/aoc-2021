let maybe_read_line () = try Some(read_line()) with End_of_file -> None

let read_all () =
  let rec f acc =
    match maybe_read_line () with
    | Some (line) -> f (line :: acc)
    | None -> List.rev acc
  in
  f []

module Graph = Map.Make(String)

let create_map =
  let add_edges_to_map map l =
    let add_one_direction map k v = if k = "end" then map else match Graph.find_opt k map with
    | Some(l) -> Graph.add k (v :: l) map
    | None -> Graph.add k [v] map
    in
    match l with
    | k :: v :: [] -> add_one_direction (add_one_direction map k v) v k
    | _ -> map
  in
  let rec helper (m: string list Graph.t) = function
  | [] -> m
  | hd :: tl -> helper (add_edges_to_map m hd) tl
  in
  helper Graph.empty

let all_caps x =
  x 
  |> String.map (fun c -> if int_of_char c >= 65 && int_of_char c <= 90 then '1' else '0')
  |> (fun x -> x = (String.make (String.length x) '1'))

let is_not_closed visited (node: string) = 
  if node = "start" then false else
  if all_caps node then true else
  match (
    visited
    |> List.filter (fun x -> x |> all_caps |> not)
    |> List.filter (fun x -> x != "start")
    |> (
      fun x -> 
        let unique = List.sort_uniq (fun x y -> if x = y then 0 else if x > y then 1 else -1) x
        in
        List.length unique = List.length x
    )
  ) with
  | false -> (* small cave has been visited twice *) not @@ List.mem node visited
  | _ -> true

let f map =
  let rec dfs visited map curr =
    match Graph.find_opt curr map with
    | Some(neighbors) ->
      neighbors
      |> List.filter @@ is_not_closed (visited @ [curr])
      |> List.map @@ dfs (visited @ [curr]) map
      |> List.fold_left (+) 0
    | None -> 1
    in dfs [] map "start"

let _ =
  read_all ()
  |> List.map (String.split_on_char '-')
  |> create_map
  |> f
  |> print_int
  |> print_newline
