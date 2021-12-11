let maybe_read_line () = try Some(read_line()) with End_of_file -> None ;;

let read_all () =
  let rec f acc =
    match maybe_read_line () with
    | Some (line) -> f (line :: acc)
    | None -> List.rev acc
  in
  f []

let str_to_char_list s = List.init (String.length s) (String.get s)

let inc_2d x y = 
  let inc i = List.mapi (
    fun id x -> if id == i then 
      match x with
      | 0 | 9 -> 0
      | x -> x + 1
    else x
    )
  in
  List.mapi (fun id l -> if id == x then inc y l else l)

let get x y l = List.nth (List.nth l x) y

let neighbours l (x, y) =
  [(x - 1, y - 1); (x - 1, y); (x - 1, y + 1); (x, y - 1); (x, y + 1); (x + 1, y - 1); (x + 1, y); (x + 1, y + 1)]
  |> List.filter (fun (x,y) -> (x >= 0) && (x < List.length l) && (y >= 0) && (y < List.length @@ List.hd l))

let step x =
  let unflashed_neighbours_of graph l =
    l
    |> List.map (neighbours graph)
    |> List.concat
    |> List.filter (fun (x, y) -> get x y graph > 0) in
  let first_inc = List.map (List.map (fun x -> if x == 9 then 0 else x + 1)) x in
  let first_neighbours =
    let indexed = List.mapi (fun i -> List.mapi (fun j e -> ((i, j), e))) first_inc in
    List.fold_left (fun acc x -> List.fold_left (fun acc ((i, j), e) -> if e == 0 then (i, j) :: acc else acc) acc x) [] indexed
    |> unflashed_neighbours_of first_inc
  in
  let rec helper acc graph = match acc with
    | [] -> graph
    | acc ->
      let inc = List.fold_left (fun g (x, y) -> inc_2d x y g) graph acc in
      let nbs = 
        List.fold_left (fun acc (x, y) -> if get x y inc == 0 then (x, y) :: acc else acc) [] acc
        |> List.sort_uniq (fun (x1, y1) (x2, y2) -> if x1 == x2 then y1 - y2 else x1 - x2)
        |> List.filter (fun (x, y) -> get x y graph > 0)
        |> unflashed_neighbours_of inc
      in
      helper nbs inc
  in helper first_neighbours first_inc

let f graph =
  let rec helper graph sum = function
  | 100 -> sum
  | i ->
    let next = step graph in
    let num_flashed = List.fold_left (fun acc l -> acc + List.fold_left (fun acc x -> if x == 0 then acc + 1 else acc) 0 l) 0 next
    in
    helper next (sum + num_flashed) (i + 1)
  in
  helper graph 0 0

let _ =
  read_all ()
  |> List.map str_to_char_list
  |> List.map @@ List.map (fun x -> x |> int_of_char |> ((-) 48) |> (~-))
  |> f
  |> print_int
  |> print_newline
