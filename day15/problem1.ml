let maybe_read_line () = try Some(read_line()) with End_of_file -> None

let read_all () =
  let rec f acc =
    match maybe_read_line () with
    | Some (line) -> f (line :: acc)
    | None -> List.rev acc
  in
  f []

let str_to_char_list s = List.init (String.length s) (String.get s)

module GridIndex = struct
  type t = int * int
  let compare (x1, y1) (x2, y2) = if x1 != x2 then x1 - x2 else y1 - y2
end

module Graph = Map.Make(GridIndex)
module NodeSet = Set.Make(GridIndex)

module PQValue = struct
  type t = int * (int * int)
  let compare (v1, (x1, y1)) (v2, (x2, y2)) = if v1 != v2 then v1 - v2 else if x1 != x2 then x1 - x2 else y1 - y2
end
module PQ = Batteries.Heap.Make(PQValue)

let neighbours_of i j l =
  [(i, j - 1); (i - 1, j); (i, j + 1); (i + 1, j)]
  |> List.filter (
    fun (x, y) -> x >= 0 && x < List.length l && y >= 0 && y < List.length @@ List.hd l
  )

let parse_input l =
  l
  |> List.map (str_to_char_list)
  |> List.map (List.map (fun x -> x |> int_of_char |> (-) 48 |> (~-)))
  |> List.mapi (fun i -> List.mapi (fun j x -> (i, j, x)))
  |> fun l -> (List.length l - 1, l)
  |> fun (max, l) -> (max, List.fold_left (List.fold_left (
    fun acc (i, j, v) ->
      neighbours_of i j l
      |> List.fold_left (
        fun acc (x, y) -> match Graph.find_opt (x, y) acc with
        | Some(l) -> Graph.add (x, y) (((i, j), v) :: l) acc
        | None -> Graph.add (x, y) [((i, j), v)] acc
      ) acc
  )) Graph.empty l)

let rec dijkstra graph source target visited distances pq =
  match source with
  | _ when source = target -> Graph.find source distances
  | _ ->
    Graph.find source graph
    |> List.filter (fun (i, _) -> NodeSet.find_opt i visited |> Option.is_none)
    |> List.fold_left (
      fun (acc_distances, acc_not_visited) (neighbour, v) ->
        let cost_through_current = Graph.find source acc_distances + v
        in
        match Graph.find_opt neighbour acc_distances with
        | Some(u) -> Graph.add neighbour (min u cost_through_current) acc_distances, PQ.add (min u cost_through_current, neighbour) acc_not_visited
        | None -> Graph.add neighbour cost_through_current acc_distances, PQ.add (cost_through_current, neighbour) acc_not_visited
    ) (distances, pq)
    |> (
      fun (distances, pq) ->
        let rec helper pq =
          let (_, cur_min) = PQ.find_min pq in
          if NodeSet.find_opt cur_min visited |> Option.is_some then
            helper (PQ.del_min pq)
          else
            (cur_min, pq, distances) 
        in
        helper pq
    )
    |> fun (next, new_pq, new_distances) -> 
      dijkstra graph next target (NodeSet.add source visited) new_distances new_pq

let uncurry f (x, y) = f x y

let f max x =
  dijkstra x (0, 0) (max, max) NodeSet.empty (Graph.empty |> Graph.add (0, 0) 0) (PQ.empty |> PQ.add (0, (0, 0)))

let _ =
  read_all ()
  |> parse_input
  |> uncurry @@ f
  |> print_int
  |> print_newline
