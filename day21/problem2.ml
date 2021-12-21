let maybe_read_line () = try Some(read_line()) with End_of_file -> None

let read_all () =
  let rec helper acc =
    match maybe_read_line () with
    | Some (line) -> helper (line :: acc)
    | None -> List.rev acc
  in
  helper []

module GameState =
  struct
    type t = ((int * int) * (int * int)) * int
    let pair_comp (x1, y1) (x2, y2) =
      if x1 = x2 then
        y1 - y2
      else
        x1 - x2

    let compare ((x1, y1), _) ((x2, y2), _) =
      if pair_comp x1 x2 = 0 then
        pair_comp y1 y2
      else
        pair_comp x1 x2
  end

module GameSet = Set.Make(GameState)

let parse_input l =
  List.map (
    fun s ->
      String.split_on_char ':' s
      |> (fun l -> List.nth l (List.length l - 1))
      |> (fun l -> String.sub l 1 (String.length l - 1))
      |> int_of_string
      |> fun i -> (i, 0)
  ) l
  |> fun l -> GameSet.of_list [((List.hd l, List.hd @@ List.tl l), 1)]

let cross3 x =
  let y = List.concat_map (
    fun e1 -> List.map (fun e2 -> (e1, e2)) x
  ) x
  in
  List.concat_map (
    fun (e1, e2) -> (List.map (fun e3 -> (e1, e2, e3))) x
  ) y

let [@warning "-8"] play_turn g =
  let helper (((curpos, curscore), other), count) roll =
    let pos = (curpos + roll - 1) mod 10 + 1
    in
    (other, (pos, curscore + pos)), count
  in
  GameSet.fold (
    fun game acc_g ->
      cross3 [1; 2; 3]
      |> List.map (fun (e1, e2, e3) -> e1 + e2 + e3)
      |> List.fold_left (
        fun acc_g dice ->
          let new_game = helper game dice in
          let (_, count) = new_game
          in
          match GameSet.find_opt (new_game) acc_g with
          | Some((t, c)) -> GameSet.add (t, c + count) (GameSet.remove new_game acc_g)
          | None -> GameSet.add new_game acc_g
      ) acc_g
  ) g GameSet.empty

let f =
  let rec helper accx accy x =
    if GameSet.is_empty x then
      if accx > accy then accx else accy
    else
      let (wins, ongoing) =
        GameSet.fold (
          fun game (acc, new_g) ->
            let ((_, _), (_, otherscore)), count = game
            in
            if otherscore >= 21 then
              (acc + count, GameSet.remove game new_g)
            else
              (acc, new_g)
        ) x (0, x)
      in
      helper accy (accx + wins) (play_turn ongoing)
  in
  helper 0 0

let _ =
  read_all ()
  |> parse_input
  |> f
  (* |> GameSet.iter (fun (((p1, s1), (p2, s2)), count) -> Printf.printf "p1: %d s1: %d p2: %d s2: %d count: %d\n" p1 s1 p2 s2 count) *)
  |> print_int
  |> print_newline
