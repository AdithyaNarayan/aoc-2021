let maybe_read_line () = try Some(read_line()) with End_of_file -> None

let read_all () =
  let rec helper acc =
    match maybe_read_line () with
    | Some (line) -> helper (line :: acc)
    | None -> List.rev acc
  in
  helper []

let parse_input =
  List.map (
    fun s ->
      String.split_on_char ':' s
      |> (fun l -> List.nth l (List.length l - 1))
      |> (fun l -> String.sub l 1 (String.length l - 1))
      |> int_of_string
      |> fun i -> (i, 0)
  )

let [@warning "-8"] play_turn (((curpos, curscore) :: others), dice) =
  let roll = dice + (dice mod 100 + 1) + (dice + 1 mod 100 + 1) in
  let pos = (curpos + roll - 1) mod 10 + 1 
  in
  others @ [(pos, curscore + pos)], ((dice + 2) mod 100 + 1)

let f l =
  let rec helper acc x =
    let players, _ = x in
    let (_, curscore) = List.hd players in
    let (_, otherscore) = List.hd @@ List.tl players
    in
    if otherscore >= 1000 then
      3 * acc * curscore
    else
      helper (acc + 1) (play_turn x)
  in
  helper 0 (l, 1)

let _ =
  read_all ()
  |> parse_input
  |> f
  |> print_int
  |> print_newline
