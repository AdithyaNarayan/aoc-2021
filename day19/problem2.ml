let maybe_read_line () = try Some(read_line()) with End_of_file -> None

let read_all () =
  let rec f acc =
    match maybe_read_line () with
    | Some (line) -> f (line :: acc)
    | None -> List.rev acc
  in
  f []

let split s l =
  let rec helper acc acc_list =
    function
    | [] -> acc_list @ [acc]
    | hd :: tl when hd = s -> helper [] (acc_list @ [acc]) tl
    | hd :: tl -> helper (acc @ [hd]) acc_list tl
  in
  helper [] [] l

let rec fold_until f p acc = 
  function
  | x :: xs when p acc -> acc
  | x :: xs -> fold_until f p (f acc x) xs
  | [] -> acc

let parse_input l =
  l
  |> split ""
  |> List.map @@ List.tl
  |> List.map (List.map (
    fun s ->
      String.split_on_char ',' s
      |> List.map (int_of_string)
      |> fun l -> (List.hd l, List.hd @@ List.tl l, List.hd @@ List.tl @@ List.tl l)
  ))

let orientations (x, y, z) = 
  [(1, 1, 1); (-1, 1, 1); (1, -1, 1); (1, 1, -1); (-1, -1, 1); (-1, 1, -1); (1, -1, -1); (-1, -1, -1)]
  |> List.map @@ fun (x1, y1, z1) -> (x * x1, y * y1, z * z1)

let rotations (x, y, z) =
  [(x, y, z); (x, z, y); (y, x, z); (y, z, x); (z, x, y); (z, y, x)]

let orient_and_rotate x =
  orientations x
  |> List.concat_map (rotations)

let rec transpose = function
   | [] 
   | [] :: _ -> []
   | rows    -> 
       List.map List.hd rows :: transpose (List.map List.tl rows)

let all_directions l =
  l
  |> List.map orient_and_rotate
  |> transpose

let move_scanner (x, y, z) =
  List.map (fun (x1, y1, z1) -> (x + x1, y + y1, z + z1))

let comp_point (x1, y1, z1) (x2, y2, z2) =
  if x1 = x2 then
    if y1 = y2 then
      z1 - z2
    else
      y1 - y2
  else
    x1 - x2

let fix_points fixed points =
  all_directions points
  |> fold_until (
    fun (acc_s, acc_p) points ->
      let possible_scanners = List.concat_map (
        fun (x, y, z) -> 
          fixed
          |> List.map (fun (x1, y1, z1) -> (x1 - x, y1 - y, z1 - z))
        ) points
      in
      possible_scanners
      |> fold_until (
        fun (acc_s, acc_p) scanner ->
          let moved = move_scanner scanner points in
          let count = List.filter (fun x -> List.mem x fixed) moved |> List.length
          in
          if count >= 12 then
            (scanner, moved)
          else 
            (acc_s, acc_p)
      ) (
        fun (acc_s, acc_p) -> List.length acc_p > 0
      ) (acc_s, acc_p)
  ) (
    fun (acc_s, acc_p) -> List.length acc_p > 0
  ) ((0, 0, 0), [])

let f l =
  let rec helper scanners fixed =
    function
    | [] -> scanners
    | hd :: tl ->
      let (s, fix) = fold_until (
        fun (acc_s, acc_p) l -> fix_points l hd
      ) (
        fun (acc_s, acc_p) -> List.length acc_p > 0
      ) ((0, 0, 0), []) fixed
      in
      if List.length fix = 0 then
        helper scanners fixed (tl @ [hd])
      else
        helper (s :: scanners) (fixed @ [fix]) tl in
  let scanners = helper [(0, 0, 0)] ((List.hd l) :: []) (List.tl l)
  in
  scanners
  |> List.concat_map (fun x -> List.map (fun y -> (x, y)) scanners)
  |> List.map (fun ((x1, y1, z1), (x2, y2, z2)) -> (abs (x1 - x2), abs (y2 - y1), abs (z2 - z1)))
  |> List.map (fun (x, y, z) -> x + y + z)
  |> List.fold_left (fun max x -> if x > max then x else max) 0

let _ =
  read_all ()
  |> parse_input
  |> f
  |> print_int
  |> print_newline
