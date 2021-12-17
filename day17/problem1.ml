let maybe_read_line () = try Some(read_line()) with End_of_file -> None

let read_all () =
  let rec f acc =
    match maybe_read_line () with
    | Some (line) -> f (line :: acc)
    | None -> List.rev acc
  in
  f []

let str_to_char_list s = List.init (String.length s) (String.get s)

let char_list_to_str = List.fold_left (fun acc x -> acc ^ String.make 1 x) ""  

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

let split_at n l = (take n l, drop n l)

let parse_input l =
  List.hd l
  |> str_to_char_list
  |> drop 15
  |> fun x -> split_at (findi ((=) '.') x) x
  |> fun (num, rest) -> ((int_of_string @@ char_list_to_str num, 0), (0, 0), drop 2 rest)
  |> fun ((x1, _), (_, _), rest) ->
    let num, rest = split_at (findi ((=) ',') rest) rest in
    let num = int_of_string @@ char_list_to_str num
    in
    ((x1, 0), (num, 0), drop 4 rest)
  |> fun ((x1, _), (x2, _), rest) -> 
    let num, rest = split_at (findi ((=) '.') rest) rest in
    let y2 = int_of_string @@ char_list_to_str num in
    let y1 = int_of_string @@ char_list_to_str @@ drop 2 rest
    in
    ((x1, y1), (x2, y2))

let step (posx, posy) (velx, vely) =
  (posx + velx, posy + vely), ((if velx > 0 then velx - 1 else if velx < 0 then velx + 1 else 0), vely - 1)

let print (x1, y1) (x2, y2) = Printf.printf "(%d, %d), (%d, %d)" x1 y1 x2 y2; print_newline ()

let uncurry f (x, y) = f x y

let tee f x = f x; x

let step_until =
  let rec helper acc (posx, posy) (velx, vely) (x1, y1) (x2, y2) =
    let ((nextx, nexty), (nextvelx, nextvely)) = step (posx, posy) (velx, vely) in
    let nextacc = acc @ [(nextx, nexty)]
    in
    if x1 <= nextx && nextx <= x2 && y1 >= nexty && nexty >= y2 then
      nextacc
    else if (nextx <= x2 && nexty >= y2) then
      helper nextacc (nextx, nexty) (nextvelx, nextvely) (x1, y1) (x2, y2)
    else
      []
  in
  helper [] (0, 0)

let f (x1, y1) (x2, y2) =
  let find limit =
    let rec helper_y y =
      let rec helper_x x =
        if x > limit then -1 else
        match step_until (x, y) (x1, y1) (x2, y2) with
        | [] -> helper_x (x + 1)
        | _ -> x
      in
      if y < 0 then (-1, -1) else
      match helper_x 0 with
      | -1 -> helper_y (y - 1)
      | x -> (x, y)
    in
    helper_y limit
  in
  step_until (find 300) (x1, y1) (x2, y2)
  |> List.fold_left (fun acc (_, y) -> if y > acc then y else acc) 0

let _ =
  read_all ()
  |> parse_input
  |> uncurry @@ f
  |> print_int
  |> print_newline
