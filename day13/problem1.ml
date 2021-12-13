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

let parse_input l = 
  let i = findi ((=) "") l in
  let points = 
    take i l
    |> List.map (fun x -> x |> String.split_on_char ',' |> List.map int_of_string)
    |> List.map (fun x -> (List.nth x 0, List.nth x 1)) in
  let lines = 
    drop (i + 1) l
    |> List.map (
      fun x -> 
        let line = int_of_string @@ String.sub x 13 (String.length x - 13) in
        if String.get x 11 = 'x' then (line, 0) else (0, line)
      )
  in (points, lines)

let fold_paper line points = 
  points
  |> List.map (
    fun (x, y) ->
      match line with
      | (l, 0) -> if (x < l) then (x, y) else (l - (x - l), y)
      | (0, l) -> if (y < l) then (x, y) else (x, l - (y - l))
      | _ -> (x, y)
  )
  |> List.sort_uniq (fun (x1, y1) (x2, y2) -> if x1 = x2 then y1 - y2 else x1 - x2)

let _ =
  read_all ()
  |> parse_input
  |> fun (points, line) -> fold_paper (List.hd line) points
  |> List.length
  |> print_int
  |> print_newline
