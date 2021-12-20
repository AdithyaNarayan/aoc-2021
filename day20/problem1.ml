let maybe_read_line () = try Some(read_line()) with End_of_file -> None

let read_all () =
  let rec helper acc =
    match maybe_read_line () with
    | Some (line) -> helper (line :: acc)
    | None -> List.rev acc
  in
  helper []

let int_of = function | '.' -> 0 | _ -> 1

let str_to_char_list s = List.init (String.length s) (String.get s)

let bin_char_list_to_dec = 
  let rec helper acc = function
  | [] -> acc
  | hd :: tl -> helper (acc * 2 + int_of hd) tl
  in
  helper 0

let parse_input l =
  let algo, image = List.hd l, List.tl @@ List.tl l
  in
  (str_to_char_list algo, image |> List.map str_to_char_list, '.')


let neighbours (i, j) = 
  [(i - 1, j - 1); (i - 1, j); (i - 1, j + 1);
  (i, j - 1); (i, j); (i, j + 1);
  (i + 1, j - 1); (i + 1, j); (i + 1, j + 1)]

let get l def (i, j) =
  try List.nth (List.nth l i) j with _ -> def

let step (algo, image, rest) =
  let padded = 
    image
    |> fun l ->
      let len = (List.length @@ List.hd l) + 2 in
      let v_pad = (str_to_char_list @@ String.make len rest) in
      let padded_l =
        List.map (fun l -> (rest :: l) @ [rest]) l
      in
      (v_pad :: padded_l) @ [v_pad] 
  in
  padded
  |> List.mapi (fun i -> List.mapi @@ (fun j x -> (i, j, x)))
  |> List.map (List.map (fun (i, j, _) -> neighbours (i, j) |> List.map (get padded rest)))
  |> List.map (List.map bin_char_list_to_dec)
  |> List.map (List.map (List.nth algo))
  |> fun img -> (algo, img, List.nth algo @@ bin_char_list_to_dec @@ str_to_char_list @@ String.make 9 rest)

let f x =
  x
  |> step
  |> step
  |> fun (_, image, _) -> image
  |> List.fold_left (List.fold_left (fun acc x -> if x = '#' then acc + 1 else acc)) 0

let _ =
  read_all ()
  |> parse_input
  |> f
  |> print_int
  |> print_newline
