let maybe_read_line () = try Some(read_line()) with End_of_file -> None ;;

let read_all () =
  let rec f acc =
    match maybe_read_line () with
    | Some (line) -> f (line :: acc)
    | None -> List.rev acc
  in
  f [] ;;

let str_to_char_list s = List.init (String.length s) (String.get s) ;;

let score_of = function
| ')' -> 1
| ']' -> 2
| '}' -> 3
| '>' -> 4
| _ -> 0 ;;

let closing_of = function
| '(' -> ')'
| '[' -> ']'
| '{' -> '}'
| '<' -> '>'
| _ -> '0' ;;

let process_line l = 
  let rec f acc = function
    | [] -> List.fold_left (fun x y -> 5 * x + score_of y) 0 (List.map closing_of acc)
    | h :: tl -> match h with
      | '(' | '[' | '{' | '<' -> f (h :: acc) tl
      | c when c == (closing_of (List.hd acc)) -> f (List.tl acc) tl
      | _ -> 0
  in f [] l ;;

let main () =
  let input = List.map (str_to_char_list) (read_all ()) in
  let scores = List.filter (fun x -> x > 0) (List.map process_line input) in
  let median = List.nth (List.sort (fun x y -> x - y) scores) ((List.length scores) / 2)
  in Printf.printf "%d\n" median ;;

main ();;
