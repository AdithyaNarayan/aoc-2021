let maybe_read_line () = try Some(read_line()) with End_of_file -> None ;;

let read_all () =
  let rec f acc =
    match maybe_read_line () with
    | Some (line) -> f (line :: acc)
    | None -> List.rev acc
  in
  f [] ;;

let str_to_char_list s = List.init (String.length s) (String.get s) ;;

let error_score = function
| ')' -> 3
| ']' -> 57
| '}' -> 1197
| '>' -> 25137
| _ -> 0 ;;

let closing_of = function
| '(' -> ')'
| '[' -> ']'
| '{' -> '}'
| '<' -> '>'
| _ -> '0' ;;
let process_line l = 
  let rec f acc = function
    | [] -> 0
    | h :: tl -> match h with
      | '(' | '[' | '{' | '<' -> f (h :: acc) tl
      | c when acc == [] -> 0
      | c when c == (closing_of (List.hd acc)) -> f (List.tl acc) tl
      | c -> error_score (c)
  in f [] l ;;
let main () =
  let input = List.map (str_to_char_list) (read_all ()) in
  let mapped = List.map (process_line) input in
  let ans = List.fold_left (+) 0 mapped
  in Printf.printf "%d\n" ans ;;

main ();;
