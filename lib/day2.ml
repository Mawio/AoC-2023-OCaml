type draw =
| Red of int
| Green of int
| Blue of int

type set = draw list;;

type game = {number: int; sets: set list};;

let calculate_sum games n_red n_green n_blue = 

  let is_draw_possible = function
  | Red x -> x <= n_red
  | Green x -> x <= n_green
  | Blue x -> x <= n_blue
  in

  let rec is_set_possible = function
  | [] -> true
  | h :: t -> if is_draw_possible h then is_set_possible t else false
  in

  let get_game_value game = 
    let n, sets = game in
    let rec aux = function
    | [] -> n
    | h :: t -> if is_set_possible h then aux t else 0 
  in aux sets
  in
  
  let rec evaluate_games sum = function
  | [] -> sum
  | h :: t -> evaluate_games (sum + get_game_value h) t

in evaluate_games 0 games
;;

let parse_file file = 

  let parse_draw draw =
    match (String.split_on_char ' ' draw) with
    | [value_str; colour] -> (
        let value = int_of_string value_str in
        match colour with
      | "red" -> Red value
      | "green" -> Green value
      | "blue" -> Blue value
      | _ -> raise (Invalid_argument "Found invalid colour")
    )
    | _ -> raise (Invalid_argument "Found invalid draw")
  in

  let parse_set set = 
    let draws = String.split_on_char ',' set in
    let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (parse_draw (String.trim h) :: acc) t
    in aux [] draws
  in

  let extract_sets str = 
    let sets = String.split_on_char ';' str in
    let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (parse_set (String.trim h) :: acc) t
    in aux [] sets
  in

  let extract_game_number str = 
    let number_regex = Str.regexp "[0-9]+" in
    let _ = Str.search_forward number_regex str 0 in
    int_of_string (Str.matched_string str)
  in

  let parse_line line = 
    match (String.split_on_char ':' line) with
    | [game_identifier; sets] -> (extract_game_number game_identifier, extract_sets sets)
    | _ -> raise (Invalid_argument "Found invalid game id")
  in

  let ic = open_in file in
  let rec parse_games acc = match input_line ic with
  | line -> parse_games (parse_line line :: acc)
  | exception End_of_file -> acc

in parse_games []
;;

let run = 
  let parsed_file = parse_file "input/day2" in
  let sum = calculate_sum parsed_file 12 13 14 in
  Common.print_int_endline sum
;;