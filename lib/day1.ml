type digit_opt = 
| None
| Some of string

let to_digit = function
| "one" -> "1"
| "two" -> "2"
| "three" -> "3"
| "four" -> "4"
| "five" -> "5"
| "six" -> "6"
| "seven" -> "7"
| "eight" -> "8"
| "nine" -> "9"
| _ -> raise (Failure "Not a digit");;

let extract_digit str i = 
  let digit = Str.regexp "[0-9]" in
  let digit_word = Str.regexp "one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine\\|ten" in
  if (Str.string_match digit str i) then Some (String.make 1 str.[i]) else
  if (Str.string_match digit_word str i) then Some (to_digit (Str.matched_string str)) 
  else None;;

let find_digit start stop str = 
  let rec aux i = 
    let crement = if i < stop then +1 else -1 in
    if (i != stop) then
      let digit = extract_digit str i in 
      match digit with
      | None -> aux (i+crement)
      | Some digit -> digit
    else
    raise Not_found
in aux start;;

let find_first_digit line = find_digit 0 (String.length line) line;;
let find_last_digit line = find_digit ((String.length line) - 1) (-1) line;;

let extract_value line =
  let digits = (find_first_digit line) ^ (find_last_digit line) in
  int_of_string digits;;

let calibrate file =
   let ic = open_in file in
   let rec aux sum =
    try
    let line = input_line ic in
      aux (sum + extract_value line)
    with _ -> sum
  in aux 0;;

let run = Common.print_int_endline (calibrate "input/day1");;