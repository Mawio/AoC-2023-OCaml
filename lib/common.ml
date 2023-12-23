let print_int_endline n =
  let _ = print_int n in
  print_newline ();;

let max a = function
| b when b > a -> b
| _ -> a