


(* Global step counter *)
class counter = object
val mutable count = 0
method step () = count <- count+1
method get () = count
end;;

let movement origin destination =
  print_string ("| move a disc from rod " ^ (string_of_int origin) ^ " to rod " ^ (string_of_int destination));
  print_newline ();;