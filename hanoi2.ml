(* Global steps counter *)
let c = ref 0;;
let init () = c := 0;;
let step () = incr c;;
let get () = !c;;

init ();;

(* Print a movement *)
let movement origin destination =
  print_string ("| move a disc from rod " ^ (string_of_int origin) ^ " to rod " ^ (string_of_int destination));
  print_newline ();;

(* Recursively solve the Hanoi towers problem.
   height is the height of the tower to move from src to dst, and other is the middle rod *)
let rec hanoi height src other dst =
  if height = 1
  then (movement src dst; step ())
  else begin
    hanoi (height - 1) src dst other;
    movement src dst;
    step ();
    hanoi (height - 1) other src dst;
  end;;

hanoi 10 0 1 2;;
print_string ("Total number of moves: " ^ (string_of_int (get ())));;
