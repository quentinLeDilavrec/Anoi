type disc = int;;
type pole = disc list;;

let pole = Array.make 3 [];;



let counter = object
  val mutable count=0
  method step() = count <- count+1
  method set_to t = count <- t
  method get() = count
end;;

(* Print a move *)
let print_move origin destination =
  print_string ("--> move a disc from rod " ^ (string_of_int origin) ^ " to rod " ^ (string_of_int destination));
  print_newline ();;

let rec make_tower h =
  if h==0 then [] else
    h::(make_tower (h-1));;

let print_state() =
  Array.iter (fun x -> 
      begin 
        if x==[] then print_string "|" 
        else List.iter print_int (List.rev x);
        print_newline();
      end) pole;;


exception My_Error;;

let move ori dest =
  match pole.(ori) with
    []-> print_string ("there is no disk to take on the pole " ^ string_of_int ori ^ "\n")
  |o1::o-> if pole.(dest)!=[] && o1> List.hd(pole.(dest)) 
    then print_string ("you can't put a disk bigger than the one under " ^ string_of_int dest ^ "\n")
    else begin
      Array.set pole ori o;
      Array.set pole dest (o1::pole.(dest));
      print_move ori dest;
      print_state();
    end;;

(* Recursively solve the Hanoi towers problem.
   height is the height of the tower to move from src to dst, and other is the middle rod *)
let rec hanoi height src other dst =
    begin
      counter#step();
      if height = 1
      then move src dst
      else begin
        hanoi (height - 1) src dst other;
        move src dst;
        hanoi (height - 1) other src dst;
      end
    end;;

let solve_hanoi pole height src dst =
  begin
    Array.set pole src (List.rev(make_tower height));
    print_state();
    let rec findi f l i  =
      if l==[] then -1
      else if f i then i
      else findi f (List.tl l) (i+1) in
    let other = findi (fun x-> x!=src && x!=dst) (Array.to_list pole) 0 in
    hanoi height src other dst;
  end;;

solve_hanoi pole 9 0 2;;
print_string ("Total number of moves: " ^ (string_of_int (counter#get())));;
