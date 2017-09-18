



let pole = Array.make 3 [];;

let rec make_tower h =
  if h==0 then [] else
    h::(make_tower (h-1));;

let rec read_list f l =
  match l with
    [] -> ()
  | a::b -> 
    begin
      f a;
      read_list f b;
    end;;

Array.set pole 0 (List.rev(make_tower 5));;

exception My_Error;;

let move ori dest =
  match pole.(ori) with
    []-> print_string ("there is no disk to take on the pole " ^ string_of_int ori ^ "\n")
  |o1::o-> if pole.(dest)!=[] && o1> List.hd(pole.(dest)) 
  then print_string ("you can't put a disk bigger than the one under " ^ string_of_int dest ^ "\n")
    else begin
      Array.set pole ori o;
      Array.set pole dest (o1::pole.(dest));
      print_string ("you've moved a disk from " ^ string_of_int ori ^ " to " ^ string_of_int dest ^ "\n");
      Array.iter (fun x -> 
      begin 
        if x==[] then print_string "|" 
        else read_list print_int (List.rev x);
        print_newline();
      end) pole;
          end;;

move 0 1;;
move 0 2;;
move 1 2;;
