#load "graphics.cma";;
#load "stack.cma";;
#load "unix.cma";;
open Graphics;;
open Stack;;
open Unix;;

(* Global steps counter *)
let c = ref 0;;
let init () = c := 0;;
let step () = incr c;;
let get () = !c;;
init ();;

(* Global discs & pegs parameters *)
let nb_discs = 12;;
let pegs = [|create (); create (); create ()|];;
let disc_colors = Array.make nb_discs (rgb 0 0 0);;
for i = 0 to (nb_discs - 1) do
  let r = Random.int 255
  and g = Random.int 255
  and b = Random.int 255 in
    disc_colors.(i) <- (rgb r g b)
done;;

(* Window parameters *)
let width = 8 * (15 + 10 * (nb_discs + 1)) + 80
and height = (nb_discs + 2) * 20 + 50;;

(* Animation setup *)
let animation_speed = 10.;;

(* Draw the three pegs *)
let draw_pegs () =
  let draw_single_peg () =
    rlineto 10 0;
    rlineto 0 (height - 50);
    rlineto (-20) 0;
    rlineto 0 (50 - height);
    rlineto 10 0 in
  let step = width / 4 in
    begin
      set_color (rgb 0 0 0);
      moveto step 0;
      draw_single_peg ();
      rmoveto step 0;
      draw_single_peg ();
      rmoveto step 0;
      draw_single_peg ();
    end;;

(* Initialize the scene *)
let init () =
  draw_pegs ();
  for i = (nb_discs - 1) downto 0 do
    push i pegs.(0)
  done;;

(* Update the graphics state *)
let update_window () =
  let rec translate_disc disc_poly x y =
    let f = fun (a,b) -> (a+x,b+y) in
      Array.map f disc_poly
  and draw_disc id =
    let poly = [|((15 + 10 * id),0);(15 + 10 * id,20);(-(15 + 10 * id),20);(-(15 + 10 * id),0)|] in
      fill_poly (translate_disc poly (current_x ()) (current_y ())) in
  let step = 20 in begin
    clear_graph ();
    draw_pegs ();
    let f = fun id -> rmoveto 0 (-step); set_color disc_colors.(id); draw_disc id in begin
      moveto (width/4) ((length pegs.(0)) * step);
      iter f pegs.(0);
      moveto (width/2) ((length pegs.(1)) * step);
      iter f pegs.(1);
      moveto (3*width/4) ((length pegs.(2)) * step);
      iter f pegs.(2)
    end;
  end;;

(* Print a movement and update the program state accordingly *)
let movement origin destination =
  print_string ("| move a disc from peg " ^ (string_of_int origin) ^ " to peg " ^ (string_of_int destination));
  print_newline ();
  push (pop pegs.(origin)) pegs.(destination);
  update_window ();
  sleepf (1. /. animation_speed);;

(* Recursively solve the Hanoi towers problem. height is the height of the tower to move from src to dst, and other is the middle rod *)
let rec hanoi height src other dst =
  if height = 1 then (movement src dst; step ())
  else begin
    hanoi (height - 1) src dst other;
    movement src dst;
    step ();
    hanoi (height - 1) other src dst;
  end;;

(* Initialize random number generator *)
Random.self_init;;

(* Close any possible open window *)
close_graph();;

(* Create a new window *)
open_graph (" " ^ (string_of_int width) ^ "x" ^ (string_of_int height) ^ "-0+0");;

init ();;
hanoi nb_discs 0 1 2;;
print_string ("Total number of moves: " ^ (string_of_int (get ())));;
