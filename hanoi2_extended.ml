#load "graphics.cma";;
#load "unix.cma";;
open Graphics;;
open Unix;;

(*-------------Parameters and constants----------------*)

(* Global step counter *)
let counter = object
  val mutable count = 0
  method step () = count <- count+1
  method get () = count
  end;;

(* Global discs & pegs parameters *)
let nb_discs = 12;;
let pegs = [|Stack.create (); Stack.create (); Stack.create ()|];;
let disc_colors = Array.make nb_discs (rgb 0 0 0);;
for i = 0 to (nb_discs - 1) do
  let r = Random.int 255
  and g = Random.int 255
  and b = Random.int 255 in
    disc_colors.(i) <- rgb r g b
done;;

(* Window parameters *)
let width = 8 * (15 + 10 * (nb_discs + 1)) + 80
and height = (nb_discs + 2) * 20 + 50;;

(* Animation setup *)
let animation_speed = 10.;;

(*--------------Main functions---------------*)

(* Draw the three pegs *)
let draw_pegs () =
  let peg_width = 20
  and peg_height = height - 50 in
  let draw_single_peg () =
    draw_rect (current_x () - peg_width / 2) (current_y ()) peg_width peg_height in
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
    Stack.push i pegs.(0)
  done;;

(* Update the graphics state *)
let update_window () =
  let rec translate_disc disc_poly x y =
    let f = fun (a,b) -> (a+x,b+y) in
      Array.map f disc_poly
  and draw_disc id =
    let disc_height = 20
    and disc_base_width = 30 in
    let disc_width = disc_base_width + 20 * id in
    fill_rect (current_x () - disc_width / 2) (current_y ()) disc_width disc_height in
  let step = 20 in begin
    clear_graph ();
    draw_pegs ();
    let f = fun id -> rmoveto 0 (-step); set_color disc_colors.(id); draw_disc id in begin
      moveto (width/4) ((Stack.length pegs.(0)) * step);
      Stack.iter f pegs.(0);
      moveto (width/2) ((Stack.length pegs.(1)) * step);
      Stack.iter f pegs.(1);
      moveto (3*width/4) ((Stack.length pegs.(2)) * step);
      Stack.iter f pegs.(2)
    end;
  end;;

(* Print a movement and update the program state accordingly *)
let movement origin destination =
  print_string ("| move a disc from peg " ^ (string_of_int origin) ^ " to peg " ^ (string_of_int destination));
  print_newline ();
  Stack.push (Stack.pop pegs.(origin)) pegs.(destination);
  update_window ();
  sleepf (1. /. animation_speed);;

(* Recursively solve the Hanoi towers problem. height is the height of the tower to move from src to dst, and other is the middle rod *)
let rec hanoi height src other dst =
  if height = 1 then (movement src dst; counter#step ())
  else begin
    hanoi (height - 1) src dst other;
    movement src dst;
    counter#step ();
    hanoi (height - 1) other src dst;
  end;;

(*----------------Main program-----------------*)

(* Initialize random number generator *)
Random.self_init;;

(* Close any possible open window *)
close_graph();;

(* Create a new window *)
open_graph (" " ^ (string_of_int width) ^ "x" ^ (string_of_int height) ^ "-0+0");;

init ();;
(* Assign identifiers (e.g. 0 1 2) to the three pegs *)
hanoi nb_discs 0 1 2;;
print_string ("Total number of moves: " ^ (string_of_int (counter#get ())));;

(* Keep the window open until the next key press *)
ignore (Graphics.read_key ());;

