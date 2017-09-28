#load "graphics.cma";;
#load "unix.cma";;
open Graphics;;
open Unix;;


(* Global discs & pegs parameters *)
let nb_discs = 12;;

let rec stack_of_list s l =
  match l with
  | []   -> s
  | e::l -> 
    begin 
    Stack.push e s;
    stack_of_list s l;
  end;;

let red = rgb 190 10 0
let bleue = rgb 20 20 200
let pegs = [|
stack_of_list (Stack.create ())
([]);
stack_of_list (Stack.create ())
([]);
stack_of_list (Stack.create ())
(11::10::9::8::7::6::5::4::3::2::1::0::[]);|];;

let disc_colors = [|red;red;red;red;red;red;red;red;red;red;red;bleue|];;

(* Window parameters *)
let width = 7 * (15 + 10 * (nb_discs + 1))
and height = (nb_discs + 2) * 20 + 50;;



(* Draw the three pegs *)
let draw_pegs () =
  let peg_width = 30
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




(* Update the graphics state *)
let update_window () =
  let draw_disc id =
    let disc_height = 20
    and disc_base_width = 35 in
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

(*----------------Main program-----------------*)


(* Close any possible open window *)
close_graph();;

(* Create a new window *)
open_graph (" " ^ (string_of_int width) ^ "x" ^ (string_of_int height) ^ "-0+0");;

draw_pegs ();;
update_window();;
(* Keep the window open until the next key press *)
ignore (Graphics.read_key ());;