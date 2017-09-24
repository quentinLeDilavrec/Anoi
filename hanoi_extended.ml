
open Graphics;;

type disc = int;;
type pole = disc list;;


let width = 1900;;
let height = 1000;;


class counter = 
  object
    val mutable count=0
    method step = count <- count+1
    method get = count
  end;;

let rec make_tower h =
  if h==0 then [] 
  else h::(make_tower (h-1):pole);;

let move game_state ori dest =
  if ori >= Array.length game_state || dest >= Array.length game_state 
  then draw_string "<no pole here>"
  else
    begin clear_graph();
      match game_state.(ori) with
        []-> draw_rect 0 0 10 10; (*no disk*)
      |o1::_ when game_state.(dest)!=[] && o1> List.hd(game_state.(dest)) -> 
        draw_rect 100 100 110 110;
      |o1::o->
        begin
          Array.set game_state ori o;
          Array.set game_state dest (o1::game_state.(dest));
          Array.iter (fun x -> 
              begin 
                if x==[] then draw_string "|" 
                else List.iter (fun x-> x |> string_of_int |> draw_string) (List.rev x);(*printing current pole in graphique*)
              end) game_state;
        end;
    end;

exception My_Error;;

class virtual ['a] window height width =
  let _ =  
    open_graph (" "^(string_of_int width)^"x"^(string_of_int height)^"+0-0") in
  object(self)

    val virtual mutable state : 'a

    val height=height
    val width=width

    method draw_scene () = draw_string "nothing";

    method handler (x:Graphics.status) = ();

    method start (starting_state: 'a) =
      begin

        (loop_at_exit [Key_pressed] self#handler);
      end;


  end;;

let game = object(self)

  inherit ['a] window height width as super

  val mutable state : pole array = [||];
  val mutable last_tower:int = -1;

  method private draw_scene ()=
    let e = 10 in
    let dim_max = 150 in
    let x_base = 10 in
    let y_base = 10 in
    let rec aux p x=
      let rec draw_pole d x y =
        let draw_disc dim x y e =
          draw_rect (x-dim*50/2) y (dim*50) e in
        match d with
        | []    ->();
        | dim::m-> 
          begin 
            draw_disc dim x y e;
            draw_pole m x (y+e)
          end in
      match p with
      | [] -> ();
      | pole::pole_list -> 
        begin 
          draw_pole pole x y_base;
          aux pole_list (x+dim_max)
        end in
    aux (Array.to_list state) x_base

  method move = move state

  method private handler x =
    let btw n lower greeter =
      ((lower <= n) && (n <= greeter)) in  
    match x.key with
    | '\027'   -> raise Exit;
    | '\032'   -> last_tower <- -1;
    | a when btw (int_of_char a) 49 57 -> 
      let pole_nb = (int_of_char a)-49 in
      if last_tower == -1 then last_tower <- pole_nb
      else
        begin
          self#move last_tower pole_nb;
          last_tower <- pole_nb;
          draw_circle 500 500 10;
          self#draw_scene();
        end;
    | other_key-> draw_string ((Char.escaped other_key)^" ");

  method start game =
    begin
      game_state <- game;
      (loop_at_exit [Key_pressed] self#handler);
      ();
    end;

  method restart game =
    clear_graph(); 
    game_state <- game;
    ();

  method basic_board_maker disk_heigth_per_pole =
    let rec aux d_h_per_pole =
      match d_h_per_pole with
      |x::m -> (make_tower x)::(aux m)
      |_->[] in
    Array.of_list(aux disk_heigth_per_pole);
end;;

game#start (game#basic_board_maker [0;5;2;0;0;0;1]);;

draw_rect 10 50 51 51;;