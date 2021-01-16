open Yojson.Basic.Util
open Printer

(** [txt_u] prints the text using underlines. *)
let txt_u = ANSITerminal.print_string [ANSITerminal.Underlined]

(** [txt_hl_w] highlights the background to white when printing. *)
let txt_hl_w = ANSITerminal.print_string 
    [ANSITerminal.on_white; ANSITerminal.black]

(** [txt_hl_c] highlights the background to cyan when printing. *)
let txt_hl_c = color_txt ANSITerminal.on_cyan

(** [txt_hl_r] highlights the background to red when printing. *)
let txt_hl_r = color_txt ANSITerminal.on_red

type id = int

type tile = {
  id : id;
  desc : string
}

(** Type of each tile from our json file. *)
type desc_col = {
  desc : string;
  col : int
}

(** Type of each row from our json file. *)
type row_desc_col = {
  row : int;
  desc_cols : desc_col list
}

(** [desc_col_of_json json] is a tile of type [desc_col] that [json] represents.
    Requires: json is a valid JSON tile representation. *)
let desc_col_of_json json = {
  desc = json |> member "desc" |> to_string;
  col = json |> member "column" |> to_int;
}

(** [row_of_json json] is a row of type [row_desc_col] that [json] represents.
    Requires: json is a valid JSON row representation. *)
let row_of_json json = {
  row = json |> member "row" |> to_int;
  desc_cols = json |> member "tiles" |> to_list |> List.map desc_col_of_json;
}

(** [from_json] is the game board that "clue_map.json" represents, but is 
    consisted of tiles and rows in different types compared to [game_board]. *)
let from_json = 
  "clue_map.json" 
  |> Yojson.Basic.from_file 
  |> member "Board" 
  |> to_list 
  |> List.map row_of_json

let game_map =
  let num_cols = List.length (List.hd from_json).desc_cols in
  let rec arr_row acc r = function
    | [] -> acc
    | h :: t -> {id = r * num_cols + h.col; desc = h.desc} :: arr_row acc r t
  in let rec json_to_map acc = function
      | [] -> acc
      | h :: t -> Array.of_list (arr_row [] h.row h.desc_cols) :: 
                  json_to_map acc t
  in Array.of_list (json_to_map [] from_json)

(** [num_rows] is the number of rows in [game_map]. *)
let num_rows = Array.length game_map

(** [num_cols] is the number of columns in [game_map]. *)
let num_cols = Array.length game_map.(0)

(** [last_index_check i] is true if the [i]th index of an array of length [len]
    is the last index of the array. *)
let last_index_check i len = i + 1 == len

let desc_of_id (id : id) = game_map.(id / num_cols).(id mod num_cols).desc 

(** [total_tiles] is total number of tiles in our map. *)
let total_tiles = Array.length game_map * Array.length (Array.get game_map 0)

let graph_map = Array.make_matrix total_tiles total_tiles 0

(** [make_edges r c] fills in the link between two tiles [r] and [c] in our
    adjacency matrix of our map. *)
let make_edges id1 id2 =
  graph_map.(id1).(id2) <- 1;
  graph_map.(id2).(id1) <- 1

(** [cn_case2 id_iter t_id right] connects a pair of nodes/tiles with 
    the descriptions [t_id], [right] depending on how the tiles are 
    described, in which the tile of [right] lies on the right side on the 
    tile of [t_id]. [id_iter] is the id of the tile of [t_id], and this 
    case is only dealt on the last row of game_map. *)
let cn_case2 id_iter (t_id : tile) (right : tile) = 
  match t_id.desc, right.desc with
  | "Exit", _ | "Start", "Exit" | "Start", "Start" | "Start", "Floor" 
  | "Floor", "Exit" | "Floor", "Start" | "Floor", "Floor" -> 
    make_edges id_iter (id_iter + 1)
  | "Start", _ | _, "Start" | "Floor", _ | _, "Floor" -> 
    ()
  | _, _ -> 
    make_edges id_iter (id_iter + 1)

(** [cn_case3 id_iter t_id down] connects a pair of nodes/tiles with 
    the descriptions [t_id], [down], depending on how the tiles are 
    described, in which the tile of [down] lies on the right below the 
    tile of [t_id]. [id_iter] is the id of the tile of [t_id], and this 
    case is only dealt on the last tile of each row in game_map. *)
let cn_case3 id_iter (t_id : tile) (down : tile) =
  match t_id.desc, down.desc with
  | "Exit", _ | "Start", "Exit" | "Start", "Start" | "Start", "Floor" 
  | "Floor", "Exit" | "Floor", "Start" | "Floor", "Floor" -> 
    make_edges id_iter (id_iter + num_cols)
  | "Start", _ | _, "Start" | "Floor", _ | _, "Floor" -> 
    ()
  | _, _ -> 
    make_edges id_iter (id_iter + num_cols)

(** [cn_case4 id_iter t_id right down] connects a pair of 
    nodes/tiles with the descriptions [t_id], [right] and/or [t_id], 
    [down], in which tile of [right] lies on the right side on the 
    tile of [t_id] and the tile of [down] lies on the right below 
    [t_id]. [id_iter] is the id of the tile of [t_id], and this case is
    dealt if tile of [id_iter] is neither in the last row nor the last column
    of game_map. *)
let cn_case4 id (t_id : tile) (right : tile) (down : tile) =
  match t_id.desc, right.desc, down.desc with
  | "Exit", _, _ | "Start", "Exit", "Exit" | "Start", "Exit", "Start" 
  | "Start", "Exit", "Floor" | "Start", "Start", "Exit" 
  | "Start", "Start", "Start" | "Start", "Start", "Floor" 
  | "Start", "Floor", "Exit" | "Start", "Floor", "Start" 
  | "Start", "Floor", "Floor" | "Floor", "Exit", "Exit" 
  | "Floor", "Exit", "Start" | "Floor", "Exit", "Floor" 
  | "Floor", "Start", "Exit" | "Floor", "Start", "Start" 
  | "Floor", "Start", "Floor"| "Floor", "Floor", "Exit" 
  | "Floor", "Floor", "Start" | "Floor", "Floor", "Floor" -> 
    make_edges id (id + 1); make_edges id (id + num_cols)
  | "Start", "Exit", _ | "Start", "Start", _ | "Start", "Floor", _ 
  | "Floor", "Exit", _ | "Floor", "Start", _ | "Floor", "Floor", _ -> 
    make_edges id (id + 1)
  | "Start", _, "Exit" | "Start", _, "Start" | "Start", _, "Floor"
  | "Floor", _, "Exit" | "Floor", _, "Start" | "Floor", _, "Floor" -> 
    make_edges id (id + num_cols)
  | "Start", _, _ | "Floor", _, _ | _, "Start", "Start" 
  | _, "Start", "Floor" | _, "Floor", "Start" | _, "Floor", "Floor" -> ()
  | _, "Start", _ | _, "Floor", _ -> make_edges id (id + num_cols)
  | _, _, "Start" | _, _, "Floor" -> make_edges id (id + 1)
  | _, _, _ -> make_edges id (id + 1); make_edges id (id + num_cols)

let connect_nodes =
  for i = 0 to num_rows - 1 do
    for j = 0 to num_cols - 1 do
      let t_id = game_map.(i).(j) in
      let id_iter = i * num_cols + j in
      match last_index_check i num_rows, last_index_check j num_cols with
      | true, true -> 
        ();
      | true, false -> 
        cn_case2 id_iter t_id game_map.(i).(j + 1);
      | false, true -> 
        cn_case3 id_iter t_id game_map.(i + 1).(j);
      | false, false -> 
        let right = game_map.(i).(j + 1) in
        let down = game_map.(i + 1).(j) in
        cn_case4 id_iter t_id right down;
    done;
  done

(** [get_nodes x] is a list of tiles that the payer can move to with step size
    1 from his or her current location [x]. *)
let get_nodes x =
  let connections = graph_map.(x) in
  let nodes = ref [] in
  for i = 0 to total_tiles - 1 do
    if connections.(i) = 1 then nodes := i :: !nodes
    else nodes := !nodes
  done;
  !nodes

(** [get_again acc] is a list of all nodes in which the player can move to on
    a given tile. *)
let rec get_again acc = function
  | [] -> acc
  | n :: t -> get_again (get_nodes n @ acc) t

(** [remove_visted set] is a list of tiles that the player is yet to visit
    when finding tiles that he or she can move to. *)
let rec remove_visited set = function
  | [] -> set
  | n :: t -> begin
      let new_set =
        if not (List.mem n set) then set
        else List.filter (fun x -> x <> n) set
      in
      remove_visited new_set t
    end

(** [to_assoc_list lst] is an association-list represetnation of the input
    list [lst]. *)
let to_assoc_list lst =
  List.mapi (fun i x -> (i + 1, x)) lst

let get_suggestions x k =
  let visited = ref [] in
  let possible_nodes = ref [] in
  let dist = ref (k - 1) in
  let starting_set = get_nodes x in
  possible_nodes := starting_set;
  visited := x :: starting_set @ !visited;
  while !dist > 0 do
    let next_set = get_again [] !possible_nodes |> List.sort_uniq compare in
    let rm_visited = remove_visited next_set !visited in
    possible_nodes := rm_visited;
    dist := !dist - 1;
    visited := rm_visited @ !visited |> List.sort_uniq compare
  done;
  !possible_nodes 
  |> List.sort compare 
  |> to_assoc_list

(** [pt_case2 t_id right] prints [t_id] on the console if it's on the bottom 
    row of the game board but not on the last column. It will print
    differently depending on the pair [t_id] and [right], which [right] the tile
    on the right side on [t_id]. *)
let pt_case2 (t_id : tile) (right : tile) = 
  match t_id.desc, right.desc with
  | "Exit", _ -> txt_u "exit|"
  | "Start", "Start" -> txt_hl_c "     "
  | "Start", _ -> txt_hl_c "    "; print_string "|"
  | "Floor", _ | _, "Exit" |  _, "Start" | _, "Floor" -> txt_u "    |"
  | _, _ -> txt_u "     "

(** [pt_case3 t_id down] prints [t_id] on the console if it's on the rightmost
    column of the game board. It will print differently depending on the pair
    [t_id] and [down], which [down] is the tile right under [t_id]. *)
let pt_case3 (t_id : tile) (down : tile) =
  match t_id.desc, down.desc with
  | "Exit", _ -> 
    txt_u "exit|\n|"
  | "Start", _ -> 
    txt_hl_c "    "; print_string "|\n|"
  | "Floor", _ | _, "Exit" |  _, "Start" | _, "Floor" -> 
    txt_u "    "; print_string "|\n|"
  | _, _ -> 
    print_string "    |\n|"

(** [pt_case4 t_id right down] prints t_id as long as it's neither on the 
    bottom row or on the rightmost column of the game board. The print depends
    on [t_id], [right], and [down], which [right] is on the right of [t_id] and
    [down] is right under [t_id]. *)
let pt_case4 (t_id : tile) (right : tile) (down : tile) =
  match t_id.desc, right.desc, down.desc with
  | "Exit", _, _ -> txt_u "exit|"
  | "Start", "Start", _ -> txt_hl_c "     "
  | "Start", _,_ -> txt_hl_c "    "; print_string "|"
  | "Floor", _,_ | _, "Floor", "Floor" | _, "Floor", "Exit" 
  | _, "Exit", "Floor" | _, "Exit", "Exit" -> txt_u "    |"
  | _, "Floor", _ | _, "Exit", _ -> print_string "    |"
  | _, _, "Floor" | _, _, "Exit" -> txt_u "     "
  | _, _, _ -> print_string "     "

(** [print_tile] prints an individual tile [t_id] that is positioned at the
    [i]th row and [j]th column of [game_map] according to its description. *)
let print_tile (t_id : tile) i j = 
  match last_index_check i num_rows , last_index_check j num_cols with
  | true, true -> 
    txt_u "    "; print_string "|";
  | true, false -> 
    pt_case2 t_id game_map.(i).(j + 1);
  | false, true -> 
    pt_case3 t_id game_map.(i + 1).(j);
  | false, false -> 
    let right = game_map.(i).(j + 1) in
    let down = game_map.(i + 1).(j) in
    pt_case4 t_id right down

(** [pl_case2 t_id right print_elm] will use [print_elm] to print [t_id] 
    depending on [right], which is on the right of [t_id]. [pl_case2] is used 
    only if [t_id] is on the bottom row but not on the last column of the 
    game board. *)
let pl_case2 (t_id : tile) (right : tile) print_elm =
  match t_id.desc, right.desc with
  | "Start", "Start" -> 
    print_elm; txt_hl_c " "
  | "Exit", _ | "Start", _ | "Floor", _ | _, "Floor" | _, "Exit" -> 
    print_elm; print_string "|"
  | _, _ -> 
    print_elm; print_string " "

(** [pl_case4 t_id right down print_elm] will use [print_elm] to print [t_id] 
    depending on [right] and [down], which [right] is on the right side of 
    [t_id] and [down] is right under [t_id]. [pl_case4] is used only if [t_id]
    is neither on the bottom row nor the rightmost column of the game board. *)
let pl_case4 (t_id : tile) (right : tile) (down : tile) print_elm =
  match t_id.desc, right.desc, down.desc with
  | "Start", "Start", _ -> 
    print_elm; txt_hl_c " "
  | "Exit", _, _ | "Start", _, _ | "Floor", _,_ | _, "Floor", _ 
  | _, "Exit", _ -> 
    print_elm; print_string "|"
  | _, _, _ -> 
    print_elm; print_string " "

(** [print_loc t_id i j print_elm] prints an individaul tile [t_id] that belongs 
    to one of the suggestion list or one of the player's location list 
    positioned at the [i]th row and the [j]th column of [game_map]. *)
let print_loc (t_id : tile) i j print_elm = 
  match last_index_check i num_rows , last_index_check j num_cols with
  | true, true -> 
    print_elm; print_string "|";
  | true, false -> 
    pl_case2 t_id game_map.(i).(j + 1) print_elm;
  | false, true -> 
    print_elm; print_string "|\n|";
  | false, false -> 
    let right = game_map.(i).(j + 1) in
    let down = game_map.(i + 1).(j) in
    pl_case4 t_id right down print_elm

(** [print_loc_map lst print_elm elm_arr] prints [game_map], but with the 
    addition of either [lst] being a list of [id] to move to or a list of 
    [id] of eachplayer. [print_elm] prints indivudal strings from [elm_arr]
    where each element corresponds to an element in [lst] in order. *)
let print_loc_map lst (print_elm : string -> unit) elm_arr =
  print_string ("_" ^ repeat (num_cols - 1) "_____" ^ "\n|");
  let elm_index = ref 0 in
  for i = 0 to num_rows - 1 do
    for j = 0 to num_cols - 1 do
      let t_id = game_map.(i).(j) in
      let is_lst_mem = List.mem t_id.id lst in
      match is_lst_mem with
      | true -> print_loc t_id i j (print_elm elm_arr.(!elm_index)); 
        incr elm_index;
      | false -> print_tile t_id i j
    done;
  done

(** [pp_digit n] is the string of the integer [n] that is centered in a 
    4-character space. *)
let pp_digit n =
  if n < 10 then "  " ^ string_of_int n ^ " " else " " ^ string_of_int n ^ " "

(** [id_of_locs lst] is the list of tile ids of an association list [lst].  *)
let id_of_locs lst = lst |> List.split |> snd

(** [arr_of_locs lst] is the array of identifying numbers that correspodn to
    ids of tiles in [lst]. *)
let arr_of_locs lst =
  lst 
  |> List.split 
  |> fst 
  |> List.map pp_digit 
  |> Array.of_list 

let print_suggestions lst =
  print_loc_map (id_of_locs lst) txt_hl_w (arr_of_locs lst)

let print_game_board loc =
  print_loc_map (id_of_locs loc) txt_hl_r (arr_of_locs loc)