open Dice
open Command
open Cards
open Player
open Printer
open Graph
open Queue

let capitalize_entry lst =
  List.map (fun str -> 
      String.mapi (fun idx c -> 
          if idx = 0 then Char.uppercase_ascii c else c) str)
    lst

(** [find_idx a num idx] is the index [idx] of a value that has a key value of 
    [num] in association array [a]. *)
let rec find_idx a num idx =
  if fst a.(idx) = num then idx
  else find_idx a num (idx + 1)

let rec lst_intersect acc target = function
  | [] -> acc
  | h :: t -> begin
      if List.mem h target then lst_intersect (h :: acc) target t
      else lst_intersect acc target t
    end

let get_hd = function
  | [] -> failwith "Please file an issue"
  | h :: _ -> h

(** [match_res] is a type that indicates whether there exists a match between
    two cards or not. *)
type match_res =
  | Match of string
  | NoMatch

(** [return_random] is the random element of the first list in the nested list
    input. *)
let return_random = function
  | [] -> NoMatch
  | h :: _ -> Match (random_shuffle h |> get_hd)

let extract_hands st_arr idx comparator =
  for i = 0 to Array.length st_arr - 1 do
    if i = idx then comparator := !comparator
    else begin
      let other_player_state = snd st_arr.(i) in
      let hands = other_player_state.hand |> unwrap_card_lst in
      comparator := hands :: !comparator
    end
  done;
  List.rev !comparator

(** [find_guess st_arr guess idx len] is a single random card that matches the
    guess [guess] from the hands of other players' [st_arr], except for the
    current player number [idx].

    Returns Match of string, where string would be the name of the card
      NoMatch if there are none. *)
let find_guess st_arr guess idx =
  let comparator = ref [] in
  let hands = extract_hands st_arr idx comparator in
  List.map (fun target -> lst_intersect [] target guess) hands
  |> List.filter (fun x -> x <> [])
  |> return_random

(** [print_string_list lst] prints the input string list [lst] *)
let print_string_list lst =
  List.iter (fun x -> txt_c (x ^ " ")) lst

let ids_of_user st_arr =
  Array.map (fun (i, st) -> (i + 1, st.location.id)) st_arr |> Array.to_list

let board_disp res st_arr cps =
  if dice_sum res <> 0 then 
    print_suggestions (get_suggestions cps.location.id (dice_sum res))
  else print_game_board (ids_of_user st_arr)

(** [msg] is a queue that keeps track of the activities that happened
    throughout the game. *)
let msg = ref empty_q

(** [user_loop m st st_arr res scl] is the main game loop for the multiplayer
    mode with map [m], current player state [st], array of participating 
    players [st_arr], result of rolling dice [res], and shared cards [scl] that
    all participants have information of. *)
let rec user_loop m st st_arr res scl =
  let cps = snd st in
  let cpn = string_of_int (fst st + 1) in
  let idx = find_idx st_arr (fst st) 0 in
  let num_p = Array.length st_arr in
  board_disp res st_arr cps;
  print_all 0 scl res !msg;
  print_endline ("Player " ^ cpn ^ "'s turn");
  print_string "> ";
  match read_line () with
  | str -> parse_str str m st st_arr res scl cps cpn idx num_p

(** [parse_str str m st st_arr res scl cps cpn idx num_p] parses the command
    that a user inputs. *)
and parse_str str m st st_arr res scl cps cpn idx num_p =
  try 
    let _ = Sys.command "clear" in 
    match parse str with
    | Roll -> roll_logic res m st st_arr scl cps cpn
    | Go t -> go_logic t m st st_arr res scl cps cpn idx num_p
    | Hand -> hand_logic m st st_arr res scl cps cpn
    | History -> history_logic m st st_arr scl cps cpn
    | Guess t -> guess_logic t m st st_arr scl cps cpn idx num_p
    | Answer t -> answer_logic t m st st_arr res scl cpn idx num_p
    | Help -> begin
        print_instr "help"; 
        let _ = Sys.command "clear" in 
        user_loop m st st_arr res scl
      end 
    | Quit -> print_endline "Thank you for playing! \n"; Stdlib.exit 0
  with
  | s -> exn_parsed m st st_arr res scl s

and exn_parsed m st st_arr res scl = function
  | Empty -> begin
      print_endline "Type something in! When stuck, type 'help' to get assistance.\n"; 
      user_loop m st st_arr res scl
    end
  | Malformed -> begin
      print_endline "Sorry, the command could not be understood. Type 'help' to get assistance.\n";
      user_loop m st st_arr res scl
    end
  | _ -> failwith "Impossible"

(** [roll_logic res m st st_arr scl cps] is a user routine for Roll command. *)
and roll_logic res m st st_arr scl cps cpn =
  let summed = dice_sum res in
  if summed = 0 then user_loop m st st_arr (roll 6) scl
  else begin
    print_endline "You have already rolled in your turn.";
    print_endline ("You can move " ^ string_of_int summed ^ " tiles.\n");
    msg := enqueue ("Player " ^ cpn ^ " rolled " ^ (string_of_int summed)) !msg;
    user_loop m st st_arr res scl
  end

(** [go_logic t m st st_arr res scl cps idx num_p] is a user routine for Go 
    command at any point of the game. *)
and go_logic t m st st_arr res scl cps cpn idx num_p =
  if dice_sum res = 0 then begin
    print_endline "Roll the dice first before moving using \"Roll\" command.\n";
    user_loop m st st_arr res scl
  end
  else go_proper m st st_arr res scl cps cpn idx num_p t

(** [go_proper m st st_arr res scl cps idx num_p t] is a user routine for Go
    command once the dice roll has been executed. *)
and go_proper m st st_arr res scl cps cpn idx num_p t =
  try
    let num_opt = int_of_string (get_hd t) in
    let movable = get_suggestions cps.location.id (dice_sum res) in
    if not (List.mem_assoc num_opt movable) then begin
      print_endline "The number must be one of the suggested outputs";
      user_loop m st st_arr res scl
    end
    else begin
      msg := enqueue ("Player " ^ cpn ^ " moved") !msg;
      move_proper m st st_arr scl num_p num_opt idx movable
    end
  with
  | _ -> begin
      print_endline "The input must be an integer number that corresponds to \
                     one of the suggestions displayed.";
      user_loop m st st_arr res scl
    end

(** [move_proper m st st_arr scl num_p num_opt idx movable] is a function that
    will place the user into the tile of id [num_opt] once it's been chosen. *)
and move_proper m st st_arr scl num_p num_opt idx movable =
  let new_id = List.assoc num_opt movable in 
  let new_loc = { id = new_id; desc = desc_of_id new_id } in
  let updated_state = update_location new_loc (snd st) in
  st_arr.(idx) <- (fst st, updated_state);
  user_loop m st_arr.((idx + 1) mod num_p) st_arr (0, 0) scl

(** [hand_logic m st st_arr res scl cps] is a user routine for the Hand 
    command. *)
and hand_logic m st st_arr res scl cps cpn = 
  let _ = Sys.command "clear" in 
  msg := enqueue ("Player " ^ cpn ^ " checked the cards") !msg;
  print_endline "Cards in your hand:";
  print_hands 0 cps.hand;
  print_endline "Press Enter to continue...";
  match read_line () with
  | exception End_of_file -> ()
  | _ -> let _ = Sys.command "clear" in user_loop m st st_arr res scl

(** [history_logic m st st_arr scl cps] is a user routine for the History 
    command. *)
and history_logic m st st_arr scl cps cpn =
  print_game_sheet (List.map (fun (x, b) -> 
      match x with 
      | Suspect s | Room s | Weapon s -> (s, b) 
      | Undef -> failwith "Impossible") 
      cps.tracker);
  let _ = Sys.command "clear" in 
  msg := enqueue ("Player " ^ cpn ^ " checked the tracker") !msg;
  user_loop m st st_arr (0, 0) scl

(** [guess_logic t m st st_arr scl cps cpn idx num_p] is a user routine for the
    Guess command. *)
and guess_logic t m st st_arr scl cps cpn idx num_p = 
  let guess = capitalize_entry t in
  print_string "The player asked other players to see if they have ";
  print_string_list guess;
  print_endline "cards.";
  msg := enqueue ("Player " ^ cpn ^ " asked other players about the cards") !msg;
  match find_guess st_arr guess idx with
  | Match a -> guess_match m st st_arr scl cps idx num_p a
  | NoMatch -> print_endline "No other players had cards that you asked for.";
    user_loop m st st_arr (0, 0) scl

(** [guess_match m st st_arr scl cps idx num_p a] is a function that records a
    shown card of name [a] into the user's tracker if the user guessed the
    card correctly. *)
and guess_match m st st_arr scl cps idx num_p a =
  let new_card = 
    match find_category a with 
    | Undef -> failwith "Impossible"
    | c -> c 
  in
  let new_mark = update_tracker cps.tracker new_card in
  let new_st = update_marks new_mark cps in
  st_arr.(idx) <- (fst st, new_st);
  print_endline "There was a match! One of those matches will automatically get recorded.";
  user_loop m st_arr.((idx + 1) mod num_p) st_arr (0, 0) scl

(** [answer_logic t m st st_arr res scl cpn idx num_p] is a user routine for
    the Answer command. *)
and answer_logic t m st st_arr res scl cpn idx num_p =
  let ans = unwrap_card_lst case_file in
  let attempt = capitalize_entry t in
  msg := enqueue ("Player " ^ cpn ^ " attempted to answer") !msg;
  if List.length attempt <> 3 then begin
    print_endline "The answer takes in three inputs. Type \"Help\" if you need help.";
    user_loop m st st_arr res scl
  end
  else if ans = attempt then answer_correct cpn
  else answer_incorrect cpn m st st_arr res scl idx num_p

(** [answer_correct cpn] is a display message for when the player number [cpn]
    has answered correctly. *)
and answer_correct cpn =
  print_endline ("Player " ^ cpn ^ " guessed the answer correctly.");
  print_end_msg ()

(** [answer_incorrect cpn m st st_arr res scl idx num_p] is the protocol when
    player number [cpn] has failed to make a correct answer. That player gets
    removed from the game. *)
and answer_incorrect cpn m st st_arr res scl idx num_p =
  print_endline ("Player " ^ cpn ^ " guessed the answer incorrectly.");
  print_endline "You are no longer going to be able to participate in this game.\n";
  print_endline "Press Enter to continue...";
  match read_line () with
  | _ -> begin
      let new_st_arr = 
        List.remove_assoc (fst st) (Array.to_list st_arr) 
        |> Array.of_list 
      in
      let new_len = Array.length new_st_arr in
      if idx = num_p - 1 then
        user_loop m new_st_arr.(idx mod new_len) new_st_arr res scl
      else user_loop m new_st_arr.(idx) new_st_arr res scl
    end