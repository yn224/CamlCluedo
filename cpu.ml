open Dice
open Cards
open Player
open Printer
open Command
open User
open Graph
open Queue

(** [log] is a queue that keeps track of the activities that happened
    throughout the game. *)
let log = ref empty_q

(** [prob_det p] is true if the random float number is less than the
    probability value [p] and false otherwise. *)
let prob_det p =
  let () = Random.self_init () in
  if Random.float 1.0 < p then true
  else false

(** [find_missing acc] is a list of cards that the record tracker currently
    has it marked as "not seen". *)
let rec find_missing acc = function
  | [] -> acc |> List.rev
  | (c, tf) :: t -> begin
      if not tf then find_missing (c :: acc) t
      else find_missing acc t
    end

(** [random_select lst] is a random card selected among the input card list
    [lst]. *)
let random_select lst =
  let idx = Random.int (List.length lst - 1) in
  List.mapi (fun i x -> if i = idx then x else Undef) lst
  |> List.filter (fun x -> x <> Undef)
  |> get_hd

(** [ai_guess tile_name cpu_loc new_tile st_cpu p] is the computer's logic for
    determining its next course of action based on probability value [p].*)
let ai_guess tile_name cpu_loc new_tile st_cpu p =
  if tile_name = cpu_loc then update_location new_tile st_cpu
  else begin
    print_endline "Computer attempts to guess the answer...";
    log := enqueue ("Computer attempted to answer") !log;
    let guess_and_track = prob_det p in
    let card_here = find_missing [] st_cpu.tracker |> random_shuffle in
    if List.length card_here = 3 then begin
      print_endline "AI figured out the answer!";
      print_string "The answer for this game was ";
      print_card_lst case_file;
      print_endline "Thank you for playing! \n"; 
      Stdlib.exit 0
    end
    else if guess_and_track then begin
      let mark = update_tracker st_cpu.tracker (random_select card_here) in
      print_endline "The computer decided to record the card it has seen";
      update_marks mark st_cpu |> update_location new_tile
    end
    else update_location new_tile st_cpu
  end

(** [ai_routine intro p st_cpu] is the default routine for AI's movement. It 
    probabilistically determines whether to update its record when it sees new
    card with [p]. *)
let ai_routine intro p st_cpu =
  print_endline intro;
  let res = dice_sum (roll 6) in
  let cpu_loc = st_cpu.location.desc in
  let movable = get_suggestions st_cpu.location.id res in
  let next_loc = get_hd (random_shuffle movable) in
  let tile_name = desc_of_id (snd next_loc) in
  print_endline ("Computer is at " ^ cpu_loc);
  print_endline ("Computer rolled a dice to move " ^ (string_of_int res) ^ " tiles.");
  log := enqueue ("Computer moved after rolling dice") !log;
  let new_tile = {id = snd next_loc; desc = tile_name} in
  if List.mem tile_name rooms_str then 
    ai_guess tile_name cpu_loc new_tile st_cpu p
  else update_location new_tile st_cpu

let move_ai_easy intro st_cpu = 
  ai_routine intro 0.25 st_cpu

let move_ai_medium intro st_cpu =
  ai_routine intro 0.50 st_cpu

let move_ai_hard intro st_cpu =
  ai_routine intro 0.75 st_cpu

(** [is_ai_turn] is true if and only if it is AI's turn to move. *)
let is_ai_turn = ref false

let rec single_player_loop m diff st st_cpu res =
  if !is_ai_turn then ai_loop m diff st st_cpu res
  else begin
    board_disp res [|(0, st); (1, st_cpu)|] st;
    print_all 0 [] res !log;
    print_endline "\nYour turn";
    print_string "> ";
    match read_line () with
    | str -> parse_str m diff st st_cpu res str
  end

(** [ai_loop m diff st st_cpu res] is the AI-part of the single-player game
    loop. *)
and ai_loop m diff st st_cpu res =
  let new_cpu_st = diff "Computer's turn" st_cpu in
  is_ai_turn := false;
  print_endline "Press Enter to continue...";
  match read_line () with
  | exception End_of_file -> ()
  | _ -> begin 
      let _ = Sys.command "clear" in 
      single_player_loop m diff st new_cpu_st res
    end

(** [parse_str m diff st st_cpu res str] is a parsing logic for the user input
    in a single-player loop of the game. *)
and parse_str m diff st st_cpu res str =
  try
    let _ = Sys.command "clear" in 
    match parse str with
    | Roll -> roll_command m diff st st_cpu res
    | Go t -> go_command m diff st st_cpu res t
    | Hand -> hand_command m diff st st_cpu res
    | History -> history_command m diff st st_cpu res
    | Guess t -> guess_command m diff st st_cpu res t
    | Answer t -> answer_command m diff st st_cpu res t
    | Help -> begin
      print_instr "help";
      let _ = Sys.command "clear" in 
      single_player_loop m diff st st_cpu res
    end 
    | Quit -> print_endline "Thank you for playing! \n"; Stdlib.exit 0
  with
  | s -> exn_parsed m diff st st_cpu res s

(** [exn_parsed m diff st st_cpu res] is part of parsing the player command
    where the parsed result ended up to raise exceptions. *)
and exn_parsed m diff st st_cpu res = function
  | Empty -> begin
      print_endline "Type something in! When stuck, type 'help' to get assistance.\n"; 
      single_player_loop m diff st st_cpu res
    end
  | Malformed -> begin
      print_endline "Sorry, the command could not be understood. Type 'help' to get assistance.\n";
      single_player_loop m diff st st_cpu res
    end
  | _ -> failwith "Impossible"

(** [roll_command m diff st st_cpu res] is a roll command logic for a
    single-player loop of the game. *)
and roll_command m diff st st_cpu res =
  let summed = dice_sum res in
  if summed = 0 then begin
    log := enqueue ("You have rolled a dice") !log;
    single_player_loop m diff st st_cpu (roll 6)
  end
  else begin
    print_endline "You have already rolled in your turn.";
    print_endline ("You can move " ^ string_of_int summed ^ " tiles.\n");
    single_player_loop m diff st st_cpu res
  end

(** [go_command m diff st st_cpu res t] is a go command logic for a 
    single-player loop of the game. *)
and go_command m diff st st_cpu res t =
  is_ai_turn := true;
  if dice_sum res = 0 then begin
    print_endline "Roll the dice first before moving using \"Roll\" command.\n";
    single_player_loop m diff st st_cpu res
  end
  else proper_movement m diff st st_cpu res t

(** [proper_movement m diff st st_cpu res t] is part of the [go] command for a
    single-player loop of the game where the movement is determined to be at
    least proper. *)
and proper_movement m diff st st_cpu res t =
  try
    let num_opt = int_of_string (get_hd t) in
    let movable = get_suggestions st.location.id (dice_sum res) in
    if not (List.mem_assoc num_opt movable) then begin
      print_endline "The number must be one of the suggested outputs";
      single_player_loop m diff st st_cpu res
    end
    else begin
      log := enqueue ("You moved to a different location") !log;
      set_to_move m diff st st_cpu num_opt movable
    end
  with
  | _ -> begin
    print_endline "The input must be an integer number that corresponds \
                    to the suggestions"; 
    single_player_loop m diff st st_cpu res
  end

(** [set_to_move m diff st st_cpu num_opt movable] is part of the [go] logic
    for a single-player loop of the game that is specific to logic where the
    player is set to move. *)
and set_to_move m diff st st_cpu num_opt movable =
  let new_id = List.assoc num_opt movable in
  let new_loc = { id = new_id; desc = desc_of_id new_id } in
  let updated_state = update_location new_loc st in
  is_ai_turn := true;
  single_player_loop m diff updated_state st_cpu (0, 0)

(** [hand_command m diff st st_cpu res] is a hands command logic for a
    single-player loop of the game. *)
and hand_command m diff st st_cpu res =
  let _ = Sys.command "clear" in 
  log := enqueue ("You checked the cards") !log;
  print_endline "Cards in your hand:";
  print_hands 0 st.hand;
  print_endline "Press Enter to continue...";
  match read_line () with
  | exception End_of_file -> ()
  | _ -> let _ = Sys.command "clear" in single_player_loop m diff st st_cpu res

(** [history_command m diff st st_cpu res] is a history command logic for a
    single-player loop of the game. *)
and history_command m diff st st_cpu res =
  print_game_sheet (List.map (fun (x, b) -> 
      match x with 
      | Suspect s | Room s | Weapon s -> (s, b)
      | Undef -> failwith "Impossible") 
      st.tracker);
  log := enqueue ("You checked your tracker") !log;
  let _ = Sys.command "clear" in 
  single_player_loop m diff st st_cpu res

(** [guess_command m diff st st_cpu res t] is a guess command logic for a
    single-player loop of the game. *)
and guess_command m diff st st_cpu res t =
  is_ai_turn := true;
  log := enqueue ("You have attempted to guess what the answer is") !log;
  let guess = capitalize_entry t in
  let matches = lst_intersect [] (unwrap_card_lst case_file) guess in
  if List.length matches = 0 then begin
    print_endline "There were no matches...";
    single_player_loop m diff st st_cpu res
  end
  else begin
    print_endline "There was a match! The result will get automatically updated";
    single_player_loop m diff st st_cpu res
  end

(** [answer_command m diff st st_cpu res t] is an answer command logic for a
    single-player loop of the game. *)
and answer_command m diff st st_cpu res t =
  let ans = unwrap_card_lst case_file in
  let attempt = capitalize_entry t in
  if List.length attempt <> 3 then begin
    print_endline "The answer takes in three inputs. Type \"Help\" if you need help.";
    single_player_loop m diff st st_cpu res
  end
  else if ans = attempt then begin
    print_endline "You guessed the answer correctly!";
    print_end_msg ();
  end
  else begin
    is_ai_turn := true;
    print_endline "Your answer attempt was incorrect!";
    log := enqueue ("You have attempted to answer") !log;
    single_player_loop m diff st st_cpu res
  end