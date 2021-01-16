open Cards
open Player
open Cpu
open Printer
open User
open Graph

(** [difficulty] is the difficulty level of the AI that the player is goint to
    play against in a single-player mode. *)
let difficulty = function
  | 1 -> move_ai_easy
  | 2 -> move_ai_medium
  | 3 -> move_ai_hard
  | _ -> failwith "Impossible"

(** [single_player_env game_map] is an environment initialization step of the
    single-player mode of the game with map [game_map]. *)
let rec single_player_env game_map =
  txt_g "Please enter the difficulty level of the AI: \
         1. Easy, 2. Medium, 3. Hard\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> begin
    print_endline "EOF ERROR. ABORTING..."; 
    Stdlib.exit 0
  end
  | opt -> start_with_option game_map opt

(** [start_with_option game_map opt] starts the single-player mode of the game
    with the difficulty level [opt] for the AI. *)
and start_with_option game_map opt =
  let select = int_of_string opt in
  if select < 1 || select > 3 then begin
    txt_r "Please select from one of the valid options.\n"; 
    single_player_env game_map
  end
  else begin
    let _ = Sys.command "clear" in
    let diff = difficulty select in
    let cards = 
      match distribute (play_cards shuffled_deck 5) 5 with
      | [] -> failwith "Impossible"
      | h :: _ -> h
    in
    single_player_loop game_map diff (init_state cards) (init_state []) (0, 0)
  end

(** [create_players n map lst] is an array of [n] number of players who are 
    going to play the game on map [map] using list of cards [lst]. *)
let create_players n lst scl =
  let player_array = Array.make n (0, (init_state scl)) in
  let hand_array = Array.of_list lst in
  for i = 0 to n - 1 do
    player_array.(i) <- (i, update_hand hand_array.(i) (snd player_array.(i)))
  done;
  player_array

(** [multi_player_env game_map n] is an environment initialization step of the
    multi-player mode of the game with map [game_map] and [n] number of 
    players. *)
let multi_player_env game_map n =
  let cards_to_use = play_cards shuffled_deck n in
  let cards_to_be_shared = open_cards [] shuffled_deck cards_to_use in
  let distributed_deck = distribute cards_to_use n in
  let player_array = create_players n distributed_deck cards_to_be_shared in
  user_loop game_map player_array.(0) player_array (0, 0) cards_to_be_shared

(** [init_env map] prompts for the number of players that are going to play the
    game with map [map] and initializes the game environment. *)
let rec init_env game_map =
  try
    txt_g "Please enter the number of players. (MIN: 2, MAX: 6) \
            (1 for single-player)\n";
    print_string "> ";
    match read_line () with
    | exception End_of_file ->
      print_endline "EOF ERROR. ABORTING..."; Stdlib.exit 0
    | n -> init_game_env game_map n
  with 
  | _ -> begin
    txt_y "No players were able to guess the correct answer.";
    txt_y "The correct answer was:\n"; 
    print_string "| "; 
    print_card_lst case_file
  end

(** [init_game_env game_map n] determines which game mode the user chose to
    play, denoted by [n]. *)
and init_game_env game_map n =
  let num_players = int_of_string n in
  if num_players = 1 then 
    single_player_env game_map
  else if num_players < 0 || num_players > 6 then begin
    txt_r "Please select a valid number of players\n";
    init_env game_map
  end
  else begin
    let _ = Sys.command "clear" in
    multi_player_env game_map num_players
  end

let main () =
  print_instr "help";
  let _ = Sys.command "clear" in
  txt_g "Please enter the name of the game file you want to load.\n";
  txt_y "Currently, it is defaulted to <clue_map.json>. \n";
  txt_y "We hope to make more versions of the map later.\n";
  print_string  "> Press Enter to continue...";
  match read_line () with
  | exception End_of_file -> 
    print_endline "EOF ERROR. ABORTING..."; Stdlib.exit 0
  | _ -> init_env game_map

(* Execute the game engine. *)
let () = main ()