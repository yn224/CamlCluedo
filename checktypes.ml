module type DiceSig = sig
  type outcome = int * int
  val roll : int -> outcome
  val dice_sum : outcome -> int
end

module DiceCheck : DiceSig = Dice

module type CardsSig = sig
  type card = 
  | Suspect of string  
  | Room of string   
  | Weapon of string 
  | Undef
  val full_deck : card list
  val case_file : card list
  val random_shuffle : 'a list -> 'a list
  val shuffled_deck : card list
  val play_cards : card list -> int -> card list
  val open_cards : card list -> card list -> card list -> card list
  val distribute : card list -> int -> card list list
end

module CardCheck : CardsSig = Cards

module type CommandSig = sig
  type object_phrase = string list
  type command = 
  | Roll
  | Go of object_phrase
  | Hand
  | History
  | Guess of object_phrase
  | Answer of object_phrase
  | Help
  | Quit
  exception Empty
  exception Malformed
  val parse : string -> command
end

module CommandCheck : CommandSig = Command

module type GraphSig = sig
  type id = int 
  type tile = {
    id : id;
    desc : string
  }
  val game_map : tile array array
  val graph_map : int array array
  val desc_of_id : id -> string
  val get_suggestions : id -> int -> (int * id) list
  val print_suggestions : (int * id) list -> unit
  val print_game_board : (int * id) list -> unit
end

module GraphCheck : GraphSig = Graph

module type PlayerSig = sig
  type marks = (Cards.card * bool) list
  type player_state = {
    location : Graph.tile;
    tracker : marks; 
    hand : Cards.card list;
  }
  val init_state : Cards.card list -> player_state
  val update_location : Graph.tile -> player_state -> player_state
  val update_tracker : marks -> Cards.card -> marks
  val update_marks : marks -> player_state -> player_state
  val update_hand : Cards.card list -> player_state -> player_state
end

module PlayerCheck : PlayerSig = Player

module type CpuSig = sig
  val move_ai_easy : string -> Player.player_state -> Player.player_state
  val move_ai_medium : string -> Player.player_state -> Player.player_state
  val move_ai_hard : string -> Player.player_state -> Player.player_state
  val single_player_loop : Graph.tile array array -> 
    (string -> Player.player_state -> Player.player_state) 
    -> Player.player_state -> Player.player_state -> Dice.outcome -> unit
end

module CpuCheck : CpuSig = Cpu

module type UserSig = sig
  val capitalize_entry : string list -> string list
  val lst_intersect : 'a list -> 'a list -> 'a list -> 'a list
  val get_hd : 'a list -> 'a
  val ids_of_user : (int * Player.player_state) array -> (int * Graph.id) list
  val board_disp : Dice.outcome -> (int * Player.player_state) array 
    -> Player.player_state -> unit
  val user_loop : Graph.tile array array -> int * Player.player_state 
    -> (int * Player.player_state) array -> Dice.outcome -> Cards.card list 
    -> unit
end

module UserCheck : UserSig = User

module type QueueSig = sig
  type 'a queue = 'a list
  val empty_q : 'a list
  val enqueue : 'a -> 'a list -> 'a list
end

module QueueCheck : QueueSig = Queue

module type AuthorSig = sig
  val hours_worked : int list
end

module AuthorCheck : AuthorSig = Author