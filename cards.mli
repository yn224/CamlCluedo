(** 
   Representation of different cards for the game.

   This module represents all types of cards that are necessary for the game. 
   It tells the player the total number of possible moves he or she can make.
*)

(** Generic type of cards. *)
type card = 
  | Suspect of string  
  | Room of string   
  | Weapon of string 
  | Undef 

(** [rooms_str] is a string list representation of room cards. *)
val rooms_str : string list

(** [find_category s] is a card that corresponds to the name [s]. *)
val find_category : string -> card

(** [full_deck] is the complete list of cards in a Clues gameset. *)
val full_deck : card list

(** [case_file] is a list of cards that are going to be the answer for the 
    game. *)
val case_file : card list

(** [random_shuffle lst] is a randomly shuffled list [lst]. *)
val random_shuffle : 'a list -> 'a list

(** [shuffled_deck] is a list of cards after randomly shuffling the cards. *)
val shuffled_deck : card list

(** [play_cards lst num_players] is a list of cards that are going to be 
    distributed "evenly" among players. *)
val play_cards : card list -> int -> card list

(** [open_cards acc lst1 lst2] is a list of cards that will be open to 
    everyone if the length of [shuffled_deck] is not a multiple of number of
    players. *)
val open_cards : card list -> card list -> card list -> card list

(** [distribute cards num_player] is a list of card arrays after distributing 
    the sets of cards [cards] in the game to [num_player] number of players 
    playing the game.

    Requires: [cards] must not be empty. *)
val distribute : card list -> int -> card list list

