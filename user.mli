(**
  Representation of the

  This module includes support for the general multi-player mode of the game.
*)

(** [capitalize_entry lst] is a case-sensitive translation of a parsed user 
    input [lst]. *)
val capitalize_entry : string list -> string list

(** [lst_intersect acc src target] is a random card in intersection between 
    two card lists [src] and [target] <TODO>. 
    intersection between src list and target list (src being the guess and target being the other players' hands)*)
val lst_intersect : 'a list -> 'a list -> 'a list -> 'a list

(** [get_hd] is the head element of the input list. *)
val get_hd : 'a list -> 'a

(** [ids_of_user st_arr] is a list of players' locations in the map. *)
val ids_of_user : (int * Player.player_state) array -> (int * Graph.id) list

(** [board_disp res st_arr cps] either displays the regular
    board with different players' locations or board with movable locations. *)
val board_disp : Dice.outcome -> (int * Player.player_state) array 
  -> Player.player_state -> unit

(** [user_loop m st st_arr res scl] is the main game loop for multi-player
    version of our game. The game would be played on map [m] with all players'
    states in an array [st_arr]. [res] denotes the dice outcome of the
    previous command and [scl] denotes the shared card list, which all players
    have information about in the beginning. *)
val user_loop : Graph.tile array array -> int * Player.player_state 
  -> (int * Player.player_state) array -> Dice.outcome -> Cards.card list -> unit