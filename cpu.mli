(**
  Represetntation of single-player modes.

  This module includes support for single-player mode of our game, where
  players play against AIs.
*)

(** [move_ai_easy intro st_cpu] is a routine for easy-level difficulty AI,
    where it keeps track of cards it matched with the answer with 25% chance. *)
val move_ai_easy : string -> Player.player_state -> Player.player_state

(** [move_ai_medium intro st_cpu] is a routine for medium-level difficulty AI,
    where it keeps track of cards it matched with the answer with 50% chance. *)
val move_ai_medium : string -> Player.player_state -> Player.player_state

(** [move_ai_hard intro st_cpu] is a routine for hard-level difficulty AI, 
    where it keeps track of the cards it matched with the answer with 75% 
    chance. *)
val move_ai_hard : string -> Player.player_state -> Player.player_state

(** [single_player_loop m diff st st_cpu res] is a single-player game routine
    on the map [m] with different levels of AI difficulty routine [diff] and
    its state [st_cpu].*)
val single_player_loop : Graph.tile array array -> 
  (string -> Player.player_state -> Player.player_state) -> Player.player_state 
  -> Player.player_state -> Dice.outcome -> unit