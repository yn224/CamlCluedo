(** 
   Representation of each player in this game.

   This module represents the player in this game. It contains the current 
   location at which the player is located at, a record tracker that keeps 
   track of the information that this player was exposed to, and set of cards
   that represents the cards each player has in his or her hand.
*)

(** The type of record tracker that each player has. The first element denotes
    the card and the second boolean describes whether the player has seen or 
    not seen that card. *)
type marks = (Cards.card * bool) list

(** The type for each player's state. *)
type player_state = {
  location : Graph.tile;
  tracker : marks; 
  hand : Cards.card list;
}

(** [new_tracker] is an empty tracker with all entries initialized to list of
    mapping (card, false). *)
val new_tracker : marks

(** [update_tracker marks card] is an updated [marks] given a [card]. If the 
    [bool] binded to [card] is [false] in [marks], then [bool] will be 
    [true]. *)
val update_tracker : marks -> Cards.card -> marks

(** [init_state map cards] is the initial state of the player when playing on
    the map [map] with cards [cards]. In that state the player is located in 
    the starting room. *)
val init_state : Cards.card list -> player_state

(** [update_location tile] is the new player_state with his or her location 
    changed to [tile]. *)
val update_location : Graph.tile -> player_state -> player_state

(** [update_marks mark state] is an updated state with new [mark] that is to
    be used as the new tracker of the player's state [state]. *)
val update_marks : marks -> player_state -> player_state

(** [update_hand cards state] is an updated [state] with new list of cards
    [cards] that is going to update the hands of the player. *)
val update_hand : Cards.card list -> player_state -> player_state
