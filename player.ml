open Map
open Cards
open Graph

type marks = (card * bool) list

type player_state = {
  location : tile;
  tracker : marks; 
  hand : card list;
}

let new_tracker = 
  let rec f_lst acc = function
    | [] -> acc
    | h :: t -> false :: f_lst acc t
  in List.combine Cards.full_deck (f_lst [] Cards.full_deck)

let update_tracker (tracker : marks) (card : card) : marks =
  (card, true) :: (List.remove_assoc card tracker)

(** [init_tile] is the tile that all players start at from the beginning. *)
let init_tile = { id = 275; desc = "start" }

let init_state cards = 
  let rec init_update_tracker tracker = function
    | [] -> tracker
    | h :: t -> init_update_tracker (update_tracker tracker h) t
  in 
  {
    location = init_tile;
    tracker = init_update_tracker new_tracker cards;
    hand = cards;
  }

let update_location tile state =
  { state with location = tile }

let update_marks marks state =
  { state with tracker = marks }

let update_hand cards state =
  { state with hand = cards }
