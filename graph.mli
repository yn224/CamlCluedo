(**
   Representation of the map at which the game is played at.

   This module incudes necessary abstraction of our game map.
*)

(** Type of tile's id when represented in map. *)
type id = int 

(** Type of each tile in our game. *)
type tile = {
  id : id;
  desc : string
}

(** [game_map] is a map at which the game is played under. *)
val game_map : tile array array

(** [graph_map] is an adjacency-matrix representation of our [game_map]. *)
val graph_map : int array array

(** [desc_of_id] is the description of a tile that corresponds with [id].
    Requires: [id] must be a valid index between 0 ..  *)
val desc_of_id : id -> string

(** [get_suggestions x k] is a list of tile lecations that the player can move
    to from their current location [x] to distance [k] away from it. *)
val get_suggestions : id -> int -> (int * id) list

(** [connect_nodes] is a function that creates a link to all the tiles
    necessary and records it to the adjacency matrix [graph_map], depending on 
    their descriptions, to allow players to move around the map. *)
val connect_nodes : unit

(** [print_suggestions lst] prints the game board with locations where the 
    current player can move to, given as [lst]. *)
val print_suggestions : (int * id) list -> unit

(** [print_game_board loc] prints the game board with all the locations of
    players [loc]. *)
val print_game_board : (int * id) list -> unit