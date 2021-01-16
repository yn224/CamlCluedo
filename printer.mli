(**
  Encapsulation of all the printing functions for our game.

  Pretty-prints the map and variety of text outputs.
 *)
open Cards

(** [color_txt c] pretty-prints the text with color [c]. *)
val color_txt : ANSITerminal.style -> string -> unit

(** [txt_c str] pretty-prints the string [str] using cyan color. *)
val txt_c : string -> unit

(** [txt_y str] pretty-prints the string [str] using yellow color. *)
val txt_y : string -> unit

(** [txt_m str] pretty-prints the string [str] using magenta color. *)
val txt_m : string -> unit

(** [txt_g str] pretty-prints the string [str] using green color. *)
val txt_g : string -> unit

(** [txt_r str] pretty-prints the string [str] using rec color. *)
val txt_r : string -> unit

(** [print_instr s] prints the help manual to users. *)
val print_instr : 'a -> unit

(** [unwrap_card_lst card_lst] is a name value list of a card list [card_lst] 
    represented as string list. *)
val unwrap_card_lst : Cards.card list -> string list

(** [print_card_lst lst] prints the list of cards [lst] by extracting out its
    name value. *)
val print_card_lst : Cards.card list -> unit

(** [repeat n str] prints the string [str] for [n] iterations. *)
val repeat : int -> string -> string

(** [print_game_sheet marks] prints the game sheet to the player with of the
    list of [marks] and mark whether it is suspicious or not given boolean. *)
val print_game_sheet : (string * bool) list -> unit

(** [print_hands r cards dice suspect] prints the list of cards [cards],
    list of dice [dice], and list of suspects [suspect] by each row [r] *)
val print_all : int -> Cards.card list  -> int * int -> string list -> unit

(** [print_hands r cards] prints the list of cards [cards] by each row [r] *)
val print_hands : int -> Cards.card list -> unit

(** [print_end_msg ()] prints the message that signals the termination of the
    game. This happens when one of the players was able to correctly guess 
    what the answer was. *)
val print_end_msg : unit -> unit
