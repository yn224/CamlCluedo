(**
   Parsing of player commands.
*)

(** The type [object_phrase] represents the object phrase that can be part of a 
    player command.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters. Thus, no element of the list should contain any leading,
    internal, or trailing spaces. The list is in the same order as the words 
    in the original player command.

    Required: An [object_phrase] is not permitted. *)
type object_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a noun/verb and possibly an object phrase. *)
type command = 
  | Roll
  | Go of object_phrase
  | Hand
  | History
  | Guess of object_phrase
  | Answer of object_phrase
  | Help
  | Quit

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command] in a case-insensitive
    manner and disregarding trailing spaces. *)
val parse : string -> command