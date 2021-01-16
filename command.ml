
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

(** [separate] is parsed string segments from the player's input for the game.
    Raises [Empty] if the input is empty and [Malformed] if the command is 
    unrecognizable. *)
let separate = function
  | [] -> raise Empty
  | ["roll"] -> Roll
  | ["help"] -> Help
  | ["quit"] -> Quit
  | ["hand"] -> Hand
  | ["history"] -> History
  | ["guess"] -> raise Malformed
  | ["answer"] -> raise Malformed
  | "go" :: t -> begin
    if List.length t <> 1 then raise Malformed
    else Go t
  end
  | "guess" :: t -> Guess t
  | "answer" :: t -> Answer t
  | _ -> raise Malformed

let parse str =
  String.lowercase_ascii str
  |> String.split_on_char ' '
  |> List.map String.trim
  |> List.filter (fun x -> x <> "")
  |> separate