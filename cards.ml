
type card = 
  | Suspect of string  
  | Room of string   
  | Weapon of string
  | Undef

(** Predefined cards for the game. Cannot be modified. *)
type cards = card array

(** [suspects_str] is a string list representation of suspect cards. *)
let suspects_str = ["Sarah"; "Green"; "Mustard"; 
                    "Plum"; "White"; "Peacock"]

let rooms_str = ["Kitchen"; "Utility"; "Laundry"; 
                 "Dining"; "Billiard"; "Library"; 
                 "Lounge"; "Hall"; "Attic"]

(** [weapons_str] is a string list represetnation of weapon cards. *)
let weapons_str = ["Bow"; "Dagger"; "Lead"; 
                   "Spear"; "Rope"; "Wrench"]

(** [suspects] is a card array representation of suspect cards. *)
let suspects = List.map (fun x -> Suspect x) suspects_str |> Array.of_list

(** [rooms] is a card array representation of rooms. *)
let rooms = List.map (fun x -> Room x) rooms_str |> Array.of_list

(** [weapons] is a card array representation of weapons. *)
let weapons = List.map (fun x -> Weapon x) weapons_str |> Array.of_list

let find_category name =
  if List.mem name suspects_str then Suspect name
  else if List.mem name rooms_str then Room name
  else if List.mem name weapons_str then Weapon name
  else Undef

let full_deck = 
  Array.concat [suspects; rooms; weapons] |> Array.to_list

(** [random_get cat] is a randomly chosen card from a card category [cat]. *)
let random_get cat =
  let n = Random.int (Array.length cat) in
  Array.get cat n

(** [remove cat card] is a list of cards after removing card [card] from a 
    card category [cat]. *)
let remove cards card =
  List.filter (fun x -> x <> card) (Array.to_list cards)

(** [suspect_answer] is a suspect card of the answer. *)
let suspect_answer = random_get suspects

(** [suspect_remaining] is a list of cards after removing the suspect card 
    that was chosen to be part of the answer. *)
let suspect_remaining = remove suspects suspect_answer

(** [room_answer] is a room card of the answer. *)
let room_answer = random_get rooms 

(** [room_remaining] is a list of cards after removing the room card that was
    chosen to be part of the answer. *)
let room_remaining = remove rooms room_answer

(** [weapon_answer] is a weapon card of the answer. *)
let weapon_answer = random_get weapons

(** [weapon_remaining] is a list of cards after removing the weapon card that 
    was chosen to be part of the answer. *)
let weapon_remaining = remove weapons weapon_answer

let case_file = [room_answer; suspect_answer; weapon_answer]

(** [all_cards] is a list of cards that are going to be distributed among 
    players. *)
let all_cards = 
  List.append weapon_remaining []
  |> List.append room_remaining
  |> List.append suspect_remaining

(** [cmp_pairs x y] is -1 if [x]'s second number is less than that of [y]'s,
    0 if equal, and 1 if [x]'s second number is greater than tat of [y]'s. *)
let cmp_pairs x y =
  Stdlib.compare (snd x) (snd y)

(** [sort_inc lst] is a list [lst] sorted in an increasing order with respect
    to each element's second field. *)
let sort_inc lst = 
  List.sort cmp_pairs lst

let random_shuffle lst =
  List.rev_map (fun x -> (x, Random.int 100)) lst
  |> sort_inc
  |> List.rev_map (fun (x, _) -> x)

let shuffled_deck = random_shuffle all_cards

let rec play_cards lst num_players =
  if List.length lst mod num_players = 0 then lst
  else begin
    match lst with
    | [] -> failwith "Impossible"
    | h :: t -> play_cards t num_players
  end

let rec open_cards acc lst1 lst2 =
  match lst1 with
  | [] -> acc
  | h :: t -> begin
    if List.mem h lst2 then open_cards acc t lst2
    else open_cards (h :: acc) t lst2
  end

(** Abstract type for representing players' hands for the game. *)
type hand = {
  card_set : card list;
  len : int
}

(** [hand_unit] is the base unit of players' hands. *)
let hand_unit = {
  card_set = [];
  len = 0
}

(** [cmp_len x y] is -1 if [x]'s length is less than that of [y]'s,
    0 if equal, and 1 if [x]'s length is greater than tat of [y]'s. *)
let cmp_len x y =
  Stdlib.compare x.len y.len 

(** [sort_hands lst] is a sorted list of players' hands in an increasing order
    based on the number of cards in each player's hand. *)
let sort_hands lst =
  List.sort cmp_len lst

(** [update_field h card] is an updated statistics of the player's hand [h]
    after allocating [card] to that specific player. *)
let update_field h card = 
  {card_set = card :: h.card_set; len = h.len + 1}

(** [update_hands lst card] is an updated list of players' hands based on the
    number of cards each player has. *)
let update_hand lst card =
  match lst with
  | [] -> lst
  | h :: t -> (update_field h card) :: t |> sort_hands

(** [divide lst] divides the input list of cards to [lst] of players evenly. *)
let rec divide lst = function
  | [] -> lst
  | h :: t -> divide (update_hand lst h) t

(** [init num_player acc] is a list of [num_player] players' hands. *)
let rec init num_player acc = 
  if num_player = 0 then acc
  else init (num_player - 1) (hand_unit :: acc) 

(** [extract_set acc] is a list only containing the information about the 
    distributed set of cards. *)
let rec extract_hands acc = function
  | [] -> acc
  | h :: t -> extract_hands (h.card_set :: acc) t

(** [distribute cards num_player] is a list of [num_player] players' hands 
    after distributing [cards] evenly among players. *)
let distribute cards num_player = 
  divide (init num_player []) cards
  |> extract_hands []
