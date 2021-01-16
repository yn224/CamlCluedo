open Cards 

let color_txt c = ANSITerminal.print_string [c]

let txt_c = color_txt ANSITerminal.cyan

let txt_y = color_txt ANSITerminal.yellow

let txt_m = color_txt ANSITerminal.magenta

let txt_g = color_txt ANSITerminal.green

let txt_r = color_txt ANSITerminal.red

let print_instr s =
  let _ = Sys.command "clear" in
  txt_c "\nWelcome to "; 
  txt_y "\"Camel's Cluedo\"";
  txt_c "! This game was based off of the \
         murder mystery board\ngame \"Cluedo Clue\" for three to six players \
         that was devised in 1943 by\nAnthony E. Pratt from Birmingham, \
         England. For more information, please look\nup Cluedo in Wikipedia.\
         \n\n";
  txt_y "General Overview: ";
  print_string "Someone died in an unknown room by someone.\n\
                Your objective is to identify the suspect, the weapon, and \
                the room\nof the murder by gathering information from other \
                players. ";
  print_endline "Move around different \nlocations in the map and make \
                 guesses along the way to deduce \"who\" murdered with \n\
                 \"what\" in \"where\"..\n";
  print_endline "List of possible commands (case-insensitive):";
  print_string "| ";
  txt_m "\"Roll\" ";
  print_endline "allows the player to roll a pair of dice for them to move.";
  print_string "| ";
  txt_m "\"Go <number>\" ";
  print_endline "allows the player to move to one of the suggested tiles \
                 labeled with <number>.";
  print_string "| ";
  txt_m "\"Hand\" ";
  print_endline "allows the player to look at his or her cards.";
  print_string "| ";
  txt_m "\"History\" ";
  print_endline "allows the player to look at his or her record tracker.";
  print_string "| ";
  txt_m "\"Guess\" ";
  print_endline "allows the player to ask other players to show the card.";
  txt_g "  | For single-player, the player can guess the answer\n";
  txt_g "    directly and get hints about the answer by checking if any matches.\n";
  print_string "| ";
  txt_m "\"Answer <answer>\" ";
  print_endline "allows the player to guess the solution with <answer>.";
  print_endline "  | Whenever the player moves to a room, he or she has an \
                 opportunity to directly answer.";
  print_string "  | The <answer> must be in the order of ";
  txt_y "Room";
  print_string ", ";
  txt_y "Suspect";
  print_string ", and ";
  txt_y "Weapon";
  print_endline ".";
  print_endline "  | If correct, the player wins the game.";
  print_endline "  | If incorrect, the player loses and is no longer going to \
                 be able to play the game.";
  txt_g "  | For single-player, the user does not get removed.\n";
  print_string "| ";
  txt_m "\"Help\" ";
  print_endline "displays this help manual.";
  print_string "| ";
  txt_m "\"Quit\" ";
  print_endline "ends the game.\n";
  txt_g "[ Developed by YoungSeok Na, Yubin Heo, Jason Park, Dongho Kim ] \n\n";
  print_endline "Enter any key to continue...";
  match read_line () with
  | exception End_of_file -> ()
  | _ -> ()

let print_game_sheet marks = 
  let _ = Sys.command "clear" in
  let line = "|____________________|__________________|\n" in
  let space = "|                    |                  |\n" in
  txt_c "                GAME SHEET\n";
  print_string " _______________________________________\n";
  let rec cards_sheet = function
    | [] -> print_string line;
    | h :: t -> print_string (line ^ space); 
      if snd h then 
        print_string ("|\t" ^ fst h ^ "\t     |\t\tO\t|\n")
      else 
        print_string ("|\t" ^ fst h ^ "\t     |\t\tX\t|\n");
      cards_sheet t 
  in 
  cards_sheet marks;
  print_endline "Enter any key to continue...";
  match read_line () with
  | exception End_of_file -> ()
  | _ -> ()

let unwrap_card_lst card_lst =
  List.map (fun c -> 
      match c with 
      | Suspect s | Room s | Weapon s -> s
      | Undef -> failwith "Impossible") 
    card_lst

let print_card_lst lst =
  let cl = unwrap_card_lst lst in
  let rec print_card_lst_aux = function
    | [] -> print_endline "\n";
    | h :: t -> begin
        print_string (h ^ " | ");
        print_card_lst_aux t
      end
  in
  print_card_lst_aux cl

let rec repeat n str = 
  if n = 0 then str
  else repeat (n - 1) str ^ str

(** [title n str] prints the title of each section of the UI, be it the dice
    outcome or card lists. *)
let title n str = 
  let t = repeat n "----------------" in
  let s = repeat n "            " in
  if n + 1 <= 1 then begin
    let t = "---------------  " in
    let s = "       " in
    ("\n\n" ^ str ^ s, t)
  end
  else if n + 1 = 3 then begin
    let s = repeat n "             " in
    ("\n\n" ^ str ^ s ^ "  ", t ^ "-  ")
  end
  else ("\n\n" ^ str ^ s, t ^ "--")

(** [space len str] is an empty space that occupies the specified length 
    [len] before the placement of string [str]. *)
let rec space len str =
  if len = 0 then str
  else space (len - 1) str ^ " "

(** [card_name name] prints the name of the card in appropriate location when
    displaying. *)
let card_name name = 
  let len = String.length name in
  let left = 13 - len in
  let half = left / 2 in
  let s1 = space half "" in
  let s2 = space (left - half) "" in
  s1 ^ name ^ s2

(** [print_cards r cards] displays the cards that the player has in his or her
    hand. *)
let print_cards r cards =
  let clst = unwrap_card_lst cards in
  let n = List.length cards - 1 in
  let start = " _____________   " in
  let closure = "|_____________|  " in
  let space = "|             |  " in
  if r = 0 then txt_g (fst (title n "MY CARDS"))
  else if r = 1 then txt_g (snd (title n "MY CARDS"))
  else if r = 2 then print_string (repeat n start)
  else if r = 7 then begin
    let rec print_spaces cards print = 
      match cards with
      | [] -> print_string print
      | h :: t -> 
        let p = ("|" ^ card_name h ^ "|  ") in
        print_spaces t (print ^ p)
    in 
    print_spaces clst ""
  end
  else if r = 11 then print_string (repeat n closure)
  else print_string (repeat n space)

(** [print_open_cards r cards] displays the cards that are shared across all
    players throughout the game. The shared cards are cards that are left over
    after evenly distributing all the cards to all players. *)
let print_open_cards r cards =
  let clst = unwrap_card_lst cards in
  let n = List.length cards - 1 in
  let start = " _____________   " in
  let closure = "|_____________|  " in
  let space = "|             |  " in
  if r = 0 then txt_g (fst (title n "OPEN CARDS"))
  else if r = 1 then txt_g (snd (title n "OPEN CARDS"))
  else if r = 2 then print_string (repeat n start)
  else if r = 7 then 
    if clst = [] then print_string ("None Available")
    else begin
      let rec print_spaces cards print = 
        match cards with
        | [] -> print_string print
        | h :: t -> begin
            let p = ("|" ^ card_name h ^ "|  ") in
            print_spaces t (print ^ p)
          end
      in 
      print_spaces clst ""
    end
  else if r = 11 then print_string (repeat n closure)
  else print_string (repeat n space)

(** [faces num] prints a face of one die with the outcome [num]. *)
let faces num =
  let empty = "|         |" in
  let mid =  "|    o    |" in
  let double = "|   o o   |" in
  let left = "|   o     |" in
  let right = "|     o   |" in
  if num = 1 then [empty; mid; empty]
  else if num = 2 then [left; empty; right]
  else if num = 3 then [left; mid; right]
  else if num = 4 then [double; empty; double]
  else if num = 5 then [double; mid; double]
  else if num = 6 then [double; double; double]
  else [empty; empty; empty]

(** [print_dots dice] prints the face-up value of the dice [dice] that is 
    rolled. *)
let print_dots dice = 
  let f a b = (a, b) in
  List.map2 f (faces (fst dice)) (faces (snd dice))

(** [print_dice r dice line] pretty-prints the outcome of the dice. *)
let print_dice r dice line =
  let start = " _________     _________" in
  let closure = "|_________|   |_________|" in
  let space = "|         |   |         |" in
  if r = 0 then txt_g (line ^ "DICE ROLL                ")
  else if r = 1 then txt_g ("-------------------------")
  else if r = 2 then print_string start
  else if r = 3  then print_string space
  else if 3 < r && r < 7 then begin
    let rec print_faces i lst = 
      match lst with
      | [] -> ()
      | h :: t -> begin
          if i = r - 4 then print_string ((fst h) ^ "   " ^ (snd h))
          else print_faces (i + 1) t
        end
    in 
    print_faces 0 (print_dots dice)
  end
  else if r = 7 then print_string closure
  else ()

(** [print_activity r act] prints the history of actions that the player has
    made alongside with the dice. *)
let print_activity r act =
  let len = List.length act in
  if r = 0 then txt_g "ACTIVITY                  "
  else if r = 1 then txt_g "------------------------"
  else if r = 2 then print_string " "
  else if r < len + 3 then begin
    let rec print_spaces i = function
      | [] -> ()
      | h :: t -> begin
          if h = "" then print_string " "
          else if i = r - 3 then begin
            print_string "* ";
            txt_g h;
          end
          else print_spaces (i + 1) t
        end
    in 
    print_spaces 0 act
  end
  else ()

let rec print_all r cards dice act = 
  if cards = [] then without_scl r dice cards act
  else if r < 12 then begin
    print_open_cards r cards;
    print_string "       ";
    print_dice r dice "";
    print_string "       ";
    print_activity r act;
    print_string "\n";
    print_all (r + 1) cards dice act
  end
  else print_string "\n"

(** [without_scl r dice cards act] prints the additional information when no
    sharable cards are available. *)
and without_scl r dice cards act =
  if r < 12 then begin
    print_dice r dice "\n\n";
    print_string "       ";
    print_activity r act;
    print_string "\n";
    print_all (r + 1) cards dice act
  end
  else print_string "\n"

let rec print_hands r cards = 
  if r < 12 then begin
    print_cards r cards;
    print_string "\n";
    print_hands (r + 1) cards
  end
  else if r = 12 then print_string "\n"

let print_end_msg () =
  print_string "The answer for this game was ";
  print_card_lst case_file;
  print_endline "Thank you for playing! \n"; 
  Stdlib.exit 0