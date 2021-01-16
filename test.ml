(** 
  [Testing Plan]

  To ensure the correctness of our system, we have thoroughly tested our design
  using different testing paradigms we have learned throughout this course,
  including, but not limited to test-driven development, black-box testing, 
  and playtesting.
  To construct our test case, we have adopted test-driven development procedure
  as our overall test file construction.
  (1) Write down test case drafts for functions that we were sure would be
  necessary during the initial draft of modules. Doing so gave us an idea 
  of what level of detail each function should cover and what the expected
  behavior is for each function.
  (2) Implement the function body so that would show the desired behavior 
  planned out in step (1).
  (3) With the function signature created above, update the test case draft so
  that it matches the argument signatures (black-box testing) and run the 
  test suite.
  (4) For those that fail, we think about why it failed and modify the function
  implementation as needed. Otherwise, we “play-test” our game with actual 
  user inputs to see that our functions indeed reflects the proper
  behavior.
  (5) Repeat above procedures multiple times for each game components that 
  comes up to our mind as we move along.

  Due to the nature of our project (game), the OUnit tests and playtesting
  were used in equal amounts to verify our design. We did this because not 
  only did we have to make sure that the functions behave correctly on the 
  static environments where the expected outputs are set but also under dynamic
  environments where user inputs can vary the game progressions with their 
  inputs. For functions that attempted to visualize the outcomes of user input
  (including printing maps, cards that the player has, the record tracker that
  the player has, dice outcomes, and the possible movable tile options), we
  relied heavily on playtesting since printing could not be tested based on
  simple assertion testing.

  The test-driven development (as described above) enabled 
  us to incrementally validate our implementation as we built our system. They
  have at least provided us with what functionalities we must support in our 
  functions. Further experimenting with playtesting, of course, introduced more
  ways in which we could improve upon our game design. We tried our best to 
  test all of the major functions that were exposed through the module 
  interface. Even for those that rely heavily on user inputs, we have tried our
  best to deduce what the response that our program would receive when that
  user input is received and define appropriate variables to be used in
  testing.

  There were some random components to our system, especially the ones that 
  involve AI's, that could not be tested thoroughly since the behabior was
  completely random and we were unable to catch all the possible behaviors that
  the function would possess. Also, there were few constants (such as cards) 
  that were exposed in our interface but since they were merely a constant, we
  instead used them to make our test cases be more efficient.

  Although we have made use of all the testing frameworks we can use, we have
  not used much of bisect to check the code coverage. Again, with some of our
  functions' high dependence on printing, using bisect did not provide us with
  a strongly convincing metric that we are indeed covering most of our system's
  behavior (or rather much less than those provided by black-box testing and
  playtesting). However, as explained above, we have employed various testing 
  techniques we have learned in this course including, but not limited to, 
  test-driven development, OUnit test suite, and playtesting. We also made sure
  that each time we added a new function or a module, the changes we have made
  did not affect the behaviors of already-implemented functions. 
  Observing the extensivity of our testing strategy and number of trials we
  have been through for playtesting to verify the results, we believe that our
  test suite demonstrates the correctness of our system pretty well.
*)

open OUnit2
open Dice
open Cards
open Command
open Player
open Graph
open User
open Queue

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

module DiceTester = struct
  (** [test_tuple sides outcome] is true if the outcome of rolling two dice 
      [outcome], each with number of sides [sides], results in a number that is
      inside the possible range of numbers determined by the number of sides for
      each die. Otherwise, false *)
  let test_tuple sides outcome = 
    fst(outcome) >= 1 && fst(outcome) <= sides 
    && snd(outcome) >= 1 && snd(outcome) <= sides

  (** [roll_test name sides expected_output] constructs an OUnit
      test named [name] that asserts the quality of [expected_output]
      with [roll sides]. *)
  let roll_test name sides expected_output = 
    name >:: (fun _ -> 
        assert_equal expected_output (roll sides |> test_tuple sides)
          ~printer:string_of_bool)

  (** [test_sum sides result] is true if the sum of the outcome of rolling two 
      dice results in a number [result] that is inside the possible range of 
      outcomes of rolling two dice with number of sides [sides]. 
      Otherwise, false. *)
  let test_sum sides result =
    result >= 2 && result <= (2 * sides)

  (** [dice_sum_test name result expected_output] constructs an OUnit
      test named [name] that asserts the quality of [expected_output]
      with [dice_sum result]. *)
  let dice_sum_test name sides expected_output = 
    name >:: (fun _ -> 
        assert_equal expected_output (dice_sum (roll sides) |> test_sum sides) 
          ~printer:string_of_bool)

  (** [test] represents the test suite to be run for the [DiceTester]. *)
  let test = [
    roll_test "roll two dice with 4 sides" 4 true;
    roll_test "roll two dice with 6 sides" 6 true;
    roll_test "roll two dice with 12 sides" 12 true;
    dice_sum_test "find sum of two 4-sided dice" 4 true;
    dice_sum_test "find sum of two 6-sided dice" 6 true;
    dice_sum_test "find sum of two 12-sided dice" 12 true;
  ]
end

module CardTester = struct
  (** [random_shuffle_test name lst expected_output] constructs an OUnit test 
      named [name] that checks whether the input list [lst] is shuffled
      randomly. *)
  let random_shuffle_test name lst expected_output =
    name >:: (fun _ ->
        assert_equal expected_output (random_shuffle lst |> List.sort compare) 
          ~printer:(pp_list string_of_int))

  (** [play_cards_test cards num_players expected_output] constructs an OUnit 
      test named [name] that asserts the quality of [expected_output] with 
      [play_cards cards num_players]. *)
  let play_cards_test name cards num_players expected_output =
    name >:: (fun _ -> 
        assert_equal expected_output 
          (play_cards cards num_players |> List.length) ~printer:string_of_int)

  (** [open_cards_test name player_cards expected_output] constructs an OUnit 
      test named [name] that asserts the quality of [expected_output] with 
      [open_cards [] shuffled_deck play_cards]. *)
  let open_cards_test name player_cards expected_output =
    name >:: (fun _ ->
        assert_equal expected_output 
          (open_cards [] shuffled_deck player_cards |> List.length) 
          ~printer:string_of_int)

  (** [count_cards acc l] is the total number of cards distributed among players
      to ensure a property-based testing of function distribute. *)
  let rec count_cards acc l = function
    | [] -> acc
    | h :: t -> begin
        let summed =
          if l = List.length h then List.length h + acc
          else 0
        in
        count_cards summed l t
      end

  (** [distribute_test name cards cards_per_player expected_output] constructs an 
      OUnit test named [name] that asserts the quality of [expected_output]
      with [distribute cards]. *)
  let distribute_test name cards num_player cards_per_player expected_output = 
    name >:: (fun _ -> 
        assert_equal expected_output 
          (distribute cards num_player |> count_cards 0 cards_per_player) 
          ~printer:string_of_int)

  (** [deck num_players] are cards that are going to be distributed to 
      [num_players] players. *)
  let deck = play_cards shuffled_deck

  (** [test] represents the test suite to be run for the [CardTester]. *)
  let test = [
    random_shuffle_test "Randomly shuffle integer list"
      [1; 156; 34; 31] [1; 31; 34; 156];

    play_cards_test "Make a list of distributable cards for 2 player game"
      shuffled_deck 2 18;
    play_cards_test "Make a list of distributable cards for 3 player game"
      shuffled_deck 3 18;
    play_cards_test "Make a list of distributable cards for 4 player game"
      shuffled_deck 4 16;
    play_cards_test "Make a list of distributable cards for 5 player game"
      shuffled_deck 5 15;

    open_cards_test 
      "Cards that are going to be left over and be shown to 2 players" 
      (deck 2) 0;
    open_cards_test 
      "Cards that are going to be left over and be shown to 3 players"
      (deck 3) 0;
    open_cards_test 
      "Cards that are going to be left over and be shown to 4 players"
      (deck 4) 2;
    open_cards_test 
      "Cards that are going to be left over and be shown to 5 players"
      (deck 5) 3;

    distribute_test "Distribute cards evenly among 2 players"
      (deck 2) 2 9 18;
    distribute_test "Distribute cards evenly among 3 players"
      (deck 3) 3 6 18;
    distribute_test "Distribute cards evenly among 4 players"
      (deck 4) 4 4 16;
    distribute_test "Distribute cards evenly among 5 players"
      (deck 5) 5 3 15;
  ]
end

module GraphTester = struct
  (** [print_tuple] prints the tuple in a readable format. *)
  let print_tuple f (x, y) = "(" ^ f x ^ "," ^ f y ^ ")"

  (** [empty_map_matrix] is an empty adjacency matrix that has the same
      dimensions as our game map. *)
  let empty_map_matrix =
    Array.make_matrix 600 600 0 

  (** [verify_coonections arr] is a list of numbers that represent the 
      connected nodes for a specific tile number, as taken from the adjacency
      matrix representation of our map. *)
  let verify_connections arr =
    let nodes = ref [] in
    for i = 0 to 599 do
      if arr.(i) = 1 then
        nodes := i :: !nodes
      else
        nodes := !nodes
    done;
    !nodes

  (** [get_suggestions_test name expected_output x k] constructs an OUnit test
      that asserts whether the [expected_output] is equal to the output of
      [get_suggestions x k]. *)
  let get_suggestions_test name expected_output x k = 
    name >:: (fun _ ->
        assert_equal expected_output (get_suggestions x k) 
          ~printer:(string_of_int |> print_tuple |> pp_list))

  (** [adj_matrix_test name expected_output arr] constructs an OUnit test that
      asserts the equality of [expected_output] to [verify_connections arr]. *)
  let adj_matrix_test name expected_output arr =
    name >:: (fun _ ->
        assert_equal expected_output (verify_connections arr)
          ~printer:(string_of_int |> pp_list))

  (** [test] represents the test suite to be run for the [GraphTester]. *)
  let test = [
    get_suggestions_test "only tiles" 
      [(1, 296); (2, 319); (3, 321); (4, 342); 
       (5, 346); (6, 367); (7, 369); (8, 392)] 
      344 2;
    get_suggestions_test "in room check for exit" 
      [(1, 403); (2, 426); (3, 428); 
       (4, 455); (5, 551); (6, 570); 
       (7, 574); (8, 595); (9, 597)] 
      500 5;
    get_suggestions_test "room next to wall" 
      [(1, 148); (2, 173); (3, 198); 
       (4, 246); (5, 271); (6, 288); 
       (7, 290); (8, 294); (9, 313)] 
      240 8;
    get_suggestions_test "tile next to exit" 
      [(1, 137); (2, 160); (3, 162); (4, 183); 
       (5, 187); (6, 206); (7, 212); (8, 231); 
       (9, 235); (10, 256); (11, 258); (12, 281)] 
      209 3;
    get_suggestions_test "tile next to wall" 
      [(1, 15); (2, 40); (3, 88); (4, 111)] 63 2;
    get_suggestions_test "end of map" [(1, 551); (2, 574); (3, 597)] 599 2;
    get_suggestions_test "start tile" 
      [(1, 155); (2, 178); (3, 180); (4, 201); (5, 205); 
       (6, 224); (7, 230); (8, 247); (9, 255); (10, 270);
       (11, 295); (12, 303); (13, 320); (14, 326); (15, 345);
       (16, 349); (17, 370); (18, 372); (19, 395)] 
      275 5;

    adj_matrix_test "empty map" [] empty_map_matrix.(200);
    adj_matrix_test "first tile" [24; 1] graph_map.(0);
    adj_matrix_test "last tile" [598; 575] graph_map.(599);
    adj_matrix_test "bottom row" [584; 582; 559] graph_map.(583);
    adj_matrix_test "last element of row" 
      [335; 310; 287] graph_map.(311);
    adj_matrix_test "start tile" [299; 276; 274; 251] graph_map.(275);
    adj_matrix_test "floor next to wall" [39; 16] graph_map.(15);
    adj_matrix_test "room next to wall" 
      [141; 139; 116] graph_map.(140);
    adj_matrix_test "exit tile" 
      [222; 199; 197; 174] graph_map.(198);
    adj_matrix_test "normal floor tile" 
      [367; 344; 342; 319] graph_map.(343);
    adj_matrix_test "corner tile" [47; 22] graph_map.(23);
    adj_matrix_test "next to start tile" 
      [298; 275; 273; 250] graph_map.(274)
  ]
end

module CommandTester = struct
  (** [parse_test name str expected_output] constructs an OUnit test named
      [name] that asserts whether the parsed result of string [str] matches
      the [expected_output]. *)
  let parse_test name str expected_output =
    name >:: (fun _ -> 
        assert_equal expected_output (parse str))

  (** [parse_error_test name error str] constructs an OUnit test named [name]
      that asserts the exception result of attempting to parse malformed or an
      empty string [str]. *)
  let parse_error_test name error str =
    name >:: (fun _ -> assert_raises error (fun () -> parse str))

  (** [test] represents the test suite to be run for the [CommandTester]. *)
  let test = [
    parse_test "Roll dice" "roll" Roll;
    parse_test "Go to one of the available options"
      "go 1" (Go ["1"]);
    parse_test "Hand - check the player's hands" "hand" Hand;
    parse_test "History - check the player's trackers" "history" History;
    parse_test "Guess - ask other player to show these cards" 
      "guess peacock hall revolver" (Guess ["peacock"; "hall"; "revolver"]);
    parse_test "Answer Lead Pipe" 
      "answer Lead Pipe" (Answer ["lead";"pipe"]);
    parse_test "Help needed" "help" Help;
    parse_test "Quit game" "quit" Quit;

    parse_error_test "Raise Empty error for empty string"
      Empty "";
    parse_error_test "Raise Malformed error for roll"
      Malformed "roll lead pipe";
    parse_error_test "Raise Malformed error for go" 
      Malformed "go";
    parse_error_test "Raise Malformed error for go - non-option"
      Malformed "go 12 123";
    parse_error_test "Raise Malformed error for hand"
      Malformed "hand me something";
    parse_error_test "Raise Malformed error for history"
      Malformed "history of the United States";
    parse_error_test "Raise Malformed error for answer"
      Malformed "answer";
    parse_error_test "Raise Malformed error for help"
      Malformed "help Scarlett";
    parse_error_test "Raise Malformed error for quit" 
      Malformed "quit Lounge";            
  ]
end

module PlayerTester = struct
  (** [pp_card] pretty-prints the input card. *)
  let pp_card = function
    | Suspect s | Room s | Weapon s -> s
    | Undef -> failwith "Impossible"

  (** [pp_tile tile] pretty-prints the tile [tile] *)
  let pp_tile tile = 
    "{" ^ string_of_int tile.id ^ "; " ^ tile.desc ^ "}"

  (** [pp_mark mark] pretty-prints the the base unit of tracker [mark]. *)
  let pp_mark mark =
    "(" ^ pp_card (fst mark) ^ ", " ^ string_of_bool (snd mark) ^ ")"

  (** [pp_pstate ps] pretty-prints the player-state *)
  let pp_pstate ps = 
    "{" ^ pp_tile ps.location ^ "; " 
    ^ pp_list pp_mark ps.tracker ^ "; " 
    ^ pp_list pp_card ps.hand

  (** [init_state_test name cards expected_output] constructs an OUnit test
      named [name] that asserts accuracy of [init_state] through
      [expected_output] to ensure that the same record of [player_state]
      is created. *)
  let init_state_test name cards expected_output =
    name >:: (fun _ -> 
        assert_equal ~printer: pp_pstate expected_output (init_state cards))

  (** [update_location_test name st tile expected_output] constrcuts an OUnit
      test named [name] that asserts accuracy of [update_location] through
      [expected_output] to ensure that the location of a player is changed
      to a new location [tile]. *)
  let update_location_test name st tile expected_output = 
    name >:: (fun _ ->
        assert_equal ~printer: 
          pp_tile expected_output (update_location tile st).location)

  (** [update_tracker_test name tracker card expected_output] constructs an 
      OUnit test named [name] that asserts accuracy of [update_tracker] 
      through [expected_output] to ensure that the boolean corresponding to
      [card] in [tracker] is correctly changed. *)
  let update_tracker_test name tracker card expected_output =
    name >:: (fun _ -> 
        assert_equal ~printer: (string_of_bool)
          expected_output (List.assoc card (update_tracker tracker card)))

  (** [update_marks_test name st marks expected_output] constructs an OUnit
      test named [name] that asserts accuracy of [update_marks] through
      [expected_output] to ensure that that the tracker of [st] is correctly
      updated as [marks]. *)
  let update_marks_test name st marks expected_output =
    name >:: (fun _ -> 
        assert_equal ~printer: (pp_pstate)
          expected_output (update_marks marks st))

  (** [update_hand_test name st cards expected_output] constructs an OUnit test
      named [name] that asserts accuracy of [update_hand] through
      [expected_output] to ensure that the player correctly receives each card
      from [cards] *)
  let update_hand_test name st cards expected_output = 
    name >:: (fun _ ->
        assert_equal expected_output (update_hand cards st))

  (** [update_state_test name st tile card cards expected_output] constructs an
      OUnit test named [name] that asserts accuracy of a combination of 
      [update_location], [update_marks], and [update_hand] to ensure that
      [st] can accurately update from receiving various data. *)
  let update_state_test name st tile card cards expected_output =
    name >:: (fun _ -> 
        assert_equal ~printer: (pp_pstate) expected_output 
          (st 
           |> update_location tile 
           |> update_marks (update_tracker st.tracker card) 
           |> update_hand cards))

  (** [empty_tracker] is an empty tracker that has been initialized. *)
  let empty_tracker = new_tracker

  (** [updated_tracker1] is an updated tracker with suspect Scarlett *)
  let updated_tracker1 = update_tracker empty_tracker (Suspect "Scarlett")

  (** [updated_tracker2] is an updated tracker with suspect Green *)
  let updated_tracker2 = update_tracker empty_tracker (Suspect "Green")

  let rec update_multiple_cards acc = function
    | [] -> acc
    | h :: t -> update_multiple_cards (update_tracker acc h) t

  (** [st_t] is a completely empty player state *)
  let st_t = init_state []

  (** [st_t1] is a state with hands of Hall and Plum *)
  let st_t1 = {st_t with hand = [Room "Hall"; Suspect "Plum"]}
  let st_t2 = {st_t with tracker = updated_tracker1}
  let st_t3 = {st_t with location = {id = 299; desc = "Start"}; 
                         tracker = updated_tracker2}
  let st_t4 = {st_t3 with hand = [Weapon "Dagger"; Room "Library"]}

  (** [test] represents the test suite to be run for the [PlayerTester]. *)
  let test = [
    init_state_test "Testing whether the initial state is created successfully"
      [] st_t;

    update_location_test "Update the location to one right"
      st_t {id = 276; desc = "Start"} {id = 276; desc = "Start"};
    update_location_test "Update the location to another room"
      st_t {id = 155; desc = "Exit"} {id = 155; desc = "Exit"};

    update_tracker_test "update one card on tracker" 
      empty_tracker (Suspect "Green") true;
    update_tracker_test "update a new card on an updated tracker" 
      updated_tracker1 (Suspect "Green") true;
    update_tracker_test "update a repeated card on an updated tracker" 
      updated_tracker1 (Suspect "Scarlett") true;
    update_tracker_test "update tracker given multiple cards to update"
      (update_multiple_cards empty_tracker 
         [Suspect "Scarlett"; Suspect "Green"; Room "Billiard Room"; 
          Room "Library"; Weapon "Revolver"; Weapon "Rope"]) 
      (Weapon "Wrench") true;

    update_marks_test "no change to marks"
      st_t empty_tracker st_t;
    update_marks_test "update one card to state"
      st_t updated_tracker1 st_t2;

    update_hand_test "Update the cards that the player has - no cards"
      st_t [] st_t;
    update_hand_test "Update the cards that the player has - one card"
      st_t [Room "Hall"; Suspect "Plum"] st_t1;

    update_state_test "Only Scarlett updated on tracker"
      st_t st_t.location (Suspect "Scarlett") [] st_t2;
    update_state_test "Move one down and with Green updated true"
      st_t {id = 299; desc = "Start"} (Suspect "Green") [] st_t3;
    update_state_test "Move one down, Green updated true with "
      st_t {id = 299; desc = "Start"} (Suspect "Green") [] st_t3;
  ]
end

module UserTester = struct
  (** [capitalize_test name lst expected_output] constructs an OUnit test with
      name [name] that asserts whether the [expected_output] is the result of 
      [capitalize_entry lst]. *)
  let capitalize_test name lst expected_output =
    name >:: (fun _ -> assert_equal expected_output (capitalize_entry lst))

  (** [lst_intersect_test name lst0 lst1 expected_output] constructs an OUnit
      test with name [name] that asserts whether the [expected_output] is the
      result of [lst_intersect lst0 lst1]. *)
  let lst_intersect_test name lst0 lst1 expected_output =
    name >:: (fun _ -> 
      assert_equal expected_output (lst_intersect [] lst0 lst1))

  let test = [
    capitalize_test "Capitalize the string list"
      ["hello"; "world"] ["Hello"; "World"];

    lst_intersect_test "Intersection of two empty list"
      [] [] [];
    lst_intersect_test "Intersection of empty and nonempty list"
      [] [1; 2; 5] [];
    lst_intersect_test "Intersection of nonempty and empty list"
      [1; 4; 5] [] [];
    lst_intersect_test "Intersection of two nonempty list"
      [1; 4; 5] [2; 4; 5] [5; 4];   
  ]
end

module QueueTester = struct
  (** [empty_queue_test name expected_output] constructs an OUnit test that
      asserts whether the [expected_output] is an empty queue. *)
  let empty_queue_test name expected_output = 
    name >:: (fun _ -> assert_equal expected_output empty_q)

  (** [enqueue_test name x q expected_output] constructs an OUnit test that
      asserts whether the [expected_output] is the result of enqueueing the
      value [x] to the back of the queue [q]. *)
  let enqueue_test name x q expected_output =
    name >:: (fun _ -> assert_equal expected_output (enqueue x q))

  (** [test] represents the test suite to be run for the [QueueTester]. *)
  let test = [
    empty_queue_test "Empty queue is empty" [];

    enqueue_test "Enqueue to an empty queue" 1 empty_q [1];
    enqueue_test "Enqueue to a non-empty queue" 14 [55] [55; 14];
    enqueue_test "Enqueue to a full queue" 42 [1; 2; 3; 4; 5] [2; 3; 4; 5; 42];
  ]
end

(** [dice_tests] is a list of tests dedicated to testing the [Dice] module,
    created by [DiceTester] module. *)
let dice_tests = DiceTester.test

(** [cards_tests] is a list of tests dedicated to testing the [Cards] module,
    created by [CardTester] module. *)
let cards_tests = CardTester.test

(** [graph_tests] is a list of tests dedicated to testing the [Graph] module,
    created by [GraphTester] module. *)
let graph_tests = GraphTester.test

(** [command_tests] is a list of tests dedicated to testing the [Command]
    module, created by [CommandTester] module. *)
let command_tests = CommandTester.test

(** [player_tests] is a list of tests dedicated to testing the [Player]
    module, created by [PlayerTester] module. *)
let player_tests = PlayerTester.test

(** [user_tests] is a list of tests dedicated to testing some of the [User]
    module, created by [UserTester] module. *)
let user_tests = UserTester.test

(** [queue_tests] is a list of tests dedicated to testing the [Queue]
    module, created by [QueueTester] module. *)
let queue_tests = QueueTester.test

(** [test_suite] runs the OUnit tests. *)
let test_suite =
  "test suite"  >::: List.flatten [
    dice_tests;
    cards_tests;
    graph_tests;
    command_tests;
    player_tests;
    user_tests;
    queue_tests
  ]

(** Runs the OUnit test suite [test_suite]. *)
let _ = run_test_tt_main test_suite
