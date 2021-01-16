(** 
   Representation of dynamic dice data.

   This module represents two dice being rolled. It tells the player the 
   total number of possible moves he or she can make.
*)

(** The type of two dice outcomes. *)
type outcome = (int * int)

(** [roll sides] is the outcome of the numbers produced by rolling 
    two fair dice with [sides] sides. *)
val roll : int -> outcome

(** [dice_sum result] is the sum of the numbers from 
    the roll of dice [result]. *)
val dice_sum : outcome -> int