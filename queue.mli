(**
  Representation of activity messages as queues.

  Since it is only being used to keep track of small amounts of data, the
  length of the queue is restricted to 5.
*)
 
(** Generic type for the queue implemented using lists. *)
type 'a queue = 'a list

(** [empty_q] is an empty queue. *)
val empty_q : 'a list

(** [enqueue x q] is a new queue with [x] appended at the end of [q]. If [q]
    was already full (i.e., size of 5), then the first element gets dequeued
    and the data next in line becomes the new first element. *)
val enqueue : 'a -> 'a list -> 'a list