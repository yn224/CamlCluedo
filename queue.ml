
type 'a queue = 'a list

let empty_q = []

(** [dequeue] is [None] if the input queue is empty. If the input queue is not
    empty, then it is [Some q], where [q] is the queue without its first 
    element. *)
let dequeue = function
  | [] -> None
  | _ :: q -> Some q

let enqueue x q = 
  if List.length q < 5 then q @ [x]
  else begin
    match dequeue q with
    | None -> [x]
    | Some q -> q @ [x]
  end