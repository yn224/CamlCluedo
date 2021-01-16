
type outcome = int * int

let roll = 
  let () = Random.self_init () in
  fun sides -> (1 + Random.int sides, 1 + Random.int sides)

let dice_sum result =
  fst result + snd result
  