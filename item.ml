exception InvalidItem of string

type t = 
  | Potion 
  | HyperPotion
  | FullRestore
  | PokeBall
  | GreatBall
  | UltraBall
  | MasterBall

let string_of_item i = 
  match i with 
  | Potion -> "Potion"
  | HyperPotion -> "Hyper Potion"
  | FullRestore -> "Full Restore"
  | PokeBall -> "Poke Ball"
  | GreatBall -> "Great Ball"
  | UltraBall -> "Ultra Ball"
  | MasterBall -> "Master Ball"

let item_of_string i = 
  match i with 
  | "potion" -> Potion
  | "hyper potion" -> HyperPotion
  | "full restore" -> FullRestore
  | "poke ball" -> PokeBall
  | "great ball" -> GreatBall
  | "ultra ball" -> UltraBall
  | "master ball" -> MasterBall
  | s -> raise (InvalidItem s)