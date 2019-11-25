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
  | "pokeball" -> PokeBall
  | "great ball" -> GreatBall
  | "ultra ball" -> UltraBall
  | "master ball" -> MasterBall
  | s -> raise (InvalidItem s)

let cost_of_item i = 
  match String.lowercase_ascii i with 
  | "potion" -> 300
  | "hyper potion" -> 600
  | "full restore" -> 900
  | "pokeball" -> 200
  | "great ball" -> 400
  | "ultra ball" -> 600
  | "master ball" -> 1000
  | s -> raise (InvalidItem s)