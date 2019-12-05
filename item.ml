exception InvalidItem of string

type t = 
  | Potion 
  | HyperPotion
  | FullRestore
  | PokeBall
  | GreatBall
  | UltraBall
  | MasterBall
  | Antidote
  | ParalyzeHeal
  | Awakening
  | IceHeal
  | BurnHeal
  | FullHeal

let string_of_item i = 
  match i with 
  | Potion -> "Potion"
  | HyperPotion -> "Hyper Potion"
  | FullRestore -> "Full Restore"
  | PokeBall -> "Poke Ball"
  | GreatBall -> "Great Ball"
  | UltraBall -> "Ultra Ball"
  | MasterBall -> "Master Ball"
  | Antidote -> "Antidote"
  | ParalyzeHeal -> "Paralyze Heal"
  | Awakening -> "Awakening"
  | IceHeal -> "Ice Heal"
  | BurnHeal -> "Burn Heal"
  | FullHeal -> "Full Heal"

let item_of_string i = 
  match i with 
  | "potion" -> Potion
  | "hyper potion" -> HyperPotion
  | "full restore" -> FullRestore
  | "pokeball" -> PokeBall
  | "great ball" -> GreatBall
  | "ultra ball" -> UltraBall
  | "master ball" -> MasterBall
  | "antidote" -> Antidote
  | "paralyze heal" -> ParalyzeHeal
  | "awakening" -> Awakening
  | "ice heal" -> IceHeal
  | "burn heal" -> BurnHeal
  | "full heal" -> FullHeal
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
  | "antidote" -> 200
  | "paralyze heal" -> 200
  | "awakening" -> 200
  | "ice heal" -> 200
  | "burn heal" -> 200
  | "full heal" -> 400
  | s -> raise (InvalidItem s)

(** [format_items itm_lst] returns a formatted string of the items in [itm_lst]
    and their costs. *)
let rec format_items = function
  | [] -> ""
  | h :: t -> " - " ^ h ^ ": $" ^ (string_of_int (cost_of_item h)) ^ "\n" ^ 
              (format_items t)
