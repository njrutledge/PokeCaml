open Moves;
open Pokemon;

module type Battle = sig
  type t
  type mymon = Pokemon
  type enemymon = Pokemon
  type monmoves = moves list

  val from_json : Yojson.Basic.t -> t
  val moves : t -> Moves list
  val attack : t -> ()
  val item : ()
  val team : t -> t 
  val run : t -> int
end

module Battle = struct
  let moves = failwith ""
  let attack = failwith ""
  let item = failwith ""
  let team battle = failwith ""
  let run battle = return 0
end

let main () = ANSITerminal.(print_string [red]
                              "\n\nStarting battle\n");
