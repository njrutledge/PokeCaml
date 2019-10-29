include moves;
include Pokemon;

type t
type mymon = Pokemon
type enemymon = Pokemon
type monmoves = moves list

val from_json : Yojson.Basic.t -> t
val moves : t -> moves list
val attack : t -> ()
val item : ()
val team : t -> t 
val run : t -> ()