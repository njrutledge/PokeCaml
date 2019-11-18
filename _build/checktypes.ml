module type AdventureSig = sig
  type t
  type town_id = string
  type exit_name = string
  exception UnknownTown of town_id
  exception UnknownExit of exit_name
  val from_json : Yojson.Basic.t -> t
  val start_town : t -> town_id
  val town_ids : t -> town_id list
  val description : t -> town_id -> string
  val exits : t -> town_id -> exit_name list
  val next_town : t -> town_id -> exit_name -> town_id
  val next_towns : t -> town_id -> town_id list
end

module AdventureCheck : AdventureSig = Adventure

module type CommandSig = sig
  type object_phrase = string list
  type command = | Go of object_phrase
                 | Unlock of object_phrase
                 | Lock of object_phrase
                 | Take of object_phrase
                 | Drop of object_phrase
                 | Bag
                 | Score
                 | Quit
  exception Empty
  exception Malformed
  val parse : string -> command
end

module CommandCheck : CommandSig = Command

module type StateSig = sig
  type t 
  val init_state : Adventure.t -> t
  val current_town_id : t -> string
  val visited : t -> string list
  type result = Legal of t | Illegal of string
  val go : Adventure.exit_name -> Adventure.t -> t -> result
end

module StateCheck : StateSig = State

module type AuthorSig = sig
  val hours_worked : int list
end

module AuthorCheck : AuthorSig = Author
