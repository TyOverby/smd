open! Core

module Attr : sig
  type t

  val class_ : string -> t
  val classes : string list -> t
  val id : string -> t
  val data : string -> string -> t
  val many : t list -> t
end

module Node : sig
  type t
  type creator := ?attr:Attr.t -> t list -> t

  val text : string -> t
  val svg : creator
  val div : creator
  val span : creator
  val to_string : t -> string
end
