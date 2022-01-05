open! Core

module Name : sig
  type t

  include Comparable.S with type t := t
end

module rec Kind : sig
  type t =
    | Bindings of Binding.t list
    | Value of Value.t
    | Wrapping of
        { name : string
        ; introduces : Name.t list
        ; bodies : Computation.t list
        }
end

and Binding : sig
  type t =
    { bound : Computation.t
    ; as_ : Name.t
    ; for_ : Computation.t
    }
  [@@deriving sexp]
end

and Value : sig
  type t =
    | Named of Name.t
    | Mapn of Value.t list
end

and Computation : sig
  type nonrec t =
    { kind : Kind.t
    ; free_variables : Name.Set.t
    }
end
