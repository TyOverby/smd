open! Core
open! Types
module N = Vdom.Node
module A = Vdom.Attr

module Id = struct
  include Unique_id.Int ()

  let to_string t = "id_" ^ to_string t
end

let connection kind name =
  match kind with
  | `Source ->
    A.many [ A.data "connection" "source"; A.data "name" (Name.to_string name) ]
;;

let rec value_to_html : Value.t -> N.t = function
  | Named name -> N.div [ N.text (Name.to_string name) ]
  | Mapn _others ->
    let id = Id.create () in
    N.text (Id.to_string id)
;;

let rec computation_to_html : Computation.t -> N.t = function
  | { kind = Value v; free_variables } ->
    let incoming = free_variables |> Set.to_list |> ignore in
    N.text "hi"
;;

let to_html t = computation_to_html t |> N.to_string
