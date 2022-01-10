open! Core
open! Types
module N = Vdom.Node
module A = Vdom.Attr

let conn kind name =
  match kind with
  | `Provide -> A.data "src-name" (Name.to_string name)
  | `Consume -> A.class_ ("dest-class-" ^ Name.to_string name)
;;

let rec value_to_html ~point_to (me : Value.t) =
  match me with
  | Singleton ->
    [ [ N.div
          ~attr:
            (A.many
               [ A.classes [ "value" ]
               ; A.data "kind" "singleton"
               ; conn `Provide point_to
               ])
          []
      ]
    ]
  | Named name ->
    [ [ N.div
          ~attr:
            (A.many
               [ A.classes [ "value" ]
               ; A.data "kind" "named"
               ; conn `Provide point_to
               ; conn `Consume name
               ])
          []
      ]
    ]
  | Mapn [] -> failwith "mapn with an empty list?"
  | Mapn children ->
    let me = Name.create () in
    let children, attrs =
      List.fold
        children
        ~init:([], A.many [])
        ~f:(fun (children, attr) child ->
          match child with
          | Named name -> children, A.many [ attr; conn `Consume name ]
          | other -> other :: children, attr)
    in
    let children =
      children
      |> List.map ~f:(value_to_html ~point_to:me)
      |> List.reduce_balanced ~f:(fun a b ->
             let abs, rest = List.zip_with_remainder a b in
             let abs : N.t list list = List.map abs ~f:(fun (a, b) -> a @ b) in
             match rest with
             | None -> abs
             | Some (First a) -> abs @ a
             | Some (Second b) -> abs @ b)
      |> Option.value ~default:[]
    in
    [ N.div
        ~attr:
          (A.many
             [ A.classes [ "value" ]
             ; A.data "kind" "mapn"
             ; conn `Consume me
             ; conn `Provide point_to
             ; attrs
             ])
        []
    ]
    :: children
;;

let value_to_html ~point_to value =
  let rows =
    value
    |> value_to_html ~point_to
    |> List.rev_map ~f:(fun l -> N.div ~attr:(A.many [ A.classes [ "hbox" ] ]) l)
  in
  N.div ~attr:(A.many [ A.classes [ "vbox" ] ]) rows
;;

let compare_bindings
    { Binding.bound = bound1; as_ = as1 }
    { Binding.bound = bound2; as_ = as2 }
  =
  if Set.mem bound2.free_variables as1
  then -1
  else if Set.mem bound1.free_variables as2
  then 1
  else 0
;;

let rec computation_to_html ~point_to (c : Computation.t) =
  match c with
  | { kind = Value v; free_variables = _ } ->
    N.div
      ~attr:(A.many [ A.classes [ "computation" ]; A.data "kind" "return" ])
      [ value_to_html ~point_to v ]
  | { kind = Bindings { bindings; last_body }; free_variables = _ } ->
    { Binding.bound = last_body; as_ = point_to } :: bindings
    |> List.sort_and_group ~compare:compare_bindings
    |> List.map ~f:(fun row ->
           row
           |> List.map ~f:(fun { as_; bound } -> computation_to_html bound ~point_to:as_)
           |> N.div ~attr:(A.classes [ "hbox" ]))
    |> N.div ~attr:(A.classes [ "vbox" ])
  | _ -> assert false
;;

let computation_to_html c =
  let out = Name.create () in
  computation_to_html c ~point_to:out
;;

let to_html t =
  N.div ~attr:(A.class_ "map") [ N.svg []; N.div [ computation_to_html t ] ]
  |> N.to_string
;;
