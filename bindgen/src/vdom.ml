open! Core

module Attr = struct
  type t =
    { classes : string list
    ; other : string
    }

  let stringlike key data = { classes = []; other = key ^ "=\"" ^ data ^ "\"" }
  let combine a b = { classes = a.classes @ b.classes; other = a.other ^ " " ^ b.other }
  let id = stringlike "id"
  let classes l = { classes = l; other = "" }
  let class_ c = classes [ c ]
  let data k = stringlike ("data-" ^ k)
  let empty = { classes = []; other = "" }
  let many l = l |> List.reduce ~f:combine |> Option.value ~default:empty

  let to_string { classes; other } =
    other ^ " class=\"" ^ String.concat classes ~sep:" " ^ "\""
  ;;
end

module Node = struct
  type t = string list list
  type creator = ?attr:Attr.t -> t list -> t

  let create : string -> creator =
   fun tag ?(attr = Attr.empty) children ->
    let children = children |> List.join |> List.map ~f:(fun xs -> "  " :: xs) in
    List.concat
      [ [ [ [%string "<%{tag} %{Attr.to_string attr}>"] ] ]
      ; children
      ; [ [ [%string "</%{tag}>"] ] ]
      ]
 ;;

  let text s = [ [ s ] ]
  let svg = create "svg"
  let div = create "div"
  let span = create "span"

  let to_string (t : t) : string =
    t |> List.map ~f:String.concat |> String.concat ~sep:"\n"
  ;;
end
