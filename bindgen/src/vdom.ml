open! Core

module Attr = struct
  type t = string

  let stringlike key data = key ^ "=\"" ^ data ^ "\""
  let id = stringlike "id"
  let class_ = stringlike "class"
  let data k = stringlike ("data-" ^ k)
  let many = String.concat ~sep:" "
end

module Node = struct
  type t = string list list
  type creator = ?attr:Attr.t -> t list -> t

  let create : string -> creator =
   fun tag ?(attr = "") children ->
    let children = children |> List.join |> List.map ~f:(fun xs -> "  " :: xs) in
    List.concat
      [ [ [ [%string "<%{tag} %{attr}>"] ] ]; children; [ [ [%string "</%{tag}>"] ] ] ]
 ;;

  let text s = [ [ s ] ]
  let div = create "div"
  let span = create "span"

  let to_string (t : t) : string =
    t |> List.map ~f:String.concat |> String.concat ~sep:"\n"
  ;;
end
