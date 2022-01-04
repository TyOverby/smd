module Ucharr = Uchar
open Core
open! Bonsai_term
open! Notty
open! Notty.Infix
open Bonsai.Let_syntax

let outline i =
  let attr = A.(fg lightblack) in
  let w, h = I.(width i, height i) in
  let chr x = I.uchar ~attr (Ucharr.of_int x) 1 1
  and hbar = I.uchar ~attr (Ucharr.of_int 0x2500) w 1
  and vbar = I.uchar ~attr (Ucharr.of_int 0x2502) 1 h in
  let a, b, c, d = chr 0x256d, chr 0x256e, chr 0x256f, chr 0x2570 in
  let frame =
    a <|> hbar <|> b <-> (vbar <|> I.void w h <|> vbar) <-> (d <|> hbar <|> c)
  in
  frame </> I.pad ~t:1 ~l:1 i
;;

module Event_list = struct
  type t = Handle.Event.t list [@@deriving sexp, equal]
end

let concatlike m a ~add =
  Bonsai.state_machine0 [%here] m a ~apply_action:(fun ~inject:_ ~schedule_event:_ m a ->
      add m a)
;;

let component size =
  let%sub text =
    concatlike (module String) (module String) ~add:( ^ ) ~default_model:""
  in
  let%sub unhandled =
    concatlike
      (module Event_list)
      (module Handle.Event)
      ~default_model:[]
      ~add:(Fn.flip List.cons)
  in
  let%arr _width, _height = size
  and text, append_text = text
  and unhandled, append_unhandled = unhandled in
  let view =
    unhandled
    |> List.map ~f:(fun e ->
           e |> Handle.Event.sexp_of_t |> Sexp.to_string_mach |> I.string)
    |> I.vcat
    |> outline
    <-> outline (I.string text)
  in
  let handle_key = function
    | `Key (`ASCII c, []) -> append_text (String.of_char c)
    | other -> append_unhandled other
  in
  view, handle_key
;;

let () =
  let handle = Handle.create component in
  Handle.loop handle
;;
