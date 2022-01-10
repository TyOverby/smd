open! Core
open! Bindgen

let t = Computation.return (Value.singleton ())

let print name t =
  printf "<h1> %s </h1>\n" name;
  print_endline "<div class='testcase'>";
  print_endline (To_html.to_html t);
  print_endline "</div>"
;;

let () =
  print_endline
    {|
  <!DOCTYPE html>
  <html> 
  <head>
    <link rel="stylesheet" href="./style.css">
    <script src="./arrows.js"></script>
  </head>
  <body>
  |}
;;

let () = Computation.return (Value.singleton ()) |> print "return named value"

let () =
  Computation.return (Value.mapn [ Value.singleton () ]) |> print "mapn with single child"
;;

let () =
  Computation.return
    (Value.mapn [ Value.singleton (); Value.singleton (); Value.singleton () ])
  |> print "mapn with multiple children "
;;

let () =
  Computation.return
    (Value.mapn
       [ Value.mapn [ Value.singleton (); Value.singleton (); Value.singleton () ]
       ; Value.mapn [ Value.singleton (); Value.singleton (); Value.singleton () ]
       ])
  |> print "multiple mapns"
;;

let () =
  Computation.return
    (Value.mapn
       [ Value.mapn [ Value.singleton (); Value.singleton (); Value.singleton () ]
       ; Value.mapn
           [ Value.mapn [ Value.singleton (); Value.singleton (); Value.singleton () ] ]
       ])
  |> print "multiple mapns (offset)"
;;

let () =
  let bound_as = Name.create () in
  Computation.sub
    ~bound:(Computation.return (Value.singleton ()))
    ~as_:bound_as
    ~for_:(Computation.return (Value.named bound_as))
  |> print "sub"
;;

let () =
  let a = Name.create () in
  let b = Name.create () in
  Computation.sub
    ~bound:(Computation.return (Value.singleton ()))
    ~as_:a
    ~for_:
      (Computation.sub
         ~bound:(Computation.return (Value.singleton ()))
         ~as_:b
         ~for_:
           (Computation.return
              (Value.mapn [ Value.named a; Value.mapn [ Value.named a ]; Value.named b ])))
  |> print "sub"
;;

let () = print_endline {|
  </body>
  </html>
  |}
