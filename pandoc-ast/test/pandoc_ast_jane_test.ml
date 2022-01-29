module Caml_unix = Unix
open! Core
module Pandoc = Pandoc_ast_jane

let pandoc_path = "pandoc"

let run prog args =
  let command = sprintf "%s %s" prog (String.concat args ~sep:" ") in
  let stdout = Caml_unix.open_process_in command in
  In_channel.input_all stdout
;;

let communicate_with_pandoc ~data ~args =
  let tmpfile = String.strip (run "mktemp" []) in
  Out_channel.write_all tmpfile ~data;
  let args = tmpfile :: args in
  run "pandoc" args
;;

let ast_of_markdown markdown =
  communicate_with_pandoc ~data:markdown ~args:[ "-f"; "markdown"; "-t"; "json" ]
;;

let markdown_of_ast markdown =
  communicate_with_pandoc ~data:markdown ~args:[ "-f"; "json"; "-t"; "markdown"; "-s" ]
;;

let big_example =
  {|
---
title: hello world
is_good: true
---

# Numbered Lists

1. a numbered
2. list is
   1. nested
   2. here
3. back out

# Bulleted Lists

- a bulleted
- list is
  - nested
  - here
- back out
  this one has
  some overhang

|}
;;

let%expect_test "small round trip" =
  "# hello" |> ast_of_markdown |> markdown_of_ast |> print_endline;
  [%expect {| # hello |}]
;;

let%expect_test "big round trip" =
  big_example |> ast_of_markdown |> markdown_of_ast |> print_endline;
  [%expect
    {|
    ---
    is_good: true
    title: hello world
    ---

    # Numbered Lists

    1.  a numbered
    2.  list is
        1.  nested
        2.  here
    3.  back out

    # Bulleted Lists

    -   a bulleted
    -   list is
        -   nested
        -   here
    -   back out this one has some overhang |}]
;;

let%expect_test "folding_map" =
  big_example
  |> ast_of_markdown
  |> Pandoc.of_pandoc_ast_string
  |> Pandoc.folding_map
       ~init:true
       ~block:(fun acc block -> acc, block)
       ~inline:(fun acc inline ->
         match acc, inline with
         | true, Str s -> false, Str (String.uppercase s)
         | false, Str s -> true, Str (String.lowercase s)
         | acc, other -> acc, other)
  |> Pandoc.to_pandoc_ast_string
  |> markdown_of_ast
  |> print_endline;
  [%expect
    {|
    ---
    is_good: true
    title: hello world
    ---

    # NUMBERED lists

    1.  A numbered
    2.  LIST is
        1.  NESTED
        2.  here
    3.  BACK out

    # BULLETED lists

    -   A bulleted
    -   LIST is
        -   NESTED
        -   here
    -   BACK out THIS one HAS some OVERHANG |}]
;;

let%expect_test "definition list ast" =
  {|
foo
: from : abc
: to   : def

bar
: abc
         |}
  |> ast_of_markdown
  |> Pandoc.of_pandoc_ast_string
  |> Pandoc.sexp_of_t
  |> print_s;
  [%expect
    {|
    ((api_version (1 22 1)) (meta (Assoc))
     (blocks
      ((UnhandledBlock
        (Assoc (t (String DefinitionList))
         (c
          (List
           (List (List (Assoc (t (String Str)) (c (String foo))))
            (List
             (List
              (Assoc (t (String Plain))
               (c
                (List (Assoc (t (String Str)) (c (String from)))
                 (Assoc (t (String Space)))
                 (Assoc (t (String Str)) (c (String :)))
                 (Assoc (t (String Space)))
                 (Assoc (t (String Str)) (c (String abc)))))))
             (List
              (Assoc (t (String Plain))
               (c
                (List (Assoc (t (String Str)) (c (String to)))
                 (Assoc (t (String Space)))
                 (Assoc (t (String Str)) (c (String :)))
                 (Assoc (t (String Space)))
                 (Assoc (t (String Str)) (c (String def)))))))))
           (List (List (Assoc (t (String Str)) (c (String bar))))
            (List
             (List
              (Assoc (t (String Plain))
               (c (List (Assoc (t (String Str)) (c (String abc)))))))))))))))) |}]
;;

let%expect_test "blockquote ast" =
  {|
> a
> b
> c
> 
>> foo bar
>> baz 
> buz

>> abc
>> def
         |}
  |> ast_of_markdown
  |> Pandoc.of_pandoc_ast_string
  |> Pandoc.sexp_of_t
  |> print_s;
  [%expect
    {|
    ((api_version (1 22 1)) (meta (Assoc))
     (blocks
      ((Blockquote
        ((Para (Str a) (UnhandledInline SoftBreak) (Str b)
          (UnhandledInline SoftBreak) (Str c))
         (Blockquote
          ((Para (Str foo) Space (Str bar) (UnhandledInline SoftBreak) (Str baz)
            (UnhandledInline SoftBreak) (Str buz))))))
       (Blockquote
        ((Blockquote ((Para (Str abc) (UnhandledInline SoftBreak) (Str def))))))))) |}]
;;

let%expect_test "fold" =
  {|
> a
> b
> c
> 
>> foo bar
>> baz 
> buz

>> abc
>> def
         |}
  |> ast_of_markdown
  |> Pandoc.of_pandoc_ast_string
  |> Pandoc.fold
       ~init:""
       ~inline:
         (fun acc -> function
           | Str s -> acc ^ s
           | Space -> acc ^ " "
           | UnhandledInline uh ->
             (match Pandoc.Inline.Additional.reveal uh with
             | SoftBreak -> acc ^ "\n"
             | _ -> acc)
           | _ -> acc)
       ~block:(fun acc _ -> acc ^ "\n\n")
  |> print_endline;
  [%expect {|
    a
    b
    c

    foo bar
    baz
    buz





    abc
    def |}]
;;

let%expect_test "metadata ast" =
  {|
foobar 

---
from : abc
to   : def
---

more 
         |}
  |> ast_of_markdown
  |> Pandoc.of_pandoc_ast_string
  |> Pandoc.sexp_of_t
  |> print_s;
  [%expect
    {|
    ((api_version (1 22 1))
     (meta
      (Assoc
       (from
        (Assoc (t (String MetaInlines))
         (c (List (Assoc (t (String Str)) (c (String abc)))))))
       (to
        (Assoc (t (String MetaInlines))
         (c (List (Assoc (t (String Str)) (c (String def)))))))))
     (blocks ((Para (Str foobar)) (Para (Str more))))) |}]
;;

let%expect_test "big ast" =
  big_example
  |> ast_of_markdown
  |> Pandoc.of_pandoc_ast_string
  |> Pandoc.sexp_of_t
  |> print_s;
  [%expect
    {|
    ((api_version (1 22 1))
     (meta
      (Assoc (is_good (Assoc (t (String MetaBool)) (c (Bool true))))
       (title
        (Assoc (t (String MetaInlines))
         (c
          (List (Assoc (t (String Str)) (c (String hello)))
           (Assoc (t (String Space)))
           (Assoc (t (String Str)) (c (String world)))))))))
     (blocks
      ((Header 1 (numbered-lists () ()) ((Str Numbered) Space (Str Lists)))
       (OrderedList (1 Decimal Period)
        (((Plain (Str a) Space (Str numbered)))
         ((Plain (Str list) Space (Str is))
          (OrderedList (1 Decimal Period)
           (((Plain (Str nested))) ((Plain (Str here))))))
         ((Plain (Str back) Space (Str out)))))
       (Header 1 (bulleted-lists () ()) ((Str Bulleted) Space (Str Lists)))
       (BulletList ((Plain (Str a) Space (Str bulleted)))
        ((Plain (Str list) Space (Str is))
         (BulletList ((Plain (Str nested))) ((Plain (Str here)))))
        ((Plain (Str back) Space (Str out) (UnhandledInline SoftBreak) (Str this)
          Space (Str one) Space (Str has) (UnhandledInline SoftBreak) (Str some)
          Space (Str overhang))))))) |}]
;;

let%expect_test "div ast" =
  {|

test 

:::{}
# h1
:::

after|}
  |> ast_of_markdown
  |> Pandoc.of_pandoc_ast_string
  |> Pandoc.sexp_of_t
  |> print_s;
  [%expect
    {|
    ((api_version (1 22 1)) (meta (Assoc))
     (blocks
      ((Para (Str test)) (Div ("" () ()) ((Header 1 (h1 () ()) ((Str h1)))))
       (Para (Str after))))) |}]
;;

let%expect_test "more bullets ast" =
  {|
- [ ] normal item

  more
  - [ ] unfinished
  - [x] finished

1. [ ] unfinished
2. [x] finished
  |}
  |> ast_of_markdown
  |> Pandoc.of_pandoc_ast_string
  |> Pandoc.sexp_of_t
  |> print_s;
  [%expect
    {|
    ((api_version (1 22 1)) (meta (Assoc))
     (blocks
      ((BulletList
        ((Para (Str "\226\152\144") Space (Str normal) Space (Str item))
         (Para (Str more))
         (BulletList ((Plain (Str "\226\152\144") Space (Str unfinished)))
          ((Plain (Str "\226\152\146") Space (Str finished))))))
       (OrderedList (1 Decimal Period)
        (((Plain (Str "\226\152\144") Space (Str unfinished)))
         ((Plain (Str "\226\152\146") Space (Str finished)))))))) |}]
;;

let%expect_test "checkboxes in other places" =
  {|
[ ]

- [ ] normal [ ] item
  - hi [ ]

-
  |}
  |> ast_of_markdown
  |> Pandoc.of_pandoc_ast_string
  |> Pandoc.sexp_of_t
  |> print_s;
  [%expect
    {|
    ((api_version (1 22 1)) (meta (Assoc))
     (blocks
      ((Para (Str [) Space (Str ]))
       (BulletList
        ((Plain (Str "\226\152\144") Space (Str normal) Space (Str [) Space
          (Str ]) Space (Str item))
         (BulletList ((Plain (Str hi) Space (Str [) Space (Str ])))))
        ())))) |}]
;;

let%expect_test "checkboxe kinds" =
  let doc =
    {|
-
- hello
- [x] checked
- [ ] unchecked
- ```ocaml
  ```
- bar
  - [ ] nested
  |}
    |> ast_of_markdown
    |> Pandoc.of_pandoc_ast_string
  in
  doc |> Pandoc.to_pandoc_ast_string |> markdown_of_ast |> print_endline;
  (match Pandoc.top_level_blocks doc with
  | [ BulletList items ] ->
    items
    |> List.map ~f:Pandoc.List_item.reveal
    |> List.map ~f:[%sexp_of: Pandoc.List_item.details]
    |> List.iter ~f:print_s
  | _ -> assert false);
  [%expect
    {|
    -

    -   hello

    -   [x] checked

    -   [ ] unchecked

    -   ``` ocaml
        ```

    -   bar
        -   [ ] nested

    Empty
    (Normal (Plain (Str hello)))
    (Checked (Plain (Str "\226\152\146") Space (Str checked)))
    (Unchecked (Plain (Str "\226\152\144") Space (Str unchecked)))
    (No_inline_strings (CodeBlock ("" (ocaml) ()) ""))
    (Normal (Plain (Str bar))
     (BulletList ((Plain (Str "\226\152\144") Space (Str nested))))) |}]
;;

let%expect_test "print_permutations" =
  let doc =
    {|
-
- hello
- [x] checked
- [ ] unchecked
- nested
  - [ ] yep
  |}
    |> ast_of_markdown
    |> Pandoc.of_pandoc_ast_string
  in
  let items =
    match Pandoc.top_level_blocks doc with
    | [ BulletList items ] -> items |> List.map ~f:Pandoc.List_item.reveal
    | _ -> assert false
  in
  let print ~title items =
    Pandoc.with_top_level_blocks
      doc
      [ Header
          ( 2
          , Pandoc.Attrs.make ~id:(String.lowercase title) ~classes:[] ~attributes:[]
          , [ Str title ] )
      ; BulletList (List.map items ~f:Pandoc.List_item.conceal)
      ]
    |> Pandoc.to_pandoc_ast_string
    |> markdown_of_ast
    |> print_endline
  in
  print ~title:"Unchanged" items;
  print ~title:"Checked" (List.map items ~f:Pandoc.List_item.check);
  print ~title:"Unchecked" (List.map items ~f:Pandoc.List_item.uncheck);
  print ~title:"Normal" (List.map items ~f:Pandoc.List_item.remove_checkbox);
  [%expect
    {|
    ## Unchanged

    -
    -   hello
    -   [x] checked
    -   [ ] unchecked
    -   nested
        -   [ ] yep

    ## Checked

    -   [x]
    -   [x] hello
    -   [x] checked
    -   [x] unchecked
    -   [x] nested
        -   [ ] yep

    ## Unchecked

    -   [ ]
    -   [ ] hello
    -   [ ] checked
    -   [ ] unchecked
    -   [ ] nested
        -   [ ] yep

    ## Normal

    -
    -   hello
    -   checked
    -   unchecked
    -   nested
        -   [ ] yep |}]
;;

let%expect_test "paragraph ast" =
  {|

this is a test of a paragraph
that has a softbreak inside of it

and this is just a completely different
paragraph, but there's another softbreak.
      |}
  |> ast_of_markdown
  |> Pandoc.of_pandoc_ast_string
  |> Pandoc.sexp_of_t
  |> print_s;
  [%expect
    {|
    ((api_version (1 22 1)) (meta (Assoc))
     (blocks
      ((Para (Str this) Space (Str is) Space (Str a) Space (Str test) Space
        (Str of) Space (Str a) Space (Str paragraph) (UnhandledInline SoftBreak)
        (Str that) Space (Str has) Space (Str a) Space (Str softbreak) Space
        (Str inside) Space (Str of) Space (Str it))
       (Para (Str and) Space (Str this) Space (Str is) Space (Str just) Space
        (Str a) Space (Str completely) Space (Str different)
        (UnhandledInline SoftBreak) (Str paragraph,) Space (Str but) Space
        (Str "there\226\128\153s") Space (Str another) Space (Str softbreak.))))) |}]
;;

let%expect_test "raw html" =
  {|

this is <span class="f"> a test </span> of 

<div>
some inline
</div>

markdown
    |}
  |> ast_of_markdown
  |> markdown_of_ast
  |> print_endline;
  [%expect
    {|
    this is [ a test ]{.f} of

    <div>

    some inline

    </div>

    markdown |}]
;;
