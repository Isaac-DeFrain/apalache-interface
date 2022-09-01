open! Base
open! Apalache

let example_tla_module =
  "---- MODULE example ----\n\n\
   (* multiline block comment about\n\
  \   the variable x\n\
   *)\n\n\
   VARIABLE x\n\n\
   incr(x) == x + 1\n\n\
   Init == x = 0\n\n\
   \\* profound line comment about the extremely subtle Next action\n\
   \\* we probably have a ton to say about this so here's another line comment\n\n\
   Next == FALSE\n\n\
   Spec == Init /\\ [][Next]_x\n\n\
   ========================"

let%expect_test "group_block_comments" =
  group_block_comments [] (String.split ~on:'\n' example_tla_module)
  |> List.iter ~f:Caml.print_endline;
  [%expect
    {|
  ---- MODULE example ----

  (* multiline block comment about
     the variable x
  *)

  VARIABLE x

  incr(x) == x + 1

  Init == x = 0

  \* profound line comment about the extremely subtle Next action
  \* we probably have a ton to say about this so here's another line comment

  Next == FALSE

  Spec == Init /\ [][Next]_x

  ========================
  |}]
