---- MODULE example ----

EXTENDS Integers

(* multiline block comment about
   the variable x
*)

VARIABLE
    \* @type: a;
    x

vars == <<x>>

\* @type: Int => Int;
incr(z) == z + 1

\* @type: (Int, Int) -> Int;
add[ y \in Int, z \in Int ] == y + z

\* @type: Str;
const == "42"

Init == x = 0

\* profound line comment about the extremely subtle Next action
\* we probably have a ton to say about this so here's another line comment
Next == FALSE

Spec == Init /\ [][Next]_vars

========================
