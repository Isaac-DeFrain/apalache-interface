open! Base
open! Tla

(* quick 'n dirty unit tests *)

let _ =
  let open String in
  let get_tla_decl_name s = get_tla_decl_name_and_indent s |> snd in
  let s1 = " abc(x, y, z) == x + y + z" in
  assert (get_tla_decl_name s1 = "abc");
  let s2 = "f[ x \\in Nat ] == x * 2" in
  assert (get_tla_decl_name s2 = "f");
  let s3 = "m == 42" in
  assert (get_tla_decl_name s3 = "m")
