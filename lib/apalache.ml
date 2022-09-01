open! Core

[@@@warning "-26"]

(* apalache + tla -> tla *)

let get_tla_decl_name s =
  let open String in
  let s = strip s in
  let f c = not Char.(c = '-' || c = '\\' || c = '=' || c = '(' || c = '*' || c = '[') in
  let s' = String.take_while s ~f |> strip in
  if s <> s' then s' else ""

(*  *)
let translate path =
  let open Hashtbl in
  let tla_lines = Stdio.In_channel.read_lines path in
  let tla_op_num_map =
    let tla_map = create (module String) in
    List.iteri tla_lines ~f:(fun i s ->
      let s' = get_tla_decl_name s in
      if not String.(is_empty s') then
        add tla_map ~key:s ~data:i |> fun _ -> ());
    tla_map
  in
  (* get apalache decls *)
  (* insert type defs where appropriate *)
  ()

(* tla -> apalache + tla *)

let rec group_block_comments ?(is_block = false) ?(temp = []) acc =
  let open String in
  function
  | [] -> List.rev acc
  | hd :: tl -> (
    match prefix hd 2 with
    | "\\*" -> group_block_comments ~is_block (hd :: acc) ~temp tl
    | "(*" -> group_block_comments ~is_block:true acc ~temp:[ hd ] tl
    | _ ->
      if is_block then
        if suffix hd 2 <> "*)" then
          group_block_comments ~is_block acc ~temp:(hd :: temp) tl
        else
          group_block_comments ~is_block:false
            (concat ~sep:"\n" (List.rev (hd :: temp)) :: acc)
            tl
      else group_block_comments ~is_block (hd :: acc) ~temp tl)

(* let rec lines_to_comment_decls = () *)

(* translate and clean the .tla file *)
let untranslate path =
  let tla_lines = Stdio.In_channel.read_lines path in
  ()
