open! Core

(* get apalache declarations *)

let apalache_decl_map path =
  let apalache_lines = Stdio.In_channel.read_lines (path ^ ".apalache") in
  let apalache_map = Hashtbl.create (module String) in
  List.iter apalache_lines ~f:(fun s ->
      let open String in
      match split ~on:':' s with
      | [ name; type_def ] ->
        let name = strip name in
        let type_def = strip type_def in
        ignore @@ Hashtbl.add apalache_map ~key:name ~data:type_def
      | _ -> ());
  apalache_map

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
          let flushed = List.rev (hd :: temp) in
          group_block_comments ~is_block:false
            (concat ~sep:"\n" flushed :: acc)
            tl
      else group_block_comments ~is_block (hd :: acc) ~temp tl)
