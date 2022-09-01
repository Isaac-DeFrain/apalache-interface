open! Core

type kind =
  | Def of string
  | Alias of string
[@@deriving sexp, hash, equal, compare]

let to_kind k name =
  match k with
  | "def" -> Def name
  | "alias" -> Alias name
  | _ -> assert false

let of_kind = function
  | Def s -> s
  | Alias s -> s

module Kind = struct
  type t = kind [@@deriving sexp, hash, equal, compare]
end

(* get apalache declarations *)

let apalache_decl_map path =
  let apalache_lines = Stdio.In_channel.read_lines (path ^ ".apalache") in
  let apalache_map = Hashtbl.create (module Kind) in
  List.iter apalache_lines ~f:(fun s ->
      let open String in
      match split ~on:':' s with
      | [ kind_name; type_def ] -> (
        match
          strip kind_name |> split ~on:' ' |> List.filter ~f:(( <> ) "")
        with
        | [ kind; name ] ->
          let key = to_kind kind name in
          let data = strip type_def in
          ignore @@ Hashtbl.add apalache_map ~key ~data
        | _ -> ())
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
