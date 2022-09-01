open! Core
open! Apalache

(* apalache + tla -> tla *)

let get_tla_decl_name_and_indent s =
  let open String in
  let ind = take_while s ~f:Char.(( = ) ' ') |> length in
  let s = strip s in
  let f c =
    not Char.(c = '-' || c = '\\' || c = '=' || c = '(' || c = '*' || c = '[')
  in
  (ind, take_while s ~f |> strip)

(* get tla declarations *)
let tla_decl_map tla_lines =
  let tla_map = Hashtbl.create (module String) in
  List.iteri tla_lines ~f:(fun i s ->
      let s' = get_tla_decl_name_and_indent s in
      let ind = fst s' in
      let s = snd s' in
      if not String.(is_empty s) then
        ignore @@ Hashtbl.add tla_map ~key:s ~data:(i, ind));
  tla_map

let mk_annotation ws key data =
  match key with
  | Def _ -> Printf.sprintf "%s\\* @type: %s;" ws data
  | Alias _ -> Printf.sprintf "%s\\* @typeAlias: %s;" ws data

(* translate apalache type declarations into tla file *)
let translate path =
  let open Hashtbl in
  let tla_path = path ^ ".tla" in
  let tla_lines = Stdio.In_channel.read_lines tla_path in
  let tla_decl_map = tla_decl_map tla_lines in
  let apalache_decl_map = apalache_decl_map path in
  let new_tla_lines =
    let added_lines = ref [] in
    fold apalache_decl_map ~init:tla_lines ~f:(fun ~key ~data acc ->
        match find tla_decl_map (of_kind key) with
        | Some (ln, ind) ->
          let open List in
          let n = count !added_lines ~f:(( > ) ln) in
          let front, back = split_n acc (ln + n) in
          let front =
            match rev front with
            | hd :: tl ->
              if String.(is_prefix (strip hd) ~prefix:"\\* @type") then rev tl
              else (
                added_lines := ln :: !added_lines;
                front)
            | _ ->
              added_lines := ln :: !added_lines;
              front
          in
          let ind = String.make ind ' ' in
          let annotation = mk_annotation ind key data in
          front @ (annotation :: back)
        | None -> acc)
  in
  Stdio.Out_channel.write_lines tla_path new_tla_lines
