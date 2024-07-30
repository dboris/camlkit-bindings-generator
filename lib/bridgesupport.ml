open Util

module S = Soup

exception GenerateError of string

let funcs = ref []
let inlines = ref []

let type64_to_objc_type x =
  try
    S.attribute "type64" x
    |> Option.map Encode.parse_type
    |> Option.get
    |> Option.get
  with
  | (Invalid_argument _) as e ->
    Printf.eprintf
    "type64_to_objc_type failed: %s\n" (S.attribute "name" x |> Option.get);
    raise e

let type64_to_objc_type_string x =
  try
    S.attribute "type64" x
    |> Option.map Encode.parse_type
    |> Option.get
    |> Option.map Encode.string_of_objc_type
    |> Option.get
  with
  | (Invalid_argument _) as e ->
    Printf.eprintf
    "type64_to_objc_type_string failed: %s\n" (S.attribute "name" x |> Option.get);
    raise e

let emit_const x =
  let name = Option.get (S.attribute "name" x)
  and t = type64_to_objc_type_string x
  in
  let safe_name =
    if is_upper (String.get name 0) then "_" ^ name else name in
  if String.equal t "id" then
    Printf.printf "let %s = new_string \"%s\"\n" safe_name name
  else
    Printf.printf "let %s = !@ (Foreign.foreign_value \"%s\" %s)\n" safe_name name t

let emit_enum x =
  let name = Option.get (S.attribute "name" x)
  and value = S.attribute "value64" x
  and ignore = S.attribute "ignore" x |> Option.is_some
  in
  if not ignore then
    let v =
      if Option.is_some value then
        Option.get value
      else
        raise (GenerateError (Printf.sprintf "emit_enum failed: %s\n" name))
    in
    Printf.printf "let %s = %s\n"
      (if is_upper (String.get name 0) then "_" ^ name else name)
      v

let opaque_module_exception = function
| "CGAffineTransform" -> true
| _ -> false

let structs = Hashtbl.create 13

let emit_opaque_dep x name =
  let ty = type64_to_objc_type x in
  let is_ref = String.ends_with ~suffix:"Ref" name in
  match is_ref, ty with
  | true, `Pointer (`Type mod_name) ->
     let mod_name = Str.replace_first (Str.regexp "^_+") "" mod_name in
     if not (opaque_module_exception mod_name) then (
       match Hashtbl.find_opt structs ty with
       | Some true -> []
       | _ ->
          Hashtbl.add structs ty true;
          [ Printf.sprintf "module %s = struct" mod_name
          ; Printf.sprintf
              "  let t : [`%s] structure typ = structure \"%s\""
              mod_name mod_name
          ; "end";
          ]
     ) else
       []
  | _ -> []

let emit_opaque fw x =
  let name = Option.get (S.attribute "name" x)
  and t = type64_to_objc_type_string x in
  let self_ref = String.equal t ("(ptr " ^ name ^ ".t)") in
  let result =
    [ Printf.sprintf "module %s = struct" name ] @
    begin
      if self_ref then
        [ Printf.sprintf
            "  let s : [`%s] structure typ = structure \"%s\"" name name
        ; Printf.sprintf "  let t = ptr s"
        ; "  " ^ emit_doc_comment fw name
        ]
      else
        [ Printf.sprintf "  let t = %s" t ]
    end @
    [ "end\n" ]
  in
  emit_opaque_dep x name @ result

let emit_type_module fw mod_name =
  let filename = Printf.sprintf "data/%s/%s.ml" fw mod_name in
  if not (Sys.file_exists filename) then
    let file = open_out filename in
    emit_prelude ~fw file;
    Printf.fprintf file
      "let t : [`%s] structure typ = structure \"%s\"\n" mod_name mod_name;
    Printf.fprintf file "%s" (emit_doc_comment fw mod_name);
    close_out file

let emit_cftype fw x =
  let name = Option.get (S.attribute "name" x)
  and t = type64_to_objc_type_string x
  in
  let is_mutable = String.member "Mutable" name
  and is_ref = String.ends_with ~suffix:"Ref" name
  and mod_name =
    name
    |> Str.replace_first (Str.regexp "Mutable") ""
    |> Str.replace_first (Str.regexp "Ref$") ""
  in
  if not is_mutable (* && not is_ref *) then
    emit_type_module fw mod_name;
    (* begin
      (* First, emit a struct type module *)
      Printf.printf "module %s = struct\n" mod_name;
      Printf.printf
        "  let t : [`%s] structure typ = structure \"%s\"\n" mod_name mod_name;
      print_endline ("  " ^ (emit_doc_comment fw mod_name));
      print_endline "end"
    end; *)

  if is_mutable || is_ref then begin
    (* Second, emit a ref module, ie ptr to the struct *)
    Printf.printf "module %s = struct\n" name;
    Printf.printf "  let t = %s\n" t;
    print_endline "end\n"
  end

let select_children tag el =
  S.children el
  |> S.elements
  |> S.fold (fun acc el -> if S.name el = tag then el :: acc else acc) []
  |> List.rev

let rec func_type el =
  let arg_types =
    select_children "arg" el
    |> List.map (fun arg ->
      try
        begin match S.attribute "function_pointer" arg with
        | Some "true" ->
           let args = func_type arg in
           "Foreign.funptr (" ^ args ^ ")"
        | _ ->
          match S.attribute "type64" arg with
          | Some ty -> Encode.type64_to_ctype_string ty
          | None ->
            S.attribute "type" arg
            |> Option.map Encode.type64_to_ctype_string
            |> Option.get
        end
      with
      | Failure _ as e ->
        Printf.eprintf
          "Encode arg failed: %s\n" (S.attribute "name" arg |> Option.get);
        raise e)
  and ret =
    try
      select_children "retval" el
      |> List.hd
      |> S.attribute "type64"
      |> Option.get
      |> Encode.type64_to_ctype_string
    with
    | Encode.Encode_struct _ -> "ptr void"
    | _ ->
      let error = "func_type retval or type64 is None" in
      Printf.eprintf "%s\n" error;
      raise (GenerateError error)
  in
  begin match List.length arg_types with
  | 0 -> "void"
  | 1 -> List.hd arg_types
  | _ -> String.concat " @-> " arg_types
  end ^ " @-> returning " ^ ret
;;

let emit_inline_func name ty =
  Printf.sprintf "(* let %s = Foreign.foreign \"%s\" (%s) *)"
    (if is_upper (String.get name 0) then "_" ^ name else name)
    ("Camlkit_" ^ name)
    ty
;;

let emit_func _fw x =
  let name = Option.get (S.attribute "name" x)
  and ty = func_type x
  in
  (* collect functions for emiting a stub during separate pass *)
  match S.attribute "inline" x with
  | Some "true" ->
    inlines := (name, ty) :: !inlines;
    funcs := emit_inline_func name ty :: !funcs
  | _ ->
    funcs :=
      Printf.sprintf "let %s = Foreign.foreign \"%s\" (%s)"
        (if is_upper (String.get name 0) then "_" ^ name else name)
        name ty
      (* ; emit_doc_comment ~search:true fw name ^ "\n" *)
      :: !funcs
;;

let emit_funcs () =
  !funcs |> List.rev

let emit_inlines fw =
  match !inlines with
  | [] -> []
  | fns ->
    (Printf.sprintf {|#include <%s/%s.h>|} fw fw ^ "\n")
    ::
    (fns |> List.rev |> List.map @@ fun (name, _ty) ->
      Printf.sprintf {|void * Camlkit_%s = &%s;|} name name)
;;

let emit_struct fw x =
  let name = Option.get (S.attribute "name" x) in
  try
    match
      S.attribute "type64" x
      |> Option.map Encode.parse_type
      |> Option.join
    with
    | Some (`Struct (tag_opt, field_list)) ->
      let tag_name = tag_opt |> Option.value ~default:name
      and accessors =
        field_list |> List.mapi @@ fun i (fname_opt, _) ->
          let fname =
            fname_opt
            |> Option.value ~default:("f" ^ string_of_int i)
            |> valid_name
          in
          Printf.sprintf "let %s t = getf t %s" fname fname
      in
      name,
      [ Printf.sprintf
          "let t : [`%s] structure typ = structure \"%s\"" name tag_name
      ; emit_doc_comment fw tag_name ^ "\n"
      ] @
      begin
        field_list |> List.mapi (fun i (fname_opt, ty) ->
          let fname =
            fname_opt |> Option.value ~default:("f" ^ string_of_int i) in
          ty
          |> Encode.string_of_objc_type
          |> Printf.sprintf "let %s = field t \"%s\" %s" (valid_name fname) fname)
      end @
      [ "\nlet () = seal t\n" ] @
      accessors
    | _ -> assert false
  with _ ->
    Printf.eprintf "Skipping struct %s...\n" name;
    name, []