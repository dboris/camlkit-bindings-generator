open Util
open Printf
module S = Soup

exception GenerateError of string

let funcs = ref []
let inlines = ref []

let type64_to_objc_type x =
  try
    S.attribute "type64" x
    |> Option.map Encode.parse_type
    |> Option.get |> Option.get
  with Invalid_argument _ as e ->
    eprintf "type64_to_objc_type failed: %s\n"
      (S.attribute "name" x |> Option.get);
    raise e

let type64_to_objc_type_string x =
  try
    S.attribute "type64" x
    |> Option.map Encode.parse_type
    |> Option.get
    |> Option.map Encode.string_of_objc_type
    |> Option.get
  with Invalid_argument _ as e ->
    eprintf "type64_to_objc_type_string failed: %s\n"
      (S.attribute "name" x |> Option.get);
    raise e

let emit_const x =
  let name = Option.get (S.attribute "name" x)
  and t = type64_to_objc_type_string x in
  let safe_name = if is_upper (String.get name 0) then "_" ^ name else name in
  if String.equal t "id" then
    printf "let %s = new_string \"%s\"\n" safe_name name
  else printf "let %s = foreign_value_or_null \"%s\" %s\n" safe_name name t

let emit_enum x =
  let name = Option.get (S.attribute "name" x)
  and value = S.attribute "value64" x
  and ignore = S.attribute "ignore" x |> Option.is_some in
  if not ignore then
    let v =
      if Option.is_some value then Option.get value
      else raise (GenerateError (sprintf "emit_enum failed: %s\n" name))
    in
    printf "let %s = %s\n"
      (if is_upper (String.get name 0) then "_" ^ name else name)
      (safe_enum_value v)

let opaque_module_exception = function
  | "CGAffineTransform" -> true
  | _ -> false

let structs = Hashtbl.create 13

let emit_opaque_dep x name =
  let ty = type64_to_objc_type x in
  let is_ref = String.ends_with ~suffix:"Ref" name in
  match (is_ref, ty) with
  | true, `Pointer (`Type mod_name) ->
      let mod_name = Str.replace_first (Str.regexp "^_+") "" mod_name in
      if not (opaque_module_exception mod_name) then (
        match Hashtbl.find_opt structs ty with
        | Some true -> []
        | _ ->
            Hashtbl.add structs ty true;
            [
              sprintf "module %s = struct" mod_name;
              sprintf "  let t : [`%s] structure typ = structure \"%s\""
                mod_name mod_name;
              "end";
            ])
      else []
  | _ -> []

let emit_opaque fw x =
  let name = Option.get (S.attribute "name" x)
  and t = type64_to_objc_type_string x in
  let self_ref = String.equal t ("(ptr " ^ name ^ ".t)") in
  let result =
    [ sprintf "module %s = struct" name ]
    @ (if self_ref then
         [
           sprintf "  let s : [`%s] structure typ = structure \"%s\"" name name;
           sprintf "  let t = ptr s";
           "  " ^ emit_doc_comment fw name;
         ]
       else [ sprintf "  let t = %s" t ])
    @ [ "end\n" ]
  in
  emit_opaque_dep x name @ result

let emit_type_module ~open_modules fw mod_name =
  let filename = sprintf "data/%s/%s.ml" fw mod_name in
  if not (Sys.file_exists filename) then (
    let file = open_out filename in
    emit_prelude ~open_modules file;
    fprintf file "let t : [`%s] structure typ = structure \"%s\"\n" mod_name
      mod_name;
    fprintf file "%s" (emit_doc_comment fw mod_name);
    close_out file)

let emit_cftype ~open_modules fw x =
  let name = Option.get (S.attribute "name" x)
  and t = type64_to_objc_type_string x in
  let is_mutable = String.member "Mutable" name
  and is_ref = String.ends_with ~suffix:"Ref" name
  and mod_name =
    name
    |> Str.replace_first (Str.regexp "Mutable") ""
    |> Str.replace_first (Str.regexp "Ref$") ""
  in
  if not is_mutable (* && not is_ref *) then
    emit_type_module ~open_modules fw mod_name;

  (* begin
      (* First, emit a struct type module *)
      printf "module %s = struct\n" mod_name;
      printf
        "  let t : [`%s] structure typ = structure \"%s\"\n" mod_name mod_name;
      print_endline ("  " ^ (emit_doc_comment fw mod_name));
      print_endline "end"
    end; *)
  if is_mutable || is_ref then (
    (* Second, emit a ref module, ie ptr to the struct *)
    printf "module %s = struct\n" name;
    printf "  let t = %s\n" t;
    print_endline "end\n")

let select_children tag el =
  S.children el |> S.elements
  |> S.fold (fun acc el -> if S.name el = tag then el :: acc else acc) []
  |> List.rev

let select_first tag el =
  S.children el |> S.elements |> S.filter (fun x -> S.name x = tag) |> S.first

let get_type64 x = S.attribute "type64" x |> Option.get

let encode_type x =
  match S.attribute "type64" x with
  | Some ty -> Encode.type64_to_ctype_string ty
  | None ->
      S.attribute "type" x
      |> Option.map Encode.type64_to_ctype_string
      |> Option.get

let rec func_type el =
  let arg_types =
    select_children "arg" el
    |> List.map (fun arg ->
           try
             match S.attribute "function_pointer" arg with
             | Some "true" ->
                 let args = func_type arg in
                 "Foreign.funptr (" ^ args ^ ")"
             | _ -> encode_type arg
           with Failure _ as e ->
             eprintf "Encode arg failed: %s\n"
               (S.attribute "name" arg |> Option.get);
             raise e)
  and ret =
    try
      select_children "retval" el
      |> List.hd |> S.attribute "type64" |> Option.get
      |> Encode.type64_to_ctype_string
    with
    | Encode.Encode_struct _ -> "ptr void"
    | _ ->
        let error = "func_type retval or type64 is None" in
        eprintf "%s\n" error;
        raise (GenerateError error)
  in
  (match List.length arg_types with
  | 0 -> "void"
  | 1 -> List.hd arg_types
  | _ -> String.concat " @-> " arg_types)
  ^ " @-> returning " ^ ret

let emit_inline_func name ty =
  sprintf "let %s = Foreign.foreign \"%s\" (%s)"
    (if is_upper (String.get name 0) then "_" ^ name else name)
    ("Camlkit_" ^ name) ty

let emit_func _fw x =
  let name = Option.get (S.attribute "name" x) and ty = func_type x in
  (* collect functions for emiting a stub during separate pass *)
  match S.attribute "inline" x with
  | Some "true" ->
      let args = select_children "arg" x |> List.map get_type64
      and retval = select_first "retval" x |> Option.get |> get_type64 in
      inlines := (name, args, retval) :: !inlines;
      funcs := emit_inline_func name ty :: !funcs
  | _ ->
      funcs :=
        sprintf "let %s = Foreign.foreign ~stub:true \"%s\" (%s)"
          (if is_upper (String.get name 0) then "_" ^ name else name)
          name ty
        (* ; emit_doc_comment ~search:true fw name ^ "\n" *)
        :: !funcs

let emit_funcs () = !funcs |> List.rev

let emit_inline_stub (name, args, retval) =
  let cdecl = Emit_c.emit_c_fundec ~name:("Camlkit_" ^ name) args retval
  and cargs =
    List.mapi (fun i _ -> "x" ^ Int.to_string i) args |> String.concat ", "
  in
  sprintf "%s {\n  return %s(%s);\n}" cdecl name cargs

let emit_inlines fw =
  match !inlines with
  | [] -> []
  | fns ->
      (sprintf {|#include <%s/%s.h>|} fw fw ^ "\n")
      :: sprintf "void _ensure_inlines_object_file_linked(void) {}\n"
      :: (fns |> List.rev |> List.map emit_inline_stub)

let emit_struct fw x =
  let name = Option.get (S.attribute "name" x) in
  try
    match
      S.attribute "type64" x |> Option.map Encode.parse_type |> Option.join
    with
    | Some (`Struct (tag_opt, field_list)) ->
        let field_names =
          field_list
          |> List.mapi @@ fun i (fname_opt, f_type) ->
             fname_opt
             |> Option.value ~default:("f" ^ string_of_int i)
             |> valid_name
             |> fun x -> (x, f_type)
        in
        let has_pointer_field =
          field_names
          |> List.exists @@ function _, `Pointer _ -> true | _ -> false
        in
        let tag_name = tag_opt |> Option.value ~default:name
        and getters =
          field_names
          |> List.map @@ fun (fname, _) ->
             sprintf "let %s t = getf t %s_field" fname fname
        and setters =
          field_names
          |> List.map @@ fun (fname, _) ->
             sprintf "let set%s t = setf t %s_field"
               (String.capitalize_ascii fname)
               fname
        and init =
          "let init"
          :: (field_names
             |> List.map @@ fun (fname, obj_type) ->
                match obj_type with
                | `Pointer ptr ->
                    let typ = Encode.string_of_objc_type ptr in
                    sprintf "    ?(%s = from_voidp %s null)" fname typ
                | _ -> sprintf "    ~%s" fname)
          @ [
              (if has_pointer_field then "    ()\n  =" else "  =");
              "  let t = make t in";
            ]
          @ (field_names
            |> List.map @@ fun (fname, _) ->
               sprintf "  setf t %s_field %s;" fname fname)
          @ [ "  t\n" ]
        in
        ( name,
          [
            sprintf "type t = [`%s] structure" name;
            emit_doc_comment fw tag_name ^ "\n";
            sprintf "let t : t typ = structure \"%s\"" tag_name;
          ]
          @ (field_names
            |> List.map @@ fun (fname, ty) ->
               ty |> Encode.string_of_objc_type
               |> sprintf "let %s_field = field t \"%s\" %s" (valid_name fname)
                    fname)
          @ [ "\nlet () = seal t\n" ] @ init @ getters @ setters )
    | _ -> assert false
  with _ ->
    eprintf "Skipping struct %s...\n" name;
    (name, [])
