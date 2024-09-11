open Runtime
open Util

module Encode = Encode
module Objc_type = Objc_type
module Util = Util
module Bridgesupport = Bridgesupport

exception Broken_binding of string

type msg_type =
| Stret of string * string * string list  (* typ, ret_ty, arg_types *)
| Normal of string * string list  (* typ, arg_types *)

type meth =
  { name : string
  ; args : string list
  ; sel : string
  ; typ : msg_type
  }

let pp_msg_type = function
| Stret (a, b, c) ->
  (* string * string * string list *)
  Printf.sprintf "(%s), (%s), (%s)" a b (String.concat "," c)
| Normal (a, b) ->
  (* string * string list *)
  Printf.sprintf "(%s), (%s)" a (String.concat "," b)

let method_type m =
  let num_args =
    Unsigned.UInt.to_int (Method.get_number_of_arguments m) in
  let arg_types =
    (* Skip the implicit self and _cmd *)
    List.init (num_args - 2) @@ fun j ->
      let i = j + 2 in
      try
        Method.get_argument_type m (Unsigned.UInt.of_int i)
        |> Encode.enc_to_ctype_string
      with _ ->
        Printf.eprintf "Failed: %s\tArgs: %s\n"
          (Sel.get_name (Method.get_name m))
          (Method.get_argument_type m (Unsigned.UInt.of_int i));
        "ptr void"
  in
  let ret = Method.get_return_type m in
  try
    Normal
      ( String.concat " @-> " arg_types ^
        (if num_args > 2 then " @-> " else "") ^
        "returning " ^ Encode.enc_to_ctype_string ~raise_on_struct:true ret
      , arg_types
      )
  with Encode.Encode_struct ret_ty ->
    Stret
      ( String.concat " @-> " arg_types ^
        (if num_args > 2 then " @-> " else "") ^
        "returning " ^ Encode.enc_to_ctype_string ret
      , ret_ty
      , arg_types
      )
;;

let converted_arg name = function
| "llong" -> "(LLong.of_int " ^ name ^ ")"
| "ullong" -> "(ULLong.of_int " ^ name ^ ")"
| _ -> name

let string_of_method_binding ~fw {name; args; sel; typ} =
  match args with
  | [] ->
    (* no args *)
    begin match typ with
    | Normal (typ, _) ->
      Printf.sprintf
        "let %s self = msg_send ~self ~cmd:(selector \"%s\") ~typ:(%s)"
        name sel typ
    | Stret (typ, ret_ty, _) ->
      Printf.sprintf
        "let %s self = msg_send_stret ~self ~cmd:(selector \"%s\") ~typ:(%s) ~return_type:%s"
        name sel typ ret_ty
    end
    (* ^ "\n" ^ emit_doc_comment ~search:true fw sel ^ "\n" *)
  | _ :: [] ->
    (* single arg *)
    begin try
      match typ with
      | Normal (typ, arg_types) ->
        Printf.sprintf
          "let %s x self = msg_send ~self ~cmd:(selector \"%s\") ~typ:(%s) %s"
          name sel typ (converted_arg "x" (List.hd arg_types))
      | Stret (typ, ret_ty, arg_types) ->
        Printf.sprintf
          "let %s x self = msg_send_stret ~self ~cmd:(selector \"%s\") ~typ:(%s) ~return_type:%s %s"
          name sel typ ret_ty (converted_arg "x" (List.hd arg_types))
      (* ^ "\n" ^ emit_doc_comment ~search:true fw sel ^ "\n" *)
      with
      | Failure msg as ex ->
        Printf.eprintf "Failure: %s: %s, %s, typ: [%s], args: [%s]\n"
          msg name sel (pp_msg_type typ) (String.concat "," args);
        raise ex
      | ex -> Printf.eprintf "Single arg failure\n"; raise ex
    end
  | _ :: rest as args ->
    (* multiple args *)
    begin match typ with
    | Normal (typ, arg_types) ->
      begin try
        let conv_args = List.map2 converted_arg args arg_types in
        Printf.sprintf
          "let %s x %s self = msg_send ~self ~cmd:(selector \"%s\") ~typ:(%s) %s"
          name (arg_labels rest) sel typ (String.concat " " conv_args)
        (* ^ "\n" ^ emit_doc_comment ~search:true fw sel ^ "\n" *)
      with
      | _ ->
        Printf.eprintf "List.map2 Error: %s %s\n" name typ;
        String.concat " " (List.init (List.length args) (fun _ -> "?"))
        |> Printf.sprintf
          "(* let %s x %s self = msg_send ~self ~cmd:(selector \"%s\") ~typ:(%s) %s *)"
          name (arg_labels rest) sel typ
      end

    | Stret (typ, ret_ty, arg_types) ->
      "\n" ^ emit_doc_comment fw sel ^
      let conv_args = List.map2 converted_arg args arg_types in
      Printf.sprintf
        "let %s x %s self = msg_send_stret ~self ~cmd:(selector \"%s\") ~typ:(%s) ~return_type:%s %s"
        name (arg_labels rest) sel typ ret_ty (String.concat " " conv_args)
      (* ^ "\n" ^ emit_doc_comment ~search:true fw sel ^ "\n" *)
    end
;;

(* check if arg is a duplicate and add '_' suffix *)
let disambiguate_args args =
  let ar = Array.of_list args in
  ar
  |> Array.mapi (fun i a ->
    if i > 0 && Array.mem a (Array.sub ar 0 i) then a ^ "_" else a)
  |> Array.to_list
;;

let method_binding m  =
  let sel = Sel.get_name (Method.get_name m) in
  if is_private sel then
    Option.none
  else
    try
      let name, args = split_selector sel in
      match name with
      | "evaluateCost" | "evaluateCostXZ" | "evaluateCostYZ"
      | "valueWithVectorFloat2" | "valueWithVectorFloat3" | "valueWithVectorFloat4"
      | "setVectorFloat2Value" | "setVectorFloat3Value" | "setVectorFloat4Value"
      | "linearGravityFieldWithVector" | "velocityFieldWithVector"
      | "setDirection" | "setEulerAngles" | "setVelocity"
      | "addPoint" | "emitPoint" | "setLastPoint" | "enumerateInterpolationFromPoint" ->
        raise (Broken_binding name)
      | _ ->
        Option.some
          {name; args = disambiguate_args args; sel; typ = method_type m}
    with exn ->
      Printf.eprintf "Non-fatal exn: %s\n%!" (Printexc.to_string exn);
      Option.none
;;

let eq_name mb {name; _} = String.equal mb.name name
let compare_sel mb {sel; _} = String.compare mb.sel sel
let compare_arg_count mb {args; _} =
  Int.compare (List.length mb.args) (List.length args)

let rename_methods mb_group =
  let l = List.of_seq mb_group in
  let len = List.length l in
  if Int.equal len 1 then
    List.to_seq l
  else
    List.sort compare_arg_count l
    |> List.mapi (fun i mb ->
      if Int.equal i 0 then mb
      else if Int.equal len 2 then {mb with name = mb.name ^ "'"}
      else {mb with name = mb.name ^ string_of_int i})
    |> List.to_seq
;;

let disambiguate mbs =
  mbs
  |> List.to_seq
  |> Seq.group eq_name
  |> Seq.map rename_methods
  |> Seq.concat
  |> List.of_seq
;;

let emit_method_bindings ?(pref = "") ~fw ~file bindings =
  bindings
  |> List.sort_uniq compare_sel
  |> disambiguate
  |> List.map (string_of_method_binding ~fw)
  |> String.concat ("\n" ^ pref)
  |> Printf.fprintf file "%s%s" pref
;;

let emit_metaclass_module ~open_modules ~fw cls cls' =
  let methods =
    Object.get_class cls'
    |> Inspect.methods
    |> List.filter_map method_binding
    |> List.filter (fun {name; _} -> not (Util.ignored_class_method name))
  in
  match methods with
  | [] -> ()
  | methods' ->
    let file = open_out (cls ^ "Class.ml") in
    emit_prelude ~open_modules file;
    Printf.fprintf file "%s\n\n" (emit_doc_comment fw cls);
    emit_method_bindings ~fw ~file methods';
    close_out file
;;

let emit_class_module
  ~fw
  ?(include_superclass = false)
  ?(min_methods = 2)
  ?(open_modules = [])
  cls
=
  let cls' = Objc.get_class cls in
  let super = Class.get_superclass cls' in
  Inspect.methods cls'
  |> List.filter_map method_binding
  |> fun bindings ->
    if List.length bindings >= min_methods then
      let file = open_out (cls ^ ".ml") in
      emit_prelude ~open_modules file;
      Printf.fprintf file "%s\n\n" (emit_doc_comment fw cls);
      Printf.fprintf file "let self = get_class \"%s\"\n\n" cls;

      if include_superclass && not (is_null super) then begin
        let superclass = Class.get_name super in
        if (
          String.starts_with ~prefix:"NS" superclass &&
          not (String.equal superclass "NSObject")
        ) then
          Printf.fprintf file "include %s\n\n" superclass;
      end;

      emit_metaclass_module ~open_modules ~fw cls cls';
      emit_method_bindings ~fw ~file bindings;
      close_out file
;;