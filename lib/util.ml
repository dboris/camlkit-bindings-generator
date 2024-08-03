open Foundation
open Runtime

exception Unsupported_type of string

module String = struct
  include String

  (* s1 in s2? *)
  let member s1 s2 =
    try
      ignore Str.(search_forward (regexp_string s1) s2 0);
      true
    with Not_found -> false

  let begins_with_char ch str =
    Char.equal (String.get str 0) ch
end

let is_upper c =
  let code = Char.code c in
  code >= 65 && code <= 90
;;

let apply_type_exceptions = function
| "NSZone" | "acl" | "stat" | "xpc_type_s" | "objc_method_description"
| "xmlNode" | "xmlParserCtxt" | "xmlElementContent" | "xmlTextReader" | "xmlDoc"
| "NSRefCountedRunArray" | "NSProgressFraction" | "NSSlice" | "SKCAction"
| "NSAppleEventManagerSuspension" | "opaque_pthread_mutex_t" | "CGImageProvider"
| "opaqueCMSampleBuffer" | "EPolygonList" | "MRCDescriptor" | "sFILE"
| "CCColorProfileContext" | "CCCharBox" | "CCBigBox" | "CCBox"
| "CCPulseWindowContext" | "rgbaColor" | "rgbMinMaxU8" | "rgbMinMaxFloat" ->
  "void"
| "va_list_tag" ->
  "(ptr void)"
| "NLConstraintParameters" | "GLKMatrix2" | "GLKMatrix3" | "GLKMatrix4"
| "GLKVector2" | "GLKVector3" | "GLKVector4" | "CCRange" as ty ->
  raise (Unsupported_type ty)
| ty ->
  if String.contains ty '_' then
    raise (Unsupported_type ty)
  else
    ty

let is_reserved = function
| "and" | "as" | "assert" | "asr" | "begin" | "class" | "constraint" | "do"
| "done" | "downto" | "else" | "end" | "exception" | "external" | "false"
| "for" | "fun" | "function" | "functor" | "if" | "in" | "include"
| "inherit" | "initializer" | "land" | "lazy" | "let" | "lor" | "lsl" | "lsr"
| "lxor" | "match" | "method" | "mod" | "module" | "mutable" | "new"
| "nonrec" | "object" | "of" | "open" | "or" | "private" | "rec" | "sig"
| "struct" | "then" | "to" | "true" | "try" | "type" | "val" | "virtual"
| "when" | "while" | "with" | "selector" | "id" | "self"
| "string" | "float" | "int" -> true
| _ -> false

let is_private sel =
  Char.equal (String.get sel 0) '.' || String.contains sel '_'
;;

let valid_name name =
  if is_upper (String.get name 0) then "_" ^ name
  else if is_reserved name then name ^ "_"
  else name
;;

let remove_empty str_list =
  str_list |> List.filter_map @@ fun a ->
    if String.equal a String.empty then
      Option.none
    else
      Option.some a
;;

let split_selector sel =
  match String.split_on_char ':' sel with
  | [] -> assert false
  | name :: args ->
    match args with
    | [] ->
      (valid_name name, [])
    | xs ->
      let xs' = remove_empty xs |> List.map valid_name in
      (valid_name name, "x" :: xs')
;;

let arg_labels args =
  args
  |> List.map ((^) "~")
  |> String.concat " "
;;

let emit_doc_comment ?(search=false) fw symbol =
  if search then
    Printf.sprintf
      "(** Apple docs: {{:https://developer.apple.com/search/?q=%s}%s} *)"
      symbol symbol
  else
    Printf.sprintf
      "(** Apple docs: {{:https://developer.apple.com/documentation/%s/%s?language=objc}%s} *)"
      (String.lowercase_ascii fw)
      (String.lowercase_ascii symbol)
      symbol
;;

(* Prelude for `type` modules *)
let emit_prelude ~fw file =
  [ "(* auto-generated, do not modify *)\n"
  ; "open Runtime"
  ; "open Objc"
  ]
  |> String.concat "\n"
  |> Printf.fprintf file "%s\n\n";

  let open_fw =
    match String.lowercase_ascii fw with
    (* | "corefoundation" ->
      [ "open CoreFoundation_globals" ] *)

    | "foundation" ->
      [ "[@@@ocaml.warning \"-33\"]"
      ; "open CoreFoundation"
      ; "open CoreFoundation_globals"
      ; "open CoreGraphics"
      ; "open CoreGraphics_globals"
      ; "open CoreAnimation"
      ; "open CoreAnimation_globals"
      ; "open AppleEvents"
      ; "open AppleEvents_globals"
      ]

    | "coregraphics" | "naturallanguage" ->
      [ "[@@@ocaml.warning \"-33\"]"
      ; "open CoreFoundation"
      ; "open CoreFoundation_globals"
      ]

    | "coreanimation" | "photosui" ->
      [ "[@@@ocaml.warning \"-33\"]"
      ; "open CoreFoundation"
      ; "open CoreFoundation_globals"
      ; "open CoreGraphics"
      ; "open CoreGraphics_globals"
      ; "open UIKit"
      ]

    | "coreautolayout" ->
      [ "[@@@ocaml.warning \"-33\"]"
      ; "open Foundation"
      ; "open CoreGraphics"
      ]

    | "appleevents" ->
      [ "[@@@ocaml.warning \"-33\"]"
      ; "open CoreFoundation"
      ; "open AppleEvents_globals"
      ]

    | "coreimage" | "coretext" ->
      [ "[@@@ocaml.warning \"-33\"]"
      ; "open CoreFoundation"
      ; "open CoreFoundation_globals"
      ; "open CoreGraphics"
      ; "open CoreGraphics_globals"
      ]

    | "spritekit" ->
      [ "[@@@ocaml.warning \"-33\"]"
      ; "open CoreFoundation"
      ; "open CoreFoundation_globals"
      ; "open CoreGraphics"
      ; "open CoreGraphics_globals"
      ; "open CoreText"
      ; "open CoreText_globals"
      ]

    | "vision" ->
      [ "[@@@ocaml.warning \"-33\"]"
      ; "open CoreFoundation"
      ; "open CoreFoundation_globals"
      ; "open CoreGraphics"
      ; "open CoreGraphics_globals"
      ; "open CoreVideo"
      ; "open CoreVideo_globals"
      ]

    | "uifoundation" ->
      [ "[@@@ocaml.warning \"-33\"]"
      ; "open CoreFoundation"
      ; "open CoreFoundation_globals"
      ; "open CoreGraphics"
      ; "open CoreGraphics_globals"
      ; "open CoreAnimation"
      ; "open CoreAnimation_globals"
      ; "open CoreText"
      ; "open CoreText_globals"
      ; "open UIKit"
      ]

    | _ -> []
  in
  match open_fw with
  | [] -> ()
  | fw ->
    fw
    |> String.concat "\n"
    |> Printf.fprintf file "%s\n\n"
;;

(* Prelude for the bridgesupport globals module only *)
let emit_globals_prelude fw =
  [ "(* auto-generated, do not modify *)\n"
  ; "open Runtime"
  ; "open Objc"
  ]
  |> String.concat "\n"
  |> Printf.printf "%s\n\n";

  let open_fw =
    match String.lowercase_ascii fw with
    (* | "corefoundation" ->
      [ "open CoreFoundation_globals" ] *)

    | "fsevents" ->
      [ "open CoreFoundation" ]

    | "foundation" | "corevideo" ->
      [ "[@@@ocaml.warning \"-33\"]"
      ; "open CoreFoundation"
      ; "open CoreFoundation_globals"
      ; "open CoreGraphics"
      ; "open CoreGraphics_globals"
      ; "open CoreAnimation"
      ; "open CoreAnimation_globals"
      ]

    | "coregraphics" ->
      [ "[@@@ocaml.warning \"-33\"]"
      ; "open CoreFoundation"
      ; "open CoreFoundation_globals"
      ; "\n"
      ; "module ProcessSerialNumber = struct let t = void end"
      ]

    | "coreanimation" | "uifoundation" | "coretext" ->
      [ "[@@@ocaml.warning \"-33\"]"
      ; "open CoreFoundation"
      ; "open CoreFoundation_globals"
      ; "open CoreGraphics"
      ; "open CoreGraphics_globals"
      ]

    | "vision" ->
      [ "[@@@ocaml.warning \"-33\"]"
      ; "open CoreFoundation"
      ; "open CoreFoundation_globals"
      ; "open CoreGraphics"
      ; "open CoreGraphics_globals"
      ; "open CoreVideo"
      ; "open CoreVideo_globals"
      ]

    | "appleevents" ->
      [ "[@@@ocaml.warning \"-33\"]"
      ; "open CoreFoundation"
      ; "module AEArrayData  = struct let t = void end"
      ]

    | _ -> []
  in
  match open_fw with
  | [] ->
    Printf.eprintf "Not emitting globals prelude\n"
  | fw ->
    fw
    |> String.concat "\n"
    |> Printf.printf "%s\n\n"
;;

let load_framework fw =
  if not (String.equal fw "") then
    let bundle =
      NSBundle._class_ |> NSBundle.C.bundleWithPath (new_string fw) in
    if is_nil bundle then
      Printf.eprintf "Framework bundle not found!\n%!"
    else
      NSBundle.load bundle |> ignore
