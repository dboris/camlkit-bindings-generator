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

let apply_type_exceptions ?(allow_underscore = true) = function
| "NSZone" | "acl" | "stat" | "xpc_type_s" | "objc_method_description"
| "AEDesc" | "addrinfo" | "CFStorage" | "FloatPoint" | "IntRect" | "IntPoint"
| "Object" | "String" | "SecTrust" | "OpaqueWKContext" | "ProcessSerialNumber"
| "OpaqueWKPageGroup" | "OpaqueWKPage" | "OpaqueWKBackForwardListItem"
| "OpaqueWKInspector" | "OpaqueWKFrame" | "OpaqueWKPageConfiguration"
| "xmlNode" | "xmlParserCtxt" | "xmlElementContent" | "xmlTextReader" | "xmlDoc"
| "NSRefCountedRunArray" | "NSProgressFraction" | "NSSlice" | "SKCAction"
| "NSAppleEventManagerSuspension" | "opaque_pthread_mutex_t" | "CGImageProvider"
| "opaqueCMSampleBuffer" | "EPolygonList" | "MRCDescriptor" | "sFILE"
| "CCColorProfileContext" | "CCCharBox" | "CCBigBox" | "CCBox" | "FSRef"
| "CCPulseWindowContext" | "rgbaColor" | "rgbMinMaxU8" | "rgbMinMaxFloat"
| "CGImageSource" | "CGImageMetadata" | "CGSRegionObject"
| "vImage_Buffer" | "SerialObjectPtrArray" | "filterShape" | "CC_MD5state_st"
| "mapped_model_file" | "FastRegistration_Signatures" | "AuthorizationOpaqueRef"
| "OpaqueCUIRendererRef" | "SpeechChannelRecord" | "RXRecognitionSystem"
| "CGSnappingInfo" | "LSASN" | "OpaqueLSSharedFileListRef" | "CGLPBufferObject"
| "OpaqueWindowPtr" | "EventTypeSpec" | "OpaqueIconRef" | "OpaqueCoreDrag"
| "OpaqueEventRef" | "OpaqueMenuRef" | "TISInputSource" | "WorkspaceKVOData_t"
| "OpaqueHIPresentationInstanceRef" | "OpaqueATSUStyle" | "AXUIElement"
| "SLMPopUpMenuContext_t" | "RXRecognizer" | "GSEvent" | "GSKeyboard"
| "IOHIDEvent" | "UIWebTouchEvent" ->
  "void"
| "va_list_tag" ->
  "(ptr void)"
| "NLConstraintParameters" | "GLKMatrix2" | "GLKMatrix3" | "GLKMatrix4"
| "GLKVector2" | "GLKVector3" | "GLKVector4" | "CCRange"
| "vec2" | "vec3" | "vec4" | "Texture" | "Rectangle" | "Options" | "Point"
| "NSKeyValueCodingControllerModeType" | "NSToolbarFullScreenContentViewLayout_t"
| "CIPredictionModelImageFeatures" | "Geometry2D_rect2D_" | "UIRectCornerRadii"
| "UITableConstantsBackgroundProperties" | "UIPeripheralAnimationGeometry"
| "ITColor" | "mach_right_send" | "PUSimpleIndexPath" | "PXSimpleIndexPath"
| "PHDisplayVelocity" | "PXAssetMediaTypeCount" | "PXAssetBadgeInfo"
| "PUGridCoordinates" | "PUDisplayVelocity" | "CTGlyphStorage" as ty ->
  raise (Unsupported_type ty)
| ty ->
  if not allow_underscore && String.contains ty '_' then
    raise (Unsupported_type ty)
  else if String.contains ty '<' then
    "id"  (* protocol-implementing types are object pointers *)
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

let ignored_class_method = function
| "alloc" | "new_" | "allocWithZone" -> true
| _ -> false

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

let emit_prelude ~open_modules file =
  [ "(* auto-generated, do not modify *)\n"
  ; "open Runtime"
  ; "open Objc"
  ]
  |> String.concat "\n"
  |> Printf.fprintf file "%s\n\n";

  match open_modules with
  | [] -> ()
  | opens ->
    String.concat "\n" opens |> Printf.fprintf file "%s\n\n"
;;

let load_framework fw =
  if not (String.equal fw "") then
    let bundle =
      NSBundle.self |> NSBundleClass.bundleWithPath (new_string fw) in
    if is_nil bundle then
      Printf.eprintf "Framework bundle not found!\n%!"
    else
      NSBundle.load bundle |> ignore
;;

let class_image class_name =
  let cls = Objc.get_class class_name in
  if is_nil cls then
    failwith "Class not found"
  else
    Inspect.Objc.get_image_name cls
;;

let open_modules mod_names =
  if String.(equal mod_names empty) then
    []
  else
    "[@@@ocaml.warning \"-33\"]"
    ::
    (String.split_on_char ',' mod_names |> List.map ((^) "open "))
;;

let safe_enum_value = function
| "18446744073709551615" -> "ULong.max_int"
| "9223372036854775808" -> "LLong.max_int"
| "9223372036854775807" -> "LLong.(pred max_int)"
| v -> v