open Util
open Enc_lexer
open Lexing

exception Encode_struct of string
exception Encode_type of string
exception Varargs of string

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_type str =
  let lexbuf = Lexing.from_string str in
  try Enc_parser.prog Enc_lexer.token lexbuf with
  | SyntaxError msg ->
    Printf.eprintf "Lexing SyntaxError: %s\n" msg;
    Printf.eprintf "%a: syntax error\n%s\n" print_position lexbuf str;
    None
  | Enc_parser.Error ->
    Printf.eprintf "Parsing Error: %a: syntax error\n%s\n"
      print_position lexbuf str;
    None
;;

(* FIXME refacor: intermediat type *)
let tag_name_to_type name =
  let remove_leading_underscores str =
    Str.replace_first (Str.regexp "^_+") "" str
  and append_dot_t str =
    if String.begins_with_char '(' str || not (is_upper (String.get str 0))
    then str
    else str ^ ".t"
  in
  remove_leading_underscores name
  |> apply_type_exceptions
  |> append_dot_t
;;

let rec string_of_objc_type : Objc_type.t -> string = function
| `Id -> "id"
| `Class -> "_Class"
| `Sel -> "_SEL"
| `Void -> "void"
| `String -> "string"
| `Bool -> "bool"
| `Uchar -> "uchar"
| `Int -> "int"
| `Uint -> "uint"
| `Short -> "short"
| `Ushort -> "ushort"
| `Long -> "long"
| `Ulong -> "ulong"
| `Llong -> "llong"
| `Ullong -> "ullong"
| `Float -> "float"
| `Double -> "double"
| `Ldouble -> "ldouble"
| `Bitfield _n -> "short"
| `Unknown | `Block -> "(ptr void)"
| `Pointer t -> "(ptr " ^ string_of_objc_type t ^ ")"
| `Modifier (_, t) -> string_of_objc_type t
| `Type name -> tag_name_to_type name
| `Struct (tag_opt, _fields) ->
  (* string option * (string option * t) list *)
  if Option.is_some tag_opt then
    Option.get tag_opt |> tag_name_to_type
  else
    raise (Encode_struct "Missing tag")
| `Array (_n, t) ->
  (* A C array type is a pointer *)
  "(ptr " ^ string_of_objc_type t ^ ")"
| `Union (tag_opt, _fields) ->
    (* string option * (string option * t) list *)
    if Option.is_some tag_opt then
      Option.get tag_opt |> tag_name_to_type
    else
      raise (Encode_struct "Missing tag")
;;

let type64_to_ctype_string ty_str =
  try
    if String.equal String.empty ty_str then
      raise (Failure "ty_str is empty");
    parse_type ty_str
    |> Option.map string_of_objc_type
    |> Option.get
  with e ->
    (Printf.eprintf "Parse type error: '%s'\n%!" ty_str;
    Printexc.print_backtrace stderr;
    raise e)

let enc_to_ctype_string enc =
  match type64_to_ctype_string enc with
  | "" ->
    (Printf.eprintf "Parse type error: %s\n%!" enc;
    Printexc.print_backtrace stderr;
    raise (Encode_type enc))
  | ty -> ty
