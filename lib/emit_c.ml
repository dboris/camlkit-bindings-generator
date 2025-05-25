open Runtime.Objc
module C = Cstubs_c_language

let rec cty_of_objc_type = function
  | `Void -> C.Ty void
  | `String -> C.Ty string
  | `Bool -> C.Ty bool
  | `Uchar -> C.Ty uchar
  | `Int -> C.Ty int
  | `Uint -> C.Ty uint
  | `Short -> C.Ty short
  | `Ushort -> C.Ty ushort
  | `Long -> C.Ty long
  | `Ulong -> C.Ty ulong
  | `Llong -> C.Ty llong
  | `Ullong -> C.Ty ullong
  | `Float -> C.Ty float
  | `Double -> C.Ty double
  | `Ldouble -> C.Ty ldouble
  | `Unknown | `Block -> C.Ty (ptr void)
  | `Pointer (`Type name) -> cty_of_objc_type (`Type (name ^ "Ref"))
  | `Pointer t ->
      let (C.Ty pt) = cty_of_objc_type t in
      C.Ty (ptr pt)
  | `Type name ->
      let name' = Util.remove_leading_underscores name in
      C.Ty (Ctypes_static.typedef (structure name) name')
  | `Struct (tag_opt, _fields) ->
      let name = Option.get tag_opt |> Util.remove_leading_underscores in
      C.Ty (Ctypes_static.typedef (structure name) name)
  | _ ->
      Printf.eprintf "cty_of_objc_type: Not implemented\n";
      C.Ty void

let cty_of_type_enc x = Encode.parse_type x |> Option.get |> cty_of_objc_type

let emit_c_fundec ~name args retval =
  let args_cty =
    args
    |> List.mapi (fun i arg -> ("x" ^ Int.to_string i, cty_of_type_enc arg))
  and retval_cty = cty_of_type_enc retval in
  Cstubs_emit_c.cfundec Format.str_formatter
    (`Fundec (name, args_cty, retval_cty));
  Format.flush_str_formatter ()
