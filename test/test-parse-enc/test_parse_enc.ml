open Lib

module A = Alcotest

(* Custom testable for the Objc_type.t type *)
let objc_type =
  let pp fmt t =
    Format.fprintf fmt "%s" (Encode.string_of_objc_type t)
  and equal t1 t2 =
    String.equal (Encode.string_of_objc_type t1) (Encode.string_of_objc_type t2)
  in
  A.testable pp equal

let test_parse_int () =
  let expected = `Int
  and actual = Encode.parse_type "i"
  in
  A.check A.bool "Result is Option.some" true (Option.is_some actual);
  A.check objc_type "same type" expected (Option.get actual)

let test_parse_ptr_float () =
  let expected = `Pointer `Float
  and actual = Encode.parse_type "^f"
  in
  A.check A.bool "Result is Option.some" true (Option.is_some actual);
  A.check objc_type "same type" expected (Option.get actual)

let test_parse_ptr_struct () =
  let expected = `Pointer (`Type "__CFData")
  and actual = Encode.parse_type "^{__CFData}" in
  A.check A.bool "Result is Option.some" true (Option.is_some actual);
  A.check objc_type "same type" expected (Option.get actual)

let test_parse_ptr_struct_with_equal () =
  let expected = `Pointer (`Type "__CFData")
  and actual = Encode.parse_type "^{__CFData=}" in
  A.check A.bool "Result is Option.some" true (Option.is_some actual);
  A.check objc_type "same type" expected (Option.get actual)
;;

let test_parse_nested_struct_equal () =
  let expected = `Pointer (`Type "NSTest")
  and actual = Encode.parse_type "^{NSTest=\"fname\"{?=}}" in
  A.check A.bool "Result is Option.some" true (Option.is_some actual);
  A.check objc_type "same type" expected (Option.get actual)
;;

let test_struct_tag_with_digit () =
  let expected = `Pointer (`Type "name1")
  and actual = Encode.parse_type "^{name1}" in
  A.check A.bool "Result is Option.some" true (Option.is_some actual);
  A.check objc_type "same type" expected (Option.get actual)

let test_encode_array () =
  let expected = "(ptr ushort)"
  and actual =
    Encode.parse_type "[10S]"
    |> Option.map Encode.string_of_objc_type
  in
  A.check A.bool "Result is Option.some" true (Option.is_some actual);
  A.check A.string "same string" expected (Option.get actual)

let test_encode_ptr_unknown_structure () =
  let expected = "(ptr void)"
  and actual =
    Encode.parse_type "^{?}"
    |> Option.map Encode.string_of_objc_type
  in
  A.check A.bool "Result is Option.some" true (Option.is_some actual);
  A.check A.string "same string" expected (Option.get actual)

let test_encode_ptr_varargs_structure () =
  let expected = "(ptr void)"
  and actual =
    Encode.parse_type "^{__va_list_tag=II^v^v}"
    |> Option.map Encode.string_of_objc_type
  in
  A.check A.bool "Result is Option.some" true (Option.is_some actual);
  A.check A.string "same string" expected (Option.get actual)

let test_encode_structure () =
  let expected = "CFRange.t"
  and actual =
    Encode.parse_type "{_CFRange=qq}"
    |> Option.map Encode.string_of_objc_type
  in
  A.check A.bool "Result is Option.some" true (Option.is_some actual);
  A.check A.string "same string" expected (Option.get actual)

let test_parse_type_with_modifier () =
  let expected = "string"
  and actual =
    Encode.parse_type "r*"
    |> Option.map Encode.string_of_objc_type
  in
  A.check A.bool "Result is Option.some" true (Option.is_some actual);
  A.check A.string "same string" expected (Option.get actual)

let test_parse_CGAffineTransform () =
  let expected = `Pointer (`Type "CGAffineTransform")
  and actual = Encode.parse_type "^{CGAffineTransform=}" in
  A.check A.bool "Result is Option.some" true (Option.is_some actual);
  A.check objc_type "same type" expected (Option.get actual)

let test_CA_Mode () =
  let expected = `Struct (Some "Mode", [])
  and actual = Encode.parse_type "{Mode=(?={?=b14b14b1b24b1b5b2b1b1}Q)}" in
  A.check A.bool "Result is Option.some" true (Option.is_some actual);
  A.check objc_type "same type" expected (Option.get actual)

let test_parse_VectorField () =
  let expected = "(ptr void)"
  and actual =
    Encode.parse_type "[100{?={CGPoint=dd}if}]"
    |> Option.map Encode.string_of_objc_type
  in
  A.check A.bool "Result is Option.some" true (Option.is_some actual);
  A.check A.string "same string" expected (Option.get actual)

let test_parse_array_of_struct () =
  let expected = "CVPlanarPixelBufferInfo.t"
  and actual =
    Encode.parse_type "{CVPlanarPixelBufferInfo=\"componentInfo\"[1{CVPlanarComponentInfo}]}"
    |> Option.map Encode.string_of_objc_type
  in
  A.check A.bool "Result is Option.some" true (Option.is_some actual);
  A.check A.string "same string" expected (Option.get actual)

let test_parse_protocol () =
  let expected = "StringTag.t"
  and actual =
    Encode.parse_type "{StringTag={RefPtr<WTF::StringImpl>=^{StringImpl}}}"
    |> Option.map Encode.string_of_objc_type
  in
  A.check A.bool "Result is Option.some" true (Option.is_some actual);
  A.check A.string "same string" expected (Option.get actual)

let suite =
  [ "parse int", `Quick, test_parse_int
  ; "parse ptr float", `Quick, test_parse_ptr_float
  ; "parse ptr struct", `Quick, test_parse_ptr_struct
  ; "parse ptr struct with equal", `Quick, test_parse_ptr_struct_with_equal
  ; "parse ptr nested struct", `Quick, test_parse_nested_struct_equal
  ; "parse struct tag with digit", `Quick, test_struct_tag_with_digit
  ; "parse and encode array", `Quick, test_encode_array
  ; "parse and encode ptr to unknown structure", `Quick, test_encode_ptr_unknown_structure
  ; "parse and encode ptr to varargs structure", `Quick, test_encode_ptr_varargs_structure
  ; "parse and encode structure", `Quick, test_encode_structure
  ; "parse type with modifier", `Quick, test_parse_type_with_modifier
  ; "parse and encode CGAffineTransform", `Quick, test_parse_CGAffineTransform
  ; "parse and encode CA_Mode", `Quick, test_CA_Mode
  ; "parse VectorField", `Quick, test_parse_VectorField
  ; "parse array_of_struct", `Quick, test_parse_array_of_struct
  ; "parse protocol", `Quick, test_parse_protocol
  ]

let () = A.run "Enc parser tests" [ "Encode", suite ]
