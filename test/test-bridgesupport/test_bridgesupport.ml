(* open Runtime *)
open Lib

module A = Alcotest
module B = Bridgesupport
module S = Soup
module M = Markup

let parse_xml str =
  M.string str
  |> M.parse_xml
  |> M.signals
  |> S.from_signals

let fw = "CoreGraphics"

let test_emit_CGAffineTransform () =
  let expected =
    [ "module CGAffineTransform = struct"
    ; "  let s : [`CGAffineTransform] structure typ = structure \"CGAffineTransform\""
    ; "  let t = ptr s"
    ; "  (** Apple docs: {{:https://developer.apple.com/documentation/coregraphics/cgaffinetransform?language=objc}CGAffineTransform} *)"
    ; "end\n"
    ]
  and actual =
    "<opaque name='CGAffineTransform' type64='^{CGAffineTransform=}'/>"
    |> S.parse
    |> S.select_one "opaque"
    |> Option.get
    |> B.emit_opaque fw
  in
  A.check A.(list string) "same type" expected actual

let test_emit_CGPDFArray () =
  let expected =
    [ "module CGPDFArray = struct"
    ; "  let t : [`CGPDFArray] structure typ = structure \"CGPDFArray\""
    ; "end"
    ; "module CGPDFArrayRef = struct"
    ; "  let t = (ptr CGPDFArray.t)"
    ; "end\n"
    ]
  and actual =
    "<opaque name='CGPDFArrayRef' type64='^{CGPDFArray=}'/>"
    |> S.parse
    |> S.select_one "opaque"
    |> Option.get
    |> B.emit_opaque fw
  in
  A.check A.(list string) "same type" expected actual
;;

let test_emit_CGPDFString () =
  let expected =
    [ "module CGPDFString = struct"
    ; "  let t : [`CGPDFString] structure typ = structure \"CGPDFString\""
    ; "end"
    ; "module CGPDFStringRef = struct"
    ; "  let t = (ptr CGPDFString.t)"
    ; "end\n"
    ]
  and actual =
    "<opaque name='CGPDFStringRef' type64='^{CGPDFString=}'/>"
    |> S.parse
    |> S.select_one "opaque"
    |> Option.get
    |> B.emit_opaque fw
  in
  A.check A.(list string) "same type" expected actual

let test_function_pointer () =
  let expected = {|
     (ptr CFAllocator.t)
     @-> Foreign.funptr ((ptr FSEventStream.t)
          @-> (ptr void)
          @-> ullong
          @-> (ptr void)
          @-> (ptr uint)
          @-> (ptr ullong)
          @-> returning void)
      @-> (ptr FSEventStreamContext.t)
      @-> (ptr CFArray.t)
      @-> ullong @-> double
      @-> uint
      @-> returning (ptr FSEventStream.t)
      |}
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> List.filter ((<>)"")
  |> String.concat " "
  and actual =
    {|
     <function name='FSEventStreamCreate'>
     <arg type64='^{__CFAllocator=}' type_modifier='n'/>
     <arg function_pointer='true' type64='^?' type_modifier='n'>
        <arg type64='^{__FSEventStream=}'/>
        <arg type64='^v'/>
        <arg type64='Q'/>
        <arg type64='^v'/>
        <arg type64='^I'/>
        <arg type64='^Q'/>
        <retval type64='v'/>
     </arg>
     <arg type64='^{FSEventStreamContext=q^v^?^?^?}' type_modifier='n'/>
     <arg type64='^{__CFArray=}'/>
     <arg type64='Q'/>
     <arg type64='d'/>
     <arg type64='I'/>
     <retval already_retained='true' type64='^{__FSEventStream=}'/>
     </function>
     |}
    |> parse_xml
    |> S.select_one "function"
    |> Option.get
    |> B.func_type
    |> String.split_on_char '\n'
    |> List.map String.trim
    |> List.filter ((<>)"")
    |> List.map String.trim
    |> String.concat " "
  in
  A.check A.string "alias" expected actual

let suite =
  [ "test_CGAffineTransform", `Quick, test_emit_CGAffineTransform
  ; "test_emit_CGPDFArray", `Quick, test_emit_CGPDFArray
  ; "test_emit_CGPDFString", `Quick, test_emit_CGPDFString
  ; "test_function_pointer", `Quick, test_function_pointer
  ]

let () = A.run "Bridgesupport" [ "emit", suite ]
