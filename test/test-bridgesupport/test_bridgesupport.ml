(* open Runtime *)
open Lib

module A = Alcotest
module B = Bridgesupport
module S = Soup

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

let suite =
  [ "test_CGAffineTransform", `Quick, test_emit_CGAffineTransform
  ; "test_emit_CGPDFArray", `Quick, test_emit_CGPDFArray
  ; "test_emit_CGPDFString", `Quick, test_emit_CGPDFString
  ]

let () = A.run "Bridgesupport" [ "emit", suite ]
