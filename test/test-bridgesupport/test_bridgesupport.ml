(* open Runtime *)
open Lib
module A = Alcotest
module B = Bridgesupport
module S = Soup
module M = Markup

let parse_xml str = M.string str |> M.parse_xml |> M.signals |> S.from_signals
let fw = "CoreGraphics"

let test_emit_CGAffineTransform () =
  let expected =
    [
      "module CGAffineTransform = struct";
      "  let s : [`CGAffineTransform] structure typ = structure \
       \"CGAffineTransform\"";
      "  let t = ptr s";
      "  (** Apple docs: \
       {{:https://developer.apple.com/documentation/coregraphics/cgaffinetransform?language=objc}CGAffineTransform} \
       *)";
      "end\n";
    ]
  and actual =
    "<opaque name='CGAffineTransform' type64='^{CGAffineTransform=}'/>"
    |> S.parse |> S.select_one "opaque" |> Option.get |> B.emit_opaque fw
  in
  A.check A.(list string) "same type" expected actual

let test_emit_CGPDFArray () =
  let expected =
    [
      "module CGPDFArray = struct";
      "  let t : [`CGPDFArray] structure typ = structure \"CGPDFArray\"";
      "end";
      "module CGPDFArrayRef = struct";
      "  let t = (ptr CGPDFArray.t)";
      "end\n";
    ]
  and actual =
    "<opaque name='CGPDFArrayRef' type64='^{CGPDFArray=}'/>" |> S.parse
    |> S.select_one "opaque" |> Option.get |> B.emit_opaque fw
  in
  A.check A.(list string) "same type" expected actual

let test_emit_CGPDFString () =
  let expected =
    [
      "module CGPDFString = struct";
      "  let t : [`CGPDFString] structure typ = structure \"CGPDFString\"";
      "end";
      "module CGPDFStringRef = struct";
      "  let t = (ptr CGPDFString.t)";
      "end\n";
    ]
  and actual =
    "<opaque name='CGPDFStringRef' type64='^{CGPDFString=}'/>" |> S.parse
    |> S.select_one "opaque" |> Option.get |> B.emit_opaque fw
  in
  A.check A.(list string) "same type" expected actual

let test_function_pointer () =
  let expected =
    {|
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
    |> String.split_on_char '\n' |> List.map String.trim
    |> List.filter (( <> ) "")
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
    |> parse_xml |> S.select_one "function" |> Option.get |> B.func_type
    |> String.split_on_char '\n' |> List.map String.trim
    |> List.filter (( <> ) "")
    |> List.map String.trim |> String.concat " "
  in
  A.check A.string "alias" expected actual

let test_struct () =
  let expected =
    [
      "type t = [`CGPoint] structure";
      "(** Apple docs: \
       {{:https://developer.apple.com/documentation/coregraphics/cgpoint?language=objc}CGPoint} \
       *)\n";
      "let t : t typ = structure \"CGPoint\"";
      "let x_field = field t \"x\" double";
      "let y_field = field t \"y\" double";
      "\nlet () = seal t\n";
      "let init";
      "    ~x";
      "    ~y";
      "  =";
      "  let t = make t in";
      "  setf t x_field x;";
      "  setf t y_field y;";
      "  t\n";
      "let x t = getf t x_field";
      "let y t = getf t y_field";
      "let setX t = setf t x_field";
      "let setY t = setf t y_field";
    ]
  and actual =
    "<struct name='CGPoint' type64='{CGPoint=&quot;x&quot;d&quot;y&quot;d}'/>"
    |> S.parse |> S.select_one "struct" |> Option.get |> B.emit_struct fw |> snd
  in
  A.check A.(list string) "same list" expected actual

let test_struct_init () =
  let expected =
    [
      "type t = [`CFXMLExternalID] structure";
      "(** Apple docs: \
       {{:https://developer.apple.com/documentation/coregraphics/cfxmlexternalid?language=objc}CFXMLExternalID} \
       *)\n";
      "let t : t typ = structure \"CFXMLExternalID\"";
      "let systemID_field = field t \"systemID\" (ptr CFURL.t)";
      "let publicID_field = field t \"publicID\" (ptr CFString.t)";
      "\nlet () = seal t\n";
      "let init";
      "    ?(systemID = from_voidp CFURL.t null)";
      "    ?(publicID = from_voidp CFString.t null)";
      "    ()\n  =";
      "  let t = make t in";
      "  setf t systemID_field systemID;";
      "  setf t publicID_field publicID;";
      "  t\n";
      "let systemID t = getf t systemID_field";
      "let publicID t = getf t publicID_field";
      "let setSystemID t = setf t systemID_field";
      "let setPublicID t = setf t publicID_field";
    ]
  and actual =
    "<struct name='CFXMLExternalID' \
     type64='{CFXMLExternalID=&quot;systemID&quot;^{__CFURL}&quot;publicID&quot;^{__CFString}}'/>"
    |> S.parse |> S.select_one "struct" |> Option.get |> B.emit_struct fw |> snd
  in
  A.check A.(list string) "same list" expected actual

let test_inline_function () =
  let expected =
    "CFRange Camlkit_CFRangeMake(long long x0, long long x1) { return \
     CFRangeMake(x0, x1); }" |> String.split_on_char '\n'
    |> List.map String.trim
    |> List.filter (( <> ) "")
    |> String.concat " "
  and actual =
    {|
    <function inline='true' name='CFRangeMake'>
    <arg type64='q'/>
    <arg type64='q'/>
    <retval type64='{_CFRange=qq}'/>
    </function>
    |}
    |> parse_xml |> S.select_one "function" |> Option.get
    |> fun x ->
    let name = Option.get (S.attribute "name" x)
    and args =
      B.select_children "arg" x
      |> List.map (fun arg -> S.attribute "type64" arg |> Option.get)
    and retval =
      B.select_first "retval" x |> Option.get |> S.attribute "type64"
      |> Option.get
    in
    B.emit_inline_stub (name, args, retval) |> Util.normalize_whitespace
  in
  A.check A.string "alias" expected actual

let test_inline_function_2 () =
  let expected =
    "unsigned short \
     Camlkit_CFStringGetCharacterFromInlineBuffer(CFStringInlineBuffer* x0, \
     long long x1) { return CFStringGetCharacterFromInlineBuffer(x0, x1); }"
    |> String.split_on_char '\n' |> List.map String.trim
    |> List.filter (( <> ) "")
    |> String.concat " "
  and actual =
    {|
    <function inline='true' name='CFStringGetCharacterFromInlineBuffer'>
    <arg type64='^{CFStringInlineBuffer=[64S]^{__CFString}^S*{CFRange=qq}qq}' type_modifier='n'/>
    <arg type64='q'/>
    <retval type64='S'/>
    </function>
    |}
    |> parse_xml |> S.select_one "function" |> Option.get
    |> fun x ->
    let name = Option.get (S.attribute "name" x)
    and args =
      B.select_children "arg" x
      |> List.map (fun arg -> S.attribute "type64" arg |> Option.get)
    and retval =
      B.select_first "retval" x |> Option.get |> S.attribute "type64"
      |> Option.get
    in
    B.emit_inline_stub (name, args, retval) |> Util.normalize_whitespace
  in
  A.check A.string "alias" expected actual

let suite =
  [
    ("test_CGAffineTransform", `Quick, test_emit_CGAffineTransform);
    ("test_emit_CGPDFArray", `Quick, test_emit_CGPDFArray);
    ("test_emit_CGPDFString", `Quick, test_emit_CGPDFString);
    ("test_function_pointer", `Quick, test_function_pointer);
    ("test_struct", `Quick, test_struct);
    ("test_struct_init", `Quick, test_struct_init);
    ("test_inline_function", `Quick, test_inline_function);
    ("test_inline_function_2", `Quick, test_inline_function_2);
  ]

let () = A.run "Bridgesupport" [ ("emit", suite) ]
