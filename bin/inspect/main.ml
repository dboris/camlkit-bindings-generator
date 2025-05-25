open Lib
open Runtime

let usage =
  {|
Usage: inspect-rt -libs | -classes <lib-name> | -methods <class-name>
|}

let show_libs = ref false
let show_classes = ref ""
let show_methods = ref ""
let show_image = ref ""
let load_fw = ref ""

let speclist =
  [
    ("-libs", Arg.Set show_libs, "Show loaded frameworks and libraries.");
    ("-classes", Arg.Set_string show_classes, "Show classes in <lib>");
    ("-methods", Arg.Set_string show_methods, "Show methods in <class>");
    ( "-image",
      Arg.Set_string show_image,
      "Show library image where <class> is defined" );
    ("-load", Arg.Set_string load_fw, "Load framework bundle <fw-path>");
  ]

let () =
  Arg.parse speclist ignore usage;
  let lib = !show_classes and cls = !show_methods and cls_img = !show_image in
  Util.load_framework !load_fw;
  if !show_libs then List.iter print_endline (Inspect.loaded_library_names ())
  else if not (String.equal lib "") then
    List.iter print_endline (Inspect.library_class_names lib)
  else if not (String.equal cls_img "") then
    print_endline (Util.class_image cls_img)
  else if not (String.equal cls "") then
    List.iter print_endline (Inspect.method_names (Objc.get_class cls))
  else print_endline usage
