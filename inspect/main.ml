open Foundation
open Runtime

let usage = {|
Usage: inspect-rt -libs | -classes <lib-name> | -methods <class-name>
|}

let show_libs = ref false
let show_classes = ref ""
let show_methods = ref ""
let load_fw = ref ""

let speclist =
  [ ("-libs", Arg.Set show_libs, "Show loaded frameworks and libraries.")
  ; ("-classes", Arg.Set_string show_classes, "Show classes in <lib>")
  ; ("-methods", Arg.Set_string show_methods, "Show methods in <class>")
  ; ("-load", Arg.Set_string load_fw, "Load framework bundle <fw-path>")
  ]

let load_framework () =
  if not (String.equal !load_fw "") then
    let bundle =
      NSBundle._class_ |> NSBundle.C.bundleWithPath (new_string !load_fw) in
    if is_nil bundle then
      Printf.eprintf "Framework bundle not found!\n%!"
    else
      NSBundle.load bundle |> ignore

let () =
  Arg.parse speclist ignore usage;
  let lib = !show_classes
  and cls = !show_methods
  in
  load_framework ();
  if !show_libs then
    List.iter print_endline (Inspect.loaded_library_names ())
  else if not (String.equal lib "") then
    List.iter print_endline (Inspect.library_class_names lib)
  else if not (String.equal cls "") then
    failwith "TODO"
  else
    print_endline usage