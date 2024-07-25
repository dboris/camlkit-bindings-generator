open Runtime
open Lib

let usage = {|
Usage: generate-ml -classes <lib-name> | -methods <class-name>
|}

let fw_name = ref ""
let gen_classes = ref ""
let gen_methods = ref ""
let include_superclass = ref false

let speclist =
  [ ("-classes", Arg.Set_string gen_classes, "Generate classes in <lib>")
  ; ("-methods", Arg.Set_string gen_methods, "Generate methods in <class>")
  ; ("-super", Arg.Set include_superclass, "Include superclass methods in generated module")
  ; ("-fw", Arg.Set_string fw_name, "Framework name")
  ]

let () =
  Arg.parse speclist ignore usage;
  let lib = !gen_classes
  and cls = !gen_methods
  and fw = !fw_name
  and include_superclass = !include_superclass
  in
  if not (String.equal lib "") then
    Inspect.library_class_names lib
    |> List.iter (fun cls ->
      if (
        not (String.starts_with ~prefix:"_" cls)
      ) then
        emit_class_module cls ~fw ~include_superclass)
  else if not (String.equal cls "") then
    emit_class_module cls ~fw ~include_superclass
  else
    print_endline usage
