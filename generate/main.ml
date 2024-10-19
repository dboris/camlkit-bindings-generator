open Runtime
open Lib
open Util

let usage = {|
Usage: generate-ml -classes <lib-name> | -methods <class-name>
|}

let fw_name = ref ""
let gen_classes = ref ""
let gen_methods = ref ""
let gen_method_def = ref ""
let gen_protocols = ref false
let include_superclass = ref false
let load_fw = ref ""
let open_modules = ref ""

let speclist =
  [ ("-classes", Arg.Set_string gen_classes, "Generate classes in <lib>")
  ; ("-methods", Arg.Set_string gen_methods, "Generate methods in <class>")
  ; ("-method-def", Arg.Set_string gen_method_def,
      "Generate methods definitions for <class>")
  ; ("-protocols", Arg.Set gen_protocols,
      "Generate protocols registered in the runtime")
  ; ("-super", Arg.Set include_superclass,
      "Include superclass methods in generated module")
  ; ("-fw", Arg.Set_string fw_name, "Framework name <fw-name>")
  ; ("-load", Arg.Set_string load_fw, "Load framework bundle <fw-path>")
  ; ("-open", Arg.Set_string open_modules,
      "Comma-separated list of modules to open in generated code")
  ]

let () =
  Arg.parse speclist ignore usage;
  let lib = !gen_classes
  and cls = !gen_methods
  and method_def_cls = !gen_method_def
  and proto = !gen_protocols
  and fw = !fw_name
  and include_superclass = !include_superclass
  and open_modules = Util.open_modules !open_modules
  in
  Util.load_framework !load_fw;
  if not (String.equal lib "") then
    Inspect.library_class_names lib
    |> List.iter (fun cls ->
      if (
        not (String.starts_with ~prefix:"_" cls) &&
        not (String.member "Internal" cls)
      ) then
        emit_class_module cls ~fw ~include_superclass ~open_modules)
  else if not (String.equal cls "") then
    emit_class_module cls ~fw ~include_superclass ~open_modules
  else if not (String.equal method_def_cls "") then
    emit_class_method_def method_def_cls ~open_modules
  else if proto then
    (* emit_protocols ~open_modules *)
    failwith "Disabled until next release of camlkit"
  else
    print_endline usage
