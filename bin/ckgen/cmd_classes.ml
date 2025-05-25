open Cmdliner
open Cmdliner.Term.Syntax
open Lib
open Runtime
open Util
open Bind_objc

type gen =
  | Protocols
  | Classes of string
  | Methods of string
  | Method_defs of string

let proto_term =
  let doc = "Generate protocols registered in the Runtime." in
  Arg.(value & flag & info [ "p"; "proto" ] ~doc)

let classes_term =
  let doc = "Generate bindings for classes in LIB." in
  Arg.(value & opt (some string) None & info [ "a"; "all-in" ] ~docv:"LIB" ~doc)

let methods_term =
  let doc = "Generate bindings for methods of CLASS." in
  Arg.(
    value
    & opt (some string) None
    & info [ "m"; "methods-in" ] ~docv:"CLASS" ~doc)

let method_defs_term =
  let doc = "Generate method definitions of CLASS." in
  Arg.(
    value
    & opt (some string) None
    & info [ "d"; "method-defs-in" ] ~docv:"CLASS" ~doc)

let open_term =
  let doc = "Comma-separated list of modules to open in generated code." in
  Arg.(
    value
    & opt (some string) None
    & info [ "o"; "open-modules" ] ~docv:"M1,M2,..." ~doc)

let filter_term =
  let doc = "Filename with class names to keep, one class name per line." in
  Arg.(
    value
    & opt (some string) None
    & info [ "f"; "filter" ] ~docv:"FILE_NAME" ~doc)

let load_term =
  let doc = "Load framework bundle specified by FW_PATH." in
  Arg.(
    value & opt (some string) None & info [ "l"; "load" ] ~docv:"FW_PATH" ~doc)

let fw_term =
  let doc = "Framework name of the generated bindings." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"FW_NAME")

let super_term =
  let doc = "Include superclass methods in generated module." in
  Arg.(value & flag & info [ "s"; "super" ] ~doc)

let meta_term =
  let doc = "Generate method definitions for the metaclass." in
  Arg.(value & flag & info [ "meta" ] ~doc)

let gen_term =
  let make_gen proto classes methods method_defs =
    match (proto, classes, methods, method_defs) with
    | true, None, None, None -> Ok Protocols
    | false, Some lib, None, None -> Ok (Classes lib)
    | false, None, Some cls, None -> Ok (Methods cls)
    | false, None, None, Some cls -> Ok (Method_defs cls)
    | _ ->
        Error
          (`Msg
             "You must specify exactly one of: --proto | --all-in LIB | \
              --methods-in CLASS | --method-defs-in CLASS")
  in
  Term.(
    term_result
      (const make_gen $ proto_term $ classes_term $ methods_term
     $ method_defs_term))

let generate ~load ~open_modules ~filter_classes ~include_superclass ~metaclass
    gen fw =
  let open_modules = Option.fold ~none:[] ~some:Util.open_modules open_modules
  and keep_class =
    Util.filter_classes_fn (Option.value ~default:"" filter_classes)
  in
  Option.iter Util.load_framework load;
  match gen with
  | Protocols -> emit_protocols ~open_modules
  | Classes lib ->
      Inspect.library_class_names lib
      |> List.iter (fun cls ->
             if
               (not (String.starts_with ~prefix:"_" cls))
               && (not (String.member "Internal" cls))
               && keep_class cls
             then emit_class_module cls ~fw ~include_superclass ~open_modules)
  | Methods cls -> emit_class_module cls ~fw ~include_superclass ~open_modules
  | Method_defs cls -> emit_class_method_def cls ~open_modules ~meta:metaclass

let cmd =
  let doc = "Generate bindings for Objective-C classes and protocols" in
  Cmd.v (Cmd.info "classes" ~doc)
  @@
  let+ gen = gen_term
  and+ load = load_term
  and+ open_modules = open_term
  and+ fw = fw_term
  and+ filter_classes = filter_term
  and+ include_superclass = super_term
  and+ metaclass = meta_term in
  generate ~load ~open_modules ~filter_classes ~include_superclass ~metaclass
    gen fw
