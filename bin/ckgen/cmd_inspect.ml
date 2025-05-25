open Cmdliner
open Cmdliner.Term.Syntax
open Lib
open Runtime

type show = Libs | Classes of string | Methods of string | Image of string

let libs_term =
  let doc = "Show loaded frameworks and libraries." in
  Arg.(value & flag & info [ "libs" ] ~doc)

let classes_term =
  let doc = "Show classes in LIB." in
  Arg.(
    value & opt (some string) None & info [ "c"; "classes-in" ] ~docv:"LIB" ~doc)

let methods_term =
  let doc = "Show methods in CLASS." in
  Arg.(
    value
    & opt (some string) None
    & info [ "m"; "methods-in" ] ~docv:"CLASS" ~doc)

let image_term =
  let doc = "Show library image where CLASS is defined." in
  Arg.(
    value
    & opt (some string) None
    & info [ "i"; "image-for" ] ~docv:"CLASS" ~doc)

let load_term =
  let doc = "Load framework bundle specified by FW_PATH." in
  Arg.(
    value & opt (some string) None & info [ "l"; "load" ] ~docv:"FW_PATH" ~doc)

let show_term =
  let make_show libs classes methods image =
    match (libs, classes, methods, image) with
    | true, None, None, None -> Ok Libs
    | false, Some lib, None, None -> Ok (Classes lib)
    | false, None, Some cls, None -> Ok (Methods cls)
    | false, None, None, Some cls -> Ok (Image cls)
    | _ ->
        Error
          (`Msg
             "You must specify exactly one of: --libs | --classes-in LIB | \
              --methods-in CLASS | --image-for CLASS")
  in
  Term.(
    term_result
      (const make_show $ libs_term $ classes_term $ methods_term $ image_term))

let inspect ~load show =
  Option.iter Util.load_framework load;
  match show with
  | Libs -> List.iter print_endline (Inspect.loaded_library_names ())
  | Classes lib -> List.iter print_endline (Inspect.library_class_names lib)
  | Methods cls ->
      List.iter print_endline (Inspect.method_names (Objc.get_class cls))
  | Image cls -> print_endline (Util.class_image cls)

let cmd =
  let doc =
    "Inspect the Objective-C runtime for loaded frameworks, classes, methods"
  in
  Cmd.v (Cmd.info "inspect" ~doc)
  @@
  let+ show = show_term and+ load = load_term in
  inspect show ~load
