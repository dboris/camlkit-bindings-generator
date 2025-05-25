open Cmdliner
open Cmdliner.Term.Syntax
open Lib

let fw_term =
  let doc = "Framework name of the generated bindings." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"FW_NAME")

let open_term =
  let doc = "Comma-separated list of modules to open in generated code." in
  Arg.(
    value
    & opt (some string) None
    & info [ "o"; "open-modules" ] ~docv:"M1,M2,..." ~doc)

let verbose_term =
  let doc = "Verbose error output." in
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc)

let cmd =
  let doc =
    {|Generate bindings for C globals using the framework "bridgesupport" file|}
  in
  Cmd.v (Cmd.info "globals" ~doc)
  @@
  let+ fw = fw_term and+ open_modules = open_term and+ verbose = verbose_term in
  let open_modules =
    Option.fold ~none:[] ~some:Util.open_modules open_modules
  in
  if verbose then Printexc.record_backtrace true;
  try Bind_c.emit_globals ~open_modules ~fw ~verbose
  with e ->
    if verbose then (
      Printf.eprintf "Main: error: %s\n%!" (Printexc.to_string e);
      Printexc.print_backtrace stderr);
    raise e
