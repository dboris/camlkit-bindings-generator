open Lib

let fw_name = ref ""
let open_modules = ref ""
let verbose = ref false
let usage = "bs-to-ml -fw <framework-name> < <FW.bridgesupport>"

let speclist =
  [
    ("-fw", Arg.Set_string fw_name, "Framework name");
    ("-v", Arg.Set verbose, "Verbose error output");
    ( "-open",
      Arg.Set_string open_modules,
      "Comma-separated list of modules to open in generated code" );
  ]

let () =
  Arg.parse speclist ignore usage;
  let verbose = !verbose
  and fw = !fw_name
  and open_modules = Util.open_modules !open_modules in

  if verbose then Printexc.record_backtrace true;
  try Bind_c.emit_globals ~open_modules ~fw ~verbose
  with e ->
    if verbose then (
      Printf.eprintf "Main: error: %s\n%!" (Printexc.to_string e);
      Printexc.print_backtrace stderr);
    raise e
