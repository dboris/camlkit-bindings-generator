open Cmdliner
(* open Cmdliner.Term.Syntax *)

let cmd_ckgen =
  let default = Term.(ret (const (`Help (`Auto, None))))
  (* show help *)
  and info = Cmd.info "ckgen" ~doc:"Generate Camlkit Cocoa bindings" in
  Cmd.group info ~default
  @@ [ Cmd_globals.cmd; Cmd_classes.cmd; Cmd_inspect.cmd ]

let main () = Cmd.eval cmd_ckgen
let () = if !Sys.interactive then () else exit (main ())
