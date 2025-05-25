(* TODO *)

type common_opts = { verbose : bool }

open Cmdliner
open Cmdliner.Term.Syntax

let verbose =
  let doc = "Verbose error output." in
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc)
