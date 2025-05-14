open Lib
open Util
open Bridgesupport
open Printf
module M = Markup
module S = Soup

let fw_name = ref ""
let open_modules = ref ""
let verbose = ref false

let emit ~open_modules fw x =
  let verbose = !verbose in
  try
    match S.name x with
    | "struct" ->
        let name, lines = emit_struct ~verbose fw x in
        let file = open_out (sprintf "data/%s/%s.ml" fw name) in
        emit_prelude ~open_modules file;
        lines |> List.iter (fprintf file "%s\n");
        close_out file
    | "constant" -> emit_const ~verbose x
    | "enum" -> emit_enum x
    | "function" -> emit_func ~verbose fw x
    | "opaque" -> emit_opaque ~verbose fw x |> List.iter print_endline
    | "cftype" -> emit_cftype ~verbose ~open_modules fw x
    | n -> if verbose then eprintf "Not emiting %s\n" n else ()
  with _ -> ()

let usage = "bs-to-ml -fw <framework-name> < <FW.bridgesupport>"

let speclist =
  [
    ("-fw", Arg.Set_string fw_name, "Framework name");
    ("-v", Arg.Set verbose, "Verbose error output");
    ( "-open",
      Arg.Set_string open_modules,
      "Comma-separated list of modules to open in generated code" );
  ]

let emit_funcs_prelude file fw =
  match String.lowercase_ascii fw with
  | "corefoundation" -> fprintf file "open CoreFoundation_globals\n\n"
  | "coregraphics" -> fprintf file "open CoreGraphics_globals\n\n"
  | _ -> ()

let main () =
  Arg.parse speclist ignore usage;
  let fw = !fw_name and open_modules = Util.open_modules !open_modules in
  emit_prelude ~open_modules stdout;

  M.channel stdin |> M.parse_xml |> M.signals |> S.from_signals
  |> S.select_one "signatures"
  |> Option.iter (fun x ->
         S.children x |> S.elements |> S.iter (emit ~open_modules fw));

  emit_funcs () |> function
  | [] -> ()
  | lines -> (
      let filename = sprintf "data/%s/%s_fn.ml" fw fw
      and to_globals = List.length lines < 200 in
      let file = if to_globals then stdout else open_out filename in
      if not to_globals then emit_prelude ~open_modules file;
      emit_funcs_prelude file fw;
      lines |> List.iter (fprintf file "%s\n");
      close_out file;

      emit_inlines fw |> function
      | [] -> ()
      | lines ->
          fprintf file
            {|(* Ensure inline function wrappers object file gets linked *)
external _ensure_inlines_object_file_linked : unit -> unit = "_ensure_inlines_object_file_linked"

|};
          let filename = sprintf "data/%s/%s_inlines.c" fw fw in
          let file = open_out filename in
          lines |> List.iter (fprintf file "%s\n");
          close_out file)

let () =
  if !verbose then Printexc.record_backtrace true;
  try main ()
  with e ->
    if !verbose then (
      eprintf "Main: error: %s\n%!" (Printexc.to_string e);
      Printexc.print_backtrace stderr);
    raise e
