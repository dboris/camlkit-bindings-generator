open Lib
open Util
open Bridgesupport
module M = Markup
module S = Soup

let fw_name = ref ""
let open_modules = ref ""

let emit ~open_modules fw x =
  match S.name x with
  | "struct" ->
      let name, lines = emit_struct fw x in
      let file = open_out (Printf.sprintf "data/%s/%s.ml" fw name) in
      emit_prelude ~open_modules file;
      lines |> List.iter (Printf.fprintf file "%s\n");
      close_out file
  | "constant" -> emit_const x
  | "enum" -> emit_enum x
  | "function" -> emit_func fw x
  | "opaque" -> emit_opaque fw x |> List.iter print_endline
  | "cftype" -> emit_cftype ~open_modules fw x
  | _n ->
      (* Printf.eprintf "Not emiting %s\n" n *)
      ()

let usage = "bs-to-ml -fw <framework-name> < <FW.bridgesupport>"

let speclist =
  [
    ("-fw", Arg.Set_string fw_name, "Framework name");
    ( "-open",
      Arg.Set_string open_modules,
      "Comma-separated list of modules to open in generated code" );
  ]

let emit_funcs_prelude file fw =
  match String.lowercase_ascii fw with
  | "corefoundation" -> Printf.fprintf file "open CoreFoundation_globals\n\n"
  | "coregraphics" -> Printf.fprintf file "open CoreGraphics_globals\n\n"
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
      let filename = Printf.sprintf "data/%s/%s_fn.ml" fw fw
      and to_globals = List.length lines < 200 in
      let file = if to_globals then stdout else open_out filename in
      if not to_globals then emit_prelude ~open_modules file;
      emit_funcs_prelude file fw;
      lines |> List.iter (Printf.fprintf file "%s\n");
      close_out file;

      emit_inlines fw |> function
      | [] -> ()
      | lines ->
          Printf.fprintf file
            {|(* Ensure inline function wrappers object file gets linked *)
external _ensure_inlines_object_file_linked : unit -> unit = "_ensure_inlines_object_file_linked"

|};
          let filename = Printf.sprintf "data/%s/%s_inlines.c" fw fw in
          let file = open_out filename in
          lines |> List.iter (Printf.fprintf file "%s\n");
          close_out file)

let () = main ()
