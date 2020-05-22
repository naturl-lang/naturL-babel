open Src.Warnings
open Src.Translation
open Internationalisation.Translation

let source = ref ""

let map_option o f default = match o with
  | None -> default ()
  | Some v -> f v

let read_input chan_name =
  let chan = map_option chan_name (fun name -> open_in name) (fun () -> Sys.catch_break true; stdin) in
  try
    (while true do
       source := !source ^ "\n" ^ input_line chan;
     done)
  with End_of_file -> close_in chan

let write_translation chan_name filename text =
  let chan = map_option chan_name (fun name -> open_out name) (fun () -> stdout) in
  Printf.fprintf chan "%s" (translate_code filename text)


let input_name = ref ""
let output_name = ref ""
let warning_severity = ref 0


let usage = "usage: " ^ Sys.argv.(0) ^ " [options]"

let speclist = [
  "--input", Arg.Set_string input_name, "The file that should be read. Default is stdin";
  "--output", Arg.Set_string output_name, "The file where the output should be printed. Default is stdout";
  "--warning", Arg.Set_int warning_severity, "The minimum severity of the warnings. Default is 0 (all warnings)";
  "--debug", Arg.Set Src.Global.debug, "Display debug infos";
  "--language", Arg.Symbol (["french"; "english"], set_lang_of_string), " This option determines the language of the error messages.";
  "--import-mode", Arg.Symbol (["write-nothing"; "moderated"; "overwrite"], Src.Global.set_import_mode),
  " Specify when imported .py files need to be generated.";
]


let () =
  Arg.parse
    speclist
    (fun arg -> raise (Arg.Bad ("unknown argument '" ^ arg ^ "'")))
    usage;
  (try read_input (match !input_name with "" -> None | name -> Some name)
   with Sys.Break -> print_newline());
  write_translation (match !output_name with "" -> None | name -> Some name) (if !input_name = "" then "<stdin>" else !input_name) !source;
  if !output_name = "" then print_newline ();
  print_warnings ~severity: !warning_severity
