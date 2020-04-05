open Src.Translation


let source = ref ""

let map_option o f default = match o with
  | None -> default ()
  | Some v -> f v

let read_input chan_name =
  let chan = map_option chan_name (fun name -> open_in name) (fun () -> Sys.catch_break true; stdin) in
  (try while true do source := !source ^ "\n" ^ input_line chan done
   with End_of_file -> close_in chan)

let write_translation chan_name text =
  let chan = map_option chan_name (fun name -> open_out name) (fun () -> stdout) in
  Printf.fprintf chan "%s" (translate_code text)


let input_name = ref ""
let output_name = ref ""

let usage = "usage: " ^ Sys.argv.(0) ^ " [options]"

let speclist = [
  "--input", Arg.Set_string input_name, "The file that should be read. Default is stdin";
  "--output", Arg.Set_string output_name, "The file where the output should be printed. Default is stdout"
]


let () =
  Arg.parse
    speclist
    (fun arg -> raise (Arg.Bad ("Unknown argument: " ^ arg ^ ".\nSea '" ^ Sys.argv.(0) ^ "' -help.")))
    usage;
  (try read_input (match !input_name with "" -> None | name -> Some name)
   with Sys.Break -> ());
  write_translation (match !output_name with "" -> None | name -> Some name) !source
