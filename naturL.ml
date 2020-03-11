open Dependencies.Translation

let read_file filename =
  let str = ref "" in
  let chan = open_in filename in
  (try while true do str := !str ^ "\n" ^ input_line chan done
   with End_of_file -> close_in chan);
  !str

let write_file filename text =
  let chan = open_out filename in
  Printf.fprintf chan "%s" text

let () =
  if Array.length Sys.argv < 2 then
    failwith "Not enough arguments"
  else if Array.length Sys.argv > 3 then
    failwith "Too many arguments."
  else if Filename.extension Sys.argv.(1) <> ".ntl" then
    failwith ("Can not read " ^ (Filename.extension Sys.argv.(1)) ^ " files")
  else
    let output_name = if Array.length Sys.argv = 3 then Sys.argv.(2)
      else Filename.remove_extension Sys.argv.(1) ^ ".py" in
    let input = read_file Sys.argv.(1) in
    let output = translate_code input in
    write_file output_name output
