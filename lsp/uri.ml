let read_uri uri =
  if Str.string_match (Str.regexp "file://\\(.*\\)") uri 0 then
    let filename = Str.matched_group 1 uri in
    if filename <> "" && Sys.file_exists filename then
      Src__Utils.read_file filename
    else
      raise Not_found
  else
    raise Not_found
