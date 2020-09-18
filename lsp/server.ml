let setup_server fx addr port =
  let open Unix in
  let sockaddr = ADDR_INET (addr, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in
  bind sock sockaddr;
  listen sock 1;  (* An LSP sevrver only accepts one client *)
  let fd, _ = accept sock in
  (* Low level UNIX file descriptor to high level channels *)
  let ic = in_channel_of_descr fd
  and oc = out_channel_of_descr fd in
  fx ic oc

let () =
  let port = 9131
  and address = Unix.inet_addr_loopback in
  setup_server Listener.start address port
