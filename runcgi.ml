(* Runs a command with CGI related envvars set. *)

let () =
  let meth = ref "GET"
  and typ = ref "text/plain"
  and accept = ref ""
  and body = ref ""
  and query = ref ""
  and cmd = ref [] in
  let default d h = h ^" (default: "^ String.escaped d ^")" in
  Arg.(parse
    [ "-method", Set_string meth, default !meth "HTTP method (likely GET or POST)" ;
      "-type", Set_string typ, default !typ "Content type" ;
      "-accept", Set_string accept, default !accept "Accept" ;
      "-body", Set_string body, default !body "Content body" ;
      "-query", Set_string query, default !query "Query string (after the \"?\" in the URL, ex: \"x=1&y=2\")"
      (* TODO: -env to add envvars *) ]
    (fun s -> cmd := s :: !cmd)
    "runcgi -method <meth> -type <type> -accept <types> -body <txt> -query <qry> command ...args...") ;
  let cmd = List.rev !cmd in
  if cmd = [] then (
    Printf.eprintf "Missing command\n" ;
    exit 1
  ) ;
  let cmd = Array.of_list cmd in
  let env =
    [ "CONTENT_LENGTH", string_of_int (String.length !body) ;
      "REQUEST_METHOD", !meth ;
      "CONTENT_TYPE", !typ ;
      "QUERY_STRING", !query ;
      "SERVER_PORT", "80" ;
      "SCRIPT_NAME", cmd.(0) ;
      "SERVER_NAME", "runcgi" ] in
  let add_env_if_set n s env =
    match s with
    | "" -> env
    | s -> (n, s) :: env in
  let add_env n env =
    match Sys.getenv n with
    | exception Not_found -> env
    | s -> (n, s) :: env in
  let env = add_env "HOME" env in
  let env = add_env "PATH" env in
  let env = add_env "OCAMLRUNPARAM" env in
  let env = add_env_if_set "HTTP_ACCEPT" !accept env in
  let env =
    List.map (fun (n, v) -> n ^"="^ v) env |>
    Array.of_list in
  let open Unix in
  if String.length !body = 0 then
    execvpe cmd.(0) cmd env
  else
    let read_fd, write_fd = pipe ~cloexec:false () in
    flush_all () ;
    let pid = fork () in
    if pid = 0 then (
      (* Child process *)
      dup2 read_fd stdin ;
      close write_fd ;
      execvpe cmd.(0) cmd env
    ) else (
      (* Parent process *)
      close read_fd ;
      write_substring write_fd !body 0 (String.length !body) |> ignore ;
      close write_fd ;
      let _, status = waitpid [] pid in
      (* Exit with the same status: *)
      match status with
      | WEXITED s ->
          exit s
      | WSIGNALED s ->
          Printf.eprintf "Killed by signal %d" s
      | WSTOPPED _ ->
          assert false (* SHould not happen given the waitpif args *)
    )
