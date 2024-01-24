(* CLI set path
 *)

let print_res res =
    if res <> "" then
        Printf.printf "%s\n" res
    else
        Printf.printf "no error\n"

let path_arg = ref []
let args = []
let usage = Printf.sprintf "Usage: %s <path>" Sys.argv.(0)

let () = if Array.length Sys.argv = 1 then (Arg.usage args usage; exit 1)
let () = Arg.parse args (fun s -> path_arg := s::!path_arg) usage

let () =
    let path_set = List.rev !path_arg in
    let h = Vy_adapter.handle_init () in
    if not (Vy_adapter.in_config_session_handle h) then
        (Vy_adapter.handle_free h;
        Printf.printf "not in config session\n")
    else
        let res_set = Vy_adapter.set_path h path_set (List.length path_set) in
        Printf.printf "setting [%s]\n" (String.concat " " (path_set));
        print_res res_set;
        Vy_adapter.handle_free h

