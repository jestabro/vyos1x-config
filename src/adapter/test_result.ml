(*
 *)
let print_res res =
  if res <> "" then
      Printf.printf "%s\n" res
  else
      Printf.printf "No result\n"

let some_path = ["interfaces"; "ethernet"; "eth0"; "description"; "foo"]

let () =
  let h = Cli.handle_init () in
  let res_set = Cli.set_result h some_path (List.length some_path) in
  print_res res_set;
  Cli.handle_free h
