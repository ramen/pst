#!/usr/bin/ocaml
#load "unix.cma";;

type proc = { cmd  : string;
              pid  : int;
              ppid : int;
              uid  : int;
              args : string list; }

let escape cmd =
  let len = String.length cmd in
  let buf = Buffer.create (len + 8) in
  for i = 0 to len - 1 do
    let code = Char.code cmd.[i] in
    if cmd.[i] = '\\'
    then Buffer.add_string buf "\\\\"
    else if code > Char.code ' ' && code <= Char.code '~'
    then Buffer.add_char buf cmd.[i]
    else Printf.bprintf buf "\\%03o" code
  done;
  Buffer.contents buf

let arg_seq ch =
  let buf = Buffer.create 256 in
  let rec next () =
    match (try Some (input_char ch) with End_of_file -> None) with
      | None when Buffer.length buf = 0 -> None
      | None | Some '\000' ->
          let s = Buffer.contents buf in
          Buffer.clear buf;
          Some (s, ())
      | Some c ->
          Buffer.add_char buf c;
          next () in
  Seq.unfold next ()

let read_args ch =
  List.of_seq (arg_seq ch)

let with_in_channel name f =
  let ch = open_in name in
  try let res = f ch in close_in ch; res
  with e -> close_in ch; raise e

let build_proc pid =
  let dir = "/proc/" ^ string_of_int pid in
  let st = Unix.stat dir in
  let uid = st.Unix.st_uid in
  let args = with_in_channel (dir ^ "/cmdline") read_args in
  let proc_stat = with_in_channel (dir ^ "/stat") input_line in
  let index = String.index proc_stat '(' + 1 in
  let length = String.rindex proc_stat ')' - index in
  let cmd = String.sub proc_stat index length in
  let index = index + length + 4 in
  let length = String.length proc_stat - index in
  Scanf.sscanf (String.sub proc_stat index length) "%d"
    (fun ppid -> {cmd=cmd; args=args; pid=pid; ppid=ppid; uid=uid})

let get_pids dir =
  Array.fold_left
    (fun pids name ->
       try int_of_string name :: pids
       with Failure _ -> pids)
    [] (Sys.readdir dir)

let read_procs () =
  List.rev_map build_proc (get_pids "/proc")

let get_username =
  let cache = Hashtbl.create 32 in
  fun uid ->
    try Hashtbl.find cache uid
    with Not_found ->
      let name = (Unix.getpwuid uid).Unix.pw_name in
      Hashtbl.replace cache uid name;
      name

let show_tree start procs =
  let rec show_children depth more_at_depth parent last_uid =
    let children = List.filter (fun proc -> proc.ppid = parent) procs in
    let depth = depth + 1 in
    match List.rev (List.sort compare children) with
      | [] -> ()
      | last :: rest ->
          List.iter
            (show_proc depth false ((depth, true) :: more_at_depth) last_uid)
            (List.rev rest);
          show_proc depth true ((depth, false) :: more_at_depth) last_uid last
  and show_proc depth last more_at_depth last_uid proc =
    if depth <> 0 then
      begin
        for i = 1 to depth do
          let more = List.assoc i more_at_depth in
          print_string "  ";
          print_string
            (if i = depth
             then (if last then "`-" else "|-")
             else (if more then "| " else "  "))
        done
      end;
    Printf.printf "%s,%d%s%s\n"
      proc.cmd proc.pid
      (if proc.uid <> last_uid
       then "," ^ (get_username proc.uid)
       else "")
      (if List.length proc.args > 1
       then " " ^ String.concat " " (List.map escape (List.tl proc.args))
       else "");
    show_children depth more_at_depth proc.pid proc.uid in
  let root = List.find (fun proc -> proc.pid = start) procs in
  show_proc 0 true [] 0 root

let () = show_tree 1 (read_procs ())
