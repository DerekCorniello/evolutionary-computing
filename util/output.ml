(* Module for handling output redirection to both console and file *)

type output_destination = Console | File of string | Both of string

let current_dest = ref (Console)

let ensure_txt_extension filename =
  if Filename.extension filename = ".txt" then filename else filename ^ ".txt"

let set_output = function
  | "console" -> current_dest := Console
  | filename -> current_dest := File (ensure_txt_extension filename)

let set_both_output filename =
  current_dest := Both (ensure_txt_extension filename)

let with_output_file filename f =
  let filename = ensure_txt_extension filename in
  let oc = open_out_gen [Open_trunc; Open_creat; Open_wronly] 0o666 filename in
  try
    let result = f oc in
    close_out oc;
    result
  with e ->
    close_out_noerr oc;
    raise e

let printf fmt =
  let kprintf k = Printf.ksprintf k fmt in
  match !current_dest with
  | Console -> kprintf (fun s -> print_string s; flush stdout)
  | File filename ->
      kprintf (fun s ->
        with_output_file filename (fun oc -> output_string oc s))
  | Both filename ->
      kprintf (fun s ->
        print_string s; flush stdout;
        with_output_file filename (fun oc -> output_string oc s))

let print_string s =
  match !current_dest with
  | Console -> print_string s
  | File filename ->
      with_output_file filename (fun oc -> output_string oc s)
  | Both filename ->
      print_string s;
      with_output_file filename (fun oc -> output_string oc s)

let print_newline () =
  match !current_dest with
  | Console -> print_newline ()
  | File filename ->
      with_output_file filename (fun oc -> output_string oc "\n")
  | Both filename ->
      print_newline ();
      with_output_file filename (fun oc -> output_string oc "\n")
