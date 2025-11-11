(* Module for handling output redirection to both console and file *)

type output_destination =
  | Console
  | File of string
  | FileOverwrite of string
  | Both of string
  | FileHandle of out_channel
  | BothHandle of string * out_channel

let current_dest = ref (Console)
let current_handle = ref None

let ensure_txt_extension filename =
  if Filename.extension filename = ".txt" then filename else filename ^ ".txt"

let set_output = function
  | "console" -> current_dest := Console
  | filename -> current_dest := File (ensure_txt_extension filename)

let set_output_overwrite = function
  | "console" -> current_dest := Console
  | filename -> current_dest := FileOverwrite (ensure_txt_extension filename)

let set_both_output filename =
  current_dest := Both (ensure_txt_extension filename)

let with_output_file filename f =
  let filename = ensure_txt_extension filename in
  let oc = open_out_gen [Open_append; Open_creat; Open_wronly] 0o666 filename in
  try
    let result = f oc in
    close_out oc;
    result
  with e ->
    close_out_noerr oc;
    raise e

let with_output_file_overwrite filename f =
  let filename = ensure_txt_extension filename in
  let oc = open_out filename in
  try
    let result = f oc in
    close_out oc;
    result
  with e ->
    close_out_noerr oc;
    raise e

let open_output filename =
  let filename = ensure_txt_extension filename in
  match !current_dest with
  | File _ -> 
      let oc = open_out_gen [Open_append; Open_creat; Open_wronly] 0o666 filename in
      current_handle := Some oc;
      current_dest := FileHandle oc;
      oc
  | FileOverwrite _ ->
      let oc = open_out filename in
      current_handle := Some oc;
      current_dest := FileHandle oc;
      oc
  | Both _ ->
      let oc = open_out_gen [Open_append; Open_creat; Open_wronly] 0o666 filename in
      current_handle := Some oc;
      current_dest := BothHandle (filename, oc);
      oc
  | _ -> failwith "Invalid output mode for opening a file handle"

let close_output () =
  match !current_handle with
  | Some oc -> 
      close_out oc;
      current_handle := None;
      (match !current_dest with
       | FileHandle _ -> current_dest := Console
       | BothHandle (filename, _) -> current_dest := Both filename
       | _ -> ())
  | None -> ()

let printf fmt =
  let kprintf k = Printf.ksprintf k fmt in
  match !current_dest with
  | Console -> kprintf (fun s -> Stdlib.print_string s; Stdlib.flush Stdlib.stdout)
  | File filename ->
      kprintf (fun s ->
        with_output_file filename (fun oc -> output_string oc s))
  | FileOverwrite filename ->
      kprintf (fun s ->
        with_output_file_overwrite filename (fun oc -> output_string oc s))
  | FileHandle oc ->
      kprintf (fun s -> output_string oc s; flush oc)
  | Both filename ->
      kprintf (fun s ->
        Stdlib.print_string s; Stdlib.flush Stdlib.stdout;
        with_output_file filename (fun oc -> output_string oc s))
  | BothHandle (_, oc) ->
      kprintf (fun s ->
        Stdlib.print_string s; Stdlib.flush Stdlib.stdout;
        output_string oc s; flush oc)

let print_string s =
  match !current_dest with
  | Console -> Stdlib.print_string s
  | File filename ->
      with_output_file filename (fun oc -> output_string oc s)
  | FileOverwrite filename ->
      with_output_file_overwrite filename (fun oc -> output_string oc s)
  | FileHandle oc ->
      output_string oc s; flush oc
  | Both filename ->
      Stdlib.print_string s;
      with_output_file filename (fun oc -> output_string oc s)
  | BothHandle (_, oc) ->
      Stdlib.print_string s; Stdlib.flush Stdlib.stdout;
      output_string oc s; flush oc

let print_newline () =
  match !current_dest with
  | Console -> Stdlib.print_newline ()
  | File filename ->
      with_output_file filename (fun oc -> output_string oc "\n")
  | FileOverwrite filename ->
      with_output_file_overwrite filename (fun oc -> output_string oc "\n")
  | FileHandle oc ->
      output_string oc "\n"; flush oc
  | Both filename ->
      Stdlib.print_newline ();
      with_output_file filename (fun oc -> output_string oc "\n")
  | BothHandle (_, oc) ->
      Stdlib.print_newline ();
      output_string oc "\n"; flush oc
