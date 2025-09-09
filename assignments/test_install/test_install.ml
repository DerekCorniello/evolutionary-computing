open Util

let () =
  Printf.printf "If you see this message, your environment is installed correctly!\n";
  Printf.printf "Square of 5 is %d\n" (Test_funcs.square 5);
  Printf.printf "%s\n" (Test_funcs.greet "Professor")
