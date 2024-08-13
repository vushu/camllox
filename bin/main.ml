open Camllox.Scanner

let () =
  (* scan_tokens ">=()>>>>><<()\"HEJ\"" |> print_tokens; *)
  scan_tokens "//<><>\n*()3223.32 a23" |> print_tokens
