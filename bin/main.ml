open Camllox.Scanner

let () =
  (* scan_tokens ">=()>>>>><<()\"HEJ\"" |> print_tokens; *)
  scan_tokens "//<><>\n(" |> print_tokens
