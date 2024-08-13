open Camllox.Scanner

let () =
  let res = scan_tokens ">=()>>>>><<()\"HEJ\"" in
  print_tokens res
