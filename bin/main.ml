open Camllox.Scanner
let () =
  let res = scan_tokens "(" in
  print_tokens res
