open Owl

let dir = Cmdargs.(get_string "-d" |> force ~usage:"-d [directory]")
let in_dir file = Printf.sprintf "%s/%s" dir file
