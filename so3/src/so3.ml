open Owl

let dir = Cmdargs.(get_string "-d" |> force ~usage:"-d [directory]")
let in_dir file = Printf.sprintf "%s/%s" dir file
let qs = Array.init 1000 (fun _ -> Quaternions.sample ())

let _ =
  qs
  |> Array.map Quaternions.to3D
  |> Mat.concatenate ~axis:0
  |> fun m -> Mat.save_txt m (in_dir "qs")


let _ =
  Array.init 100 (fun _ ->
      let q1 = Quaternions.sample () in
      Array.init 10000 (fun _ ->
          let q2 = Quaternions.sample () in
          Quaternions.geodesic_distance q1 q2)
      |> fun v -> [| Stats.mean v; Stats.sem v |])
  |> Mat.of_arrays
  |> fun m -> Mat.save_txt m (in_dir "distances")
