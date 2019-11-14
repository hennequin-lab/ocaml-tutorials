open Owl
module Q = Quaternions

let dir = Cmdargs.(get_string "-d" |> force ~usage:"-d [directory]")
let in_dir file = Printf.sprintf "%s/%s" dir file
let qs = Array.init 1000 (fun _ -> Q.sample ())
let qs3D = Array.map Q.to3D qs |> Mat.concatenate ~axis:0
let _ = Mat.save_txt qs3D (in_dir "qs")

let make_connectivity ~qs j0 j2 =
  let w =
    Array.map
      (fun qi -> Array.map (fun qj -> -.j0 +. (j2 *. Q.cos_geodesic_distance qi qj)) qs)
      qs
  in
  Mat.(of_arrays w /$ Float.of_int (Array.length qs))


let input ~qs ~epsilon ~contrast q =
  Array.map
    (fun qi -> contrast *. (1. -. epsilon +. (epsilon *. Q.cos_geodesic_distance q qi)))
    qs
  |> fun m -> Mat.of_array m 1 (-1)


let simulate ?(beta = 0.1) ?(threshold = 1.) ~w ~input duration =
  let dmdt m _ =
    let h = Mat.((m *@ w) + input) in
    let m_inf = Mat.map (fun h -> max 0. (min 1. (beta *. (h -. threshold)))) h in
    Mat.(m_inf - m)
  in
  let open Owl_ode in
  let solver = Owl_ode_sundials.cvode ~stiff:false ~relative_tol:1E-5 ~abs_tol:1E-5 in
  (* let solver = Owl_ode.Native.D.euler in *)
  let tspec = Owl_ode.Types.(T1 { t0 = 0.; duration; dt = 0.1 }) in
  let m0 = Mat.zeros 1 (Mat.row_num w) in
  let _ =
    let dm0 = dmdt m0 0. in
    Mat.(save_txt (qs3D @|| transpose dm0) (in_dir "dm0"))
  in
  Ode.odeint solver dmdt m0 tspec ()


let plot_on_so3 ~max_col x filename =
  let data = Mat.(qs3D @|| transpose x) in
  let open Gp in
  let figure (module P : Plot) =
    P.splot
      (A data)
      ~using:"1:2:3:4"
      ~style:"p pt 7 lc palette"
      (default_props @ [ unset "border"; unset "tics"; cbrange (0., max_col) ])
  in
  draw ~output:(png ~size:(640, 640) filename) figure


let _ =
  let j0 = 86.
  and j2 = 112. in
  let w = make_connectivity ~qs j0 j2 in
  let input = input ~qs ~epsilon:0.01 ~contrast:1.678 (Q.sample ()) in
  let _, activity = simulate ~w ~input 30. in
  let max_col = Mat.max' activity in
  Mat.iteri_rows
    (fun i x -> plot_on_so3 ~max_col x (in_dir (Printf.sprintf "response%05i" i)))
    activity

(*
let _ =
  Array.init 100 (fun _ ->
      let q1 = Quaternions.sample () in
      Array.init 10000 (fun _ ->
          let q2 = Quaternions.sample () in
          Quaternions.geodesic_distance q1 q2)
      |> fun v -> [| Stats.mean v; Stats.sem v |])
  |> Mat.of_arrays
  |> fun m -> Mat.save_txt m (in_dir "distances")
*)

(*
let _ =
  let open Quaternions in
  let qstar = sample () in
  let target_p q = Maths.(exp (3. *. neg (sqr (geodesic_distance qstar q)))) in
  let rec draw () =
    let q = sample () in
    if Stats.uniform_rvs ~a:0. ~b:1. < target_p q then q else draw ()
  in
  Array.init 1000 (fun _ ->
      let q = draw () in
      let q3d = to3D q in
      let color = Mat.create 1 1 (target_p q) in
      Mat.(q3d @|| color))
  |> Mat.concatenate ~axis:0
  |> fun m -> Mat.save_txt m (in_dir "target_distribution")
*)
