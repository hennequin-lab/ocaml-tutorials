open Owl

(* let dir = Cmdargs.(get_string "-d" |> default "results") *)
let dir = Cmdargs.(get_string "-d" |> force ~usage:"-d [directory]")
let in_dir file = Printf.sprintf "%s/%s" dir file
let tau = 20E-3

(* tau dx/dt = -x + x W^T + ... *)
let a =
  let w = Mat.load_txt "data/soc.txt" in
  Mat.(w - eye (row_num w))


let n, _ = Mat.shape a

let simulate =
  let at = Mat.transpose a in
  fun ~duration x0 ->
    let open Owl_ode in
    let f x _ = Mat.(x *@ at /$ tau) in
    let tspec = Types.T1 { t0 = 0.; duration; dt = 1E-3 } in
    Ode.odeint (module Native.D.RK45) f x0 tspec ()


let _ =
  let x0 = Mat.gaussian 1 n in
  let t, x = simulate ~duration:0.2 x0 in
  Mat.save_txt Mat.(t @|| x) (in_dir "response")
