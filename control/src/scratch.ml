open Owl

(* let dir = Cmdargs.(get_string "-d" |> default "results") *)
let dir = Cmdargs.(get_string "-d" |> force ~usage:"-d [directory]")
let in_dir file = Printf.sprintf "%s/%s" dir file
let tau = 20E-3

(* tau dx/dt = -x + x W^T + ... *)
let a =
  let w = Mat.load_txt "data/soc.txt" in
  Mat.((w - eye (row_num w)) /$ tau)


let n, _ = Mat.shape a

let simulate =
  let at = Mat.transpose a in
  fun ~duration x0 ->
    let open Owl_ode in
    let f x _ = Mat.(x *@ at) in
    let tspec = Types.T1 { t0 = 0.; duration; dt = 1E-3 } in
    Ode.odeint (module Native.D.RK45) f x0 tspec ()


let _ =
  let x0 =
    (* lyapunov A B is the solution to AX + XA^T = B *)
    let q = Linalg.D.lyapunov (Mat.transpose a) Mat.(neg (eye n)) in
    let u, _, _ = Linalg.D.svd q in
    (* Q = U S V^T *)
    Mat.col u 0 |> Mat.transpose
  in
  let t, x = simulate ~duration:0.2 x0 in
  Mat.save_txt Mat.(t @|| x) (in_dir "response")


let _ =
  let q = Linalg.D.lyapunov (Mat.transpose a) Mat.(neg (eye n)) in
  let u, s, _ = Linalg.D.svd q in
  let energies =
    u
    |> Mat.transpose
    |> Mat.mapi_rows (fun i x0 ->
           Printf.printf "%i\n%!" i;
           let t, x = simulate ~duration:0.2 x0 in
           let x2 = Mat.l2norm_sqr ~axis:1 x in
           let dts =
             let t2 = Mat.get_slice [ [ 1; -1 ] ] t in
             let t1 = Mat.get_slice [ [ 0; -2 ] ] t in
             Mat.(t2 - t1)
           in
           let x2 = Mat.get_slice [ [ 0; -2 ] ] x2 in
           Mat.(sum' (x2 * dts)))
    (* float array *)
    |> fun v -> Mat.of_array v (-1) 1
  in
  Mat.(save_txt (transpose s @|| energies) (in_dir "comparison"))
