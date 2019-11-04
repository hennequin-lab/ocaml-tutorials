open Owl

module Make (P : sig
  val tau : float
  val w_file : string
end) =
struct
  let a =
    let w = Mat.load_txt P.w_file in
    Mat.((w - eye (row_num w)) /$ P.tau)


  let n, _ = Mat.shape a
  let at = Mat.transpose a

  let simulate ~duration x0 =
    let open Owl_ode in
    let f x _ = Mat.(x *@ at) in
    let tspec = Types.T1 { t0 = 0.; duration; dt = 1E-3 } in
    Ode.odeint (module Native.D.RK45) f x0 tspec ()


  let q = Linalg.D.lyapunov (Mat.transpose a) Mat.(neg (eye n))
  let u, obs_svals, _ = Linalg.D.svd q
  let obs_mode i = Mat.col u i |> Mat.transpose
end
