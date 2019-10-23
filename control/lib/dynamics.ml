open Owl

let trans = Mat.transpose

let simulate ~a =
  let at = trans a in
  fun ~duration x0 ->
    let open Owl_ode in
    let f x _ = Mat.(x *@ at) in
    let tspec = Types.T1 { t0 = 0.; duration; dt = 1E-3 } in
    Ode.odeint (module Native.D.RK45) f x0 tspec ()
