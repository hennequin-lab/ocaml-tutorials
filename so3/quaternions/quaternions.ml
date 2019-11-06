open Owl

type t = float * Mat.mat

let create theta u =
  (* make sure u is normalised *)
  let u = Mat.(u /$ l2norm' u) in
  let c = Maths.cos (theta /. 2.) in
  let s = Maths.sin (theta /. 2.) in
  c, Mat.(s $* u)


let dot_prod (a1, u1) (a2, u2) = (a1 *. a2) +. Mat.(sum' (u1 * u2))

let ( * ) (a1, u1) (a2, u2) =
  let b1 = Mat.get u1 0 0 in
  let c1 = Mat.get u1 0 1 in
  let d1 = Mat.get u1 0 2 in
  let b2 = Mat.get u2 0 0 in
  let c2 = Mat.get u2 0 1 in
  let d2 = Mat.get u2 0 2 in
  let a = (a1 *. a2) -. (b1 *. b2) -. (c1 *. c2) -. (d1 *. d2) in
  let u =
    Mat.of_array
      [| (a1 *. b2) +. (b1 *. a2) +. (c1 *. d2) -. (d1 *. c2)
       ; (a1 *. c2) -. (b1 *. d2) +. (c1 *. a2) +. (d1 *. b2)
       ; (a1 *. d2) +. (b1 *. c2) -. (c1 *. b2) +. (d1 *. a2)
      |]
      1
      (-1)
  in
  a, u


let geodesic_distance q1 q2 = Maths.(acos ((2. *. sqr (dot_prod q1 q2)) -. 1.))

(* place a quaternion inside the 3D ball used to visualise them *)
let to3D (a, u) =
  let theta = 2. *. Maths.acos a in
  let u = Mat.(u /$ Maths.(sin (theta /. 2.))) in
  Mat.(theta $* u)


(* draw from some arbitrary distribution [p] with support on the interval [a; b] *)
let rec draw (a, b) p =
  let z = Stats.uniform_rvs ~a ~b in
  if Stats.uniform_rvs ~a:0. ~b:1. < p z then z else draw (a, b) p


(* sample quaternions from a geodesic-uniform distribution on SO(3) *)
let sample () =
  let theta = draw (0., Const.pi2) (fun _ -> 1.) in
  let phi = draw (-.Const.pi /. 2., Const.pi /. 2.) (fun phi -> Maths.cos phi) in
  let r = draw (0., Const.pi) (fun r -> Maths.(sqr (sin (r /. 2.)))) in
  let u =
    Mat.of_array [| cos phi *. cos theta; cos phi *. sin theta; sin phi |] 1 (-1)
  in
  create r u
