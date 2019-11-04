open Owl
open Ctrl
open Dynamics

let dir = Cmdargs.(get_string "-d" |> force ~usage:"-d [directory]")
let in_dir file = Printf.sprintf "%s/%s" dir file

module D = Dynamics.Make (struct
  let tau = 20E-3
  let w_file = "data/soc.txt"
end)

let _ =
  let x0 = D.obs_mode 0 in
  let t, x = D.simulate ~duration:0.2 x0 in
  Mat.save_txt Mat.(t @|| x) (in_dir "response")


(* minimize  || Q - vv^T ||_F^2 *)

module AD = Algodiff.D

module Prms = struct
  type 'a t = { v : 'a } [@@deriving prms]
end

module O = Owl_opt_lbfgs.Make (Prms)

let cost prms =
  let v = Prms.(prms.v) in
  AD.Maths.(l2norm_sqr' (Arr D.q - (transpose v *@ v)))


let opt_prms =
  let prms0 = Prms.{ v = AD.Mat.gaussian 1 D.n } in
  let s0 = O.init ~prms0 ~f:cost () in
  let stop s = O.iter s > 1000 in
  let s = O.min ~pgtol:0. ~stop s0 in
  Prms.(O.prms s).v |> AD.unpack_arr


let _ =
  let s0 = sqrt (Mat.get D.obs_svals 0 0) in
  let true_v = Mat.(s0 $* D.obs_mode 0) |> Mat.transpose in
  let v = opt_prms |> Mat.transpose in
  Mat.(save_txt (true_v @|| v) (in_dir "comparison"))

(*

let _ =
  let energies =
    Array.init D.n (fun i ->
        Printf.printf "%i\n%!" i;
        let t, x = D.simulate ~duration:0.2 (D.obs_mode i) in
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
  Mat.(save_txt (transpose D.obs_svals @|| energies) (in_dir "comparison"))
*)
