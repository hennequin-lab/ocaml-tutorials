open Owl

module Make (P : sig
  val tau : float
  val w_file : string
end) : sig
  val n : int
  val a : Mat.mat
  val simulate : duration:float -> Mat.mat -> Mat.mat * Mat.mat
  val q : Mat.mat
  val obs_svals : Mat.mat
  val obs_mode : int -> Mat.mat
end
