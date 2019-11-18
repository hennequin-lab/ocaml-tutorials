open Owl

(**
Here is an example of how to use this library:

{[
let qs = Array.init 1000 (fun _ -> Q.sample ())
let qs3D = Array.map Q.to3D qs |> Mat.concatenate ~axis:0
let _ = Mat.save_txt qs3D (in_dir "qs")
]}

*)

type t

(** [create theta u] makes a unit quaternion; [u] is first normalised so only its direction matters.
    @return a fresh quaternion *)
val create : float -> Mat.mat -> t

val dot_prod : t -> t -> float
val ( * ) : t -> t -> t

(** Geodesic distance on SO(3) *)
val cos_geodesic_distance : t -> t -> float

val geodesic_distance : t -> t -> float

(** place a quaternion inside the 3D ball used to visualise them *)
val to3D : t -> Mat.mat

(** sample quaternions from a geodesic-uniform distribution on SO(3);
    this function calls {!create} under the hood. *)
val sample : unit -> t
