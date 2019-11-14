open Owl

type t

(** [create theta u] makes a unit quaternion; [u] is first normalised so only its direction matters. *)
val create : float -> Mat.mat -> t

val dot_prod : t -> t -> float
val ( * ) : t -> t -> t

(** Geodesic distance on SO(3) *)
val cos_geodesic_distance : t -> t -> float

val geodesic_distance : t -> t -> float

(** place a quaternion inside the 3D ball used to visualise them *)
val to3D : t -> Mat.mat

(** sample quaternions from a geodesic-uniform distribution on SO(3) *)
val sample : unit -> t
