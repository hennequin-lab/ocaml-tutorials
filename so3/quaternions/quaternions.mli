open Owl

type t

val create : float -> Mat.mat -> t
val dot_prod : t -> t -> float
val ( * ) : t -> t -> t
val geodesic_distance : t -> t -> float
val to3D : t -> Mat.mat
