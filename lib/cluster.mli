type t = {
  mutable centroid : Point.t;
  mutable sum_coords : float array;
  mutable count : int;
  mutable converged : bool;
}
val create : Point.t -> t
val addPoint : t -> Point.t -> unit
val recalcCentroid : t -> unit