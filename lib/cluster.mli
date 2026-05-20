type t = { mutable centroid : Point.t; mutable points : Point.t list; mutable converged : bool }
val create : Point.t -> t
val addPoint: t -> Point.t -> unit
val recalcCentroid: t -> unit