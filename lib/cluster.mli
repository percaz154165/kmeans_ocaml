type cluster = { mutable centroid : point.t; mutable points : point.t list; mutable converged : bool }
val create : point.t -> t
val addPoint: t -> point.t -> unit
val recalcCentroid: t -> unit