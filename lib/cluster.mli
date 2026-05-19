type cluster = { mutable centroid : point.t; mutable points : point.t array; }
val create : point.t -> t
val addPoint: t -> point.t -> unit
val recalcCentroid: t -> unit