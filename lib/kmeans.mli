val initClusters : Point.t array -> int -> int -> Cluster.t array
val cleanClusters : Cluster.t array -> unit
val assignPointsToClusters : Point.t array -> Cluster.t array -> unit
val updateCentroids : Cluster.t array -> unit
val hasNotConvergedAll : Cluster.t array -> bool
val copyClusters : Cluster.t array -> Cluster.t array