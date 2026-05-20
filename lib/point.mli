type t = float array
val create : float array -> t
val getDimension : t -> int
val normalize : t -> float array -> float array -> t
val euclideanDistance : t -> t -> float