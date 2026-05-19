type point = float array
val create : float array -> point
val getDimension : point -> int
val normalize : point -> float array -> float array -> point
val distance : point -> point -> float