type t = {
  mutable centroid : Point.t;
  mutable points : Point.t list;
  mutable converged : bool;
}

let create centroid = {
  centroid;
  points = [];
  converged = false;
}

let addPoint cluster point =
  cluster.points <- point :: cluster.points

let recalcCentroid cluster =
  let sum =
    Array.make
      (Point.getDimension cluster.centroid)
      0.0
  in

  let n = List.length cluster.points in

  if n > 0 then begin

    List.iter (fun point ->
      Array.iteri (fun j value ->
        sum.(j) <- sum.(j) +. value
      ) point
    ) cluster.points;

    let centroidNew =
      Array.map
        (fun x -> x /. float_of_int n)
        sum
    in

    cluster.converged <-
      Point.distance
        cluster.centroid
        centroidNew
      < 1e-4;

    cluster.centroid <- centroidNew
  end