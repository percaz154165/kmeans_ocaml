type t = {
  mutable centroid : Point.t ;
  mutable points : Point.t array;
}

let create centroid = {
  centroid;
  points = [||];
}

let addPoint cluster point =
  cluster.points <- Array.append cluster.points [|point|]

let recalcCentroid cluster =
  let sum =
    Array.make
      (Point.getDimension cluster.centroid)
      0.0
  in
  let n = Array.length cluster.points in
  if n > 0 then begin
    for i = 0 to n - 1 do
      Array.iteri (fun j value ->
        sum.(j) <- sum.(j) +. value
      ) cluster.points.(i)
    done;
    let centroid =
      Array.map
      (fun x -> x /. float_of_int n)
      sum
    in
    cluster.centroid <- centroid
  end