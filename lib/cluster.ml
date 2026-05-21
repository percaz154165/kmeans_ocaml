type t = {
  mutable centroid : Point.t;
  mutable sum_coords : float array;
  mutable count : int;
  mutable converged : bool;
}

let create centroid = {
  centroid;
  sum_coords = Array.make (Point.getDimension centroid) 0.0;
  count = 0;
  converged = false;
}

(* Solo sumamos las coordenadas y aumentamos el contador *)
let addPoint cluster point =
  cluster.count <- cluster.count + 1;
  for i = 0 to Array.length point - 1 do
    cluster.sum_coords.(i) <- cluster.sum_coords.(i) +. point.(i)
  done

let recalcCentroid cluster =
  if cluster.count > 0 then begin
    let centroidNew =
      Array.map
        (fun sum -> sum /. float_of_int cluster.count)
        cluster.sum_coords
    in

    cluster.converged <-
      Point.euclideanDistance
        cluster.centroid
        centroidNew
      < 1e-4;

    cluster.centroid <- centroidNew
  end else begin
    (* Si un cluster se queda vacío, lo damos por convergido *)
    cluster.converged <- true
  end