let initClusters points seed k =
  let numPoints = Array.length points in
  let clusters = Array.make k (Cluster.create [||]) in

  Random.init seed;

  for i = 0 to k - 1 do
    let idx = Random.int numPoints in
    let centroid = points.(idx) in

    Printf.printf
      "Centroid %d: %s\n"
      (i + 1)
      (centroid
      |> Array.to_list
      |> List.map string_of_float
      |> String.concat ", ");

    clusters.(i) <- Cluster.create centroid
  done;

  clusters


let cleanClusters clusters =
  Array.iter (fun cluster ->
    cluster.points <- []
  ) clusters


let assignPointsToClusters points clusters =
  for i = 0 to Array.length points - 1 do

    let point = points.(i) in

    let closestCluster = ref 0 in

    let closestDistance =
      ref (Point.distance point clusters.(0).centroid)
    in

    for j = 1 to Array.length clusters - 1 do

      let distance =
        Point.distance
          point
          clusters.(j).centroid
      in

      if distance < !closestDistance then begin
        closestDistance := distance;
        closestCluster := j;
      end

    done;

    Cluster.addPoint
      clusters.(!closestCluster)
      point

  done


let updateCentroids clusters =
  Array.iter Cluster.recalcCentroid clusters


let hasNotConvergedAll clusters =
  Array.exists (fun cluster ->
    not cluster.converged
  ) clusters