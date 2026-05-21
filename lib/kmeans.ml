let initClusters points seed k =
  let numPoints = Array.length points in
  let clusters = Array.make k (Cluster.create [||]) in

  Random.init seed;
  let oldIdx = ref (-1) in

  for i = 0 to k - 1 do
    let idx = ref (Random.int numPoints) in
    while !idx = !oldIdx do
      idx := Random.int numPoints;
    done;

    oldIdx := !idx;
    let centroid = points.(!idx) in

    clusters.(i) <- Cluster.create centroid
  done;

  clusters

let cleanClusters clusters =
  Array.iter (fun (cluster : Cluster.t) ->
    cluster.count <- 0;
    Array.fill cluster.sum_coords 0 (Array.length cluster.sum_coords) 0.0
  ) clusters

let assignPointsToClusters points clusters =
  for i = 0 to Array.length points - 1 do
    let point = points.(i) in
    let closestCluster = ref 0 in
    let firstCluster : Cluster.t = clusters.(0) in

    let closestDistance =
      ref (Point.euclideanDistance point firstCluster.centroid)
    in

    for j = 1 to Array.length clusters - 1 do
      let distance =
        Point.euclideanDistance
          point
          clusters.(j).centroid
      in
      if distance < !closestDistance then begin
        closestDistance := distance;
        closestCluster := j;
      end
    done;

    Cluster.addPoint clusters.(!closestCluster) point
  done

let updateCentroids clusters =
  Array.iter Cluster.recalcCentroid clusters

let hasNotConvergedAll clusters =
  Array.exists (fun (cluster : Cluster.t) ->
    not cluster.converged
  ) clusters

let copyClusters clusters =
  Array.map (fun (cluster : Cluster.t) ->
    Cluster.create (Array.copy cluster.centroid)
  ) clusters