open Kmeans_ocaml

let () =

  let points = CsvReader.readDataset "data/dataset.csv" in

  let clusters =
    Kmeans.initClusters points 42 2
  in

  let iters = ref 0 in

  while Kmeans.hasNotConvergedAll clusters do
    incr iters;
    Kmeans.cleanClusters clusters;

    Kmeans.assignPointsToClusters points clusters;

    Kmeans.updateCentroids clusters;

    Array.iteri (fun i (cluster : Cluster.t) ->

    Printf.printf
      "\nCluster %d\n"
      (i+1);

    Printf.printf
      "Centroide: %s\n"
      (cluster.centroid
      |> Array.to_list
      |> List.map string_of_float
      |> String.concat ", ");

    Printf.printf
      "Puntos: %d\n"
      (List.length cluster.points)

  ) clusters;

  Printf.printf "n\n\n\n\n\n";

  done;

  Printf.printf "Converged in %d iterations\n" !iters

  