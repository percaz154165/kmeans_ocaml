open Kmeans_ocaml

let () =

  let points = [|
    [|1.0;2.0|];
    [|1.5;1.8|];
    [|5.0;8.0|];
    [|8.0;8.0|];
    [|1.0;0.6|];
    [|9.0;11.0|];
  |]
  in

  let clusters =
    Kmeans.initClusters points 42 2
  in

  while Kmeans.hasNotConvergedAll clusters do
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

  