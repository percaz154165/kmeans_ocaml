open Kmeans_ocaml

let time f =
  let start = Unix.gettimeofday () in
  let res = f () in
  let stop = Unix.gettimeofday () in
  (res, stop -. start)

let () =
  let seed = 42 in
  let k = 6 in
  let maxIters = 100 in

  print_endline "Reading dataset...";

  let points, t_read =
    time (fun () ->
      CsvReaderSerial.readDataset "DATA/datos_ampliado.csv"
    )
  in


  print_endline "Initializing clusters...";

  let clusters =
    Kmeans.initClusters points seed k
  in

  print_endline "Starting K-means iterations...";

  let iters, t_kmeans =
    time (fun () ->
      let iters = ref 0 in

      while
        (Kmeans.hasNotConvergedAll clusters)
        && (!iters < maxIters)
      do
        incr iters;
        Kmeans.cleanClusters clusters;
        Kmeans.assignPointsToClusters points clusters;
        Kmeans.updateCentroids clusters;

        if !iters mod 10 = 0 then begin
          Printf.printf "Iteration %d\n%!" !iters;
          flush stdout;
        end
      done;
      !iters
    )
  in
  Printf.printf "Used %d iterations\n%!" iters;

  Printf.printf "Dataset read in %.4f seconds\n%!" t_read;

  Printf.printf "K-means done in %.4f seconds\n%!" t_kmeans;

  Printf.printf "TOTAL TIME: %.4f seconds\n%!"
    (t_read +. t_kmeans)