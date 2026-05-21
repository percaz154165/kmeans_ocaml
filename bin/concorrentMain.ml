open Kmeans_ocaml

let time f =
  let start = Unix.gettimeofday () in
  let res = f () in
  let stop = Unix.gettimeofday () in
  (res, stop -. start)

let () =
  let seed = 42 in
  let k = 6 in
  let maxIters = 10000 in

  print_endline "Reading dataset...";

  let points, t_read =
    time (fun () ->
      CsvReaderConcorrent.readDataset "DATA/datos_ampliado.csv"
    )
  in

  print_endline "Initializing clusters...";

  let clusters =
    Kmeans.initClusters points seed k
  in

  print_endline "Starting K-means iterations...";

  let n =
    min (Domain.recommended_domain_count ()) k
  in

  let chunk = k / n in

  let point_chunks =
    Array.init n (fun i ->
      let startPoints = i * (Array.length points / n) in
      let stopPoints =
        if i = n - 1
        then Array.length points
        else (i + 1) * (Array.length points / n)
      in
      Array.sub points startPoints (stopPoints - startPoints)
    )
  in

  let cluster_chunks =
    Array.init n (fun i ->
      let startClusters = i * chunk in
      let stopClusters =
        if i = n - 1
        then k
        else (i + 1) * chunk
      in
      Array.sub clusters startClusters (stopClusters - startClusters)
    )
  in

  let iters, t_kmeans =
    time (fun () ->

      let iters = ref 0 in

      while
        Kmeans.hasNotConvergedAll clusters
        && !iters < maxIters
      do
        incr iters;

        Kmeans.cleanClusters clusters;

        (* -------- Assign points paralelo -------- *)
        let domains =
          Array.init n (fun i ->
            Domain.spawn (fun () ->
              let local_clusters = Kmeans.copyClusters clusters in
              let subPoints = point_chunks.(i) in
              Kmeans.assignPointsToClusters subPoints local_clusters;
              local_clusters
            )
          )
        in

        let results = Array.map Domain.join domains in

        (* -------- Merge Matemático -------- *)
        for c = 0 to k - 1 do
          (* Nos aseguramos de que el global empiece limpio *)
          clusters.(c).count <- 0;
          Array.fill clusters.(c).sum_coords 0 (Array.length clusters.(c).sum_coords) 0.0;

          (* Sumamos los contadores y las coordenadas de cada hilo *)
          for w = 0 to n - 1 do
            let local_cluster = results.(w).(c) in
            
            clusters.(c).count <- clusters.(c).count + local_cluster.count;
            
            for j = 0 to Array.length clusters.(c).sum_coords - 1 do
              clusters.(c).sum_coords.(j) <- 
                clusters.(c).sum_coords.(j) +. local_cluster.sum_coords.(j)
            done
          done
        done;

        (* -------- Update centroides paralelo -------- *)
        let domains =
          Array.init n (fun i ->
            Domain.spawn (fun () ->
              let subclusters = cluster_chunks.(i) in
              Kmeans.updateCentroids subclusters
            )
          )
        in

        Array.iter Domain.join domains;

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
  Printf.printf "TOTAL TIME: %.4f seconds\n%!" (t_read +. t_kmeans)