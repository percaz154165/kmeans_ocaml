let readDataset filename =
  let ic = open_in filename in

  let line = input_line ic in
  let dim = String.split_on_char ',' line |> List.length in

  let points = Array.make 9000000 [||] in

  let mins = Array.make dim max_float in
  let maxs = Array.make dim (-. max_float) in

  let i = ref 0 in

  (* -------- 1. lectura secuencial -------- *)
  (try
     while true do
       let line = input_line ic in
       let parts = String.split_on_char ',' line in

       let values = Array.make dim 0.0 in
       let k = ref 0 in

       for j = 0 to List.length parts - 1 do
         let s = String.trim (List.nth parts j) in
         if s <> "" then begin
           values.(!k) <- float_of_string s;
           incr k
         end
       done;

       points.(!i) <- values;

       for j = 0 to dim - 1 do
         mins.(j) <- min mins.(j) values.(j);
         maxs.(j) <- max maxs.(j) values.(j);
       done;

       if (!i mod 1000000 = 0) then begin
          Printf.printf "Read %d points\n" !i;
          flush stdout;
        end;

       incr i
     done
   with End_of_file ->
     close_in ic);

  let n = Domain.recommended_domain_count () in
  let chunk = !i / n in

  (* -------- 2. normalización paralela -------- *)
  let domains =
    Array.init n (fun t ->
      Domain.spawn (fun () ->
        let start = t * chunk in
        let stop =
          if t = n - 1 then !i
          else (t + 1) * chunk
        in

        for k = start to stop - 1 do
          points.(k) <- Point.normalize points.(k) mins maxs
        done
      )
    )
  in

  Array.iter Domain.join domains;

  Array.sub points 0 !i