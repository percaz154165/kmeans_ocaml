let readDataset filename =

  let ic = open_in filename in 

  let line = input_line ic in
  let dim = String.split_on_char ',' line |> List.length in

  let points = Array.make 9000000 [||] in

  let mins = Array.make dim max_float in
  let maxs = Array.make dim (-. max_float) in

  let i = ref 0 in

  (try
      while true do

        let values =
          input_line ic
          |> String.split_on_char ','
          |> List.map float_of_string
          |> Array.of_list
        in

        points.(!i) <- values;

        for j = 0 to dim - 1 do
          if values.(j) < mins.(j) then
            mins.(j) <- values.(j);

          if values.(j) > maxs.(j) then
            maxs.(j) <- values.(j);
          done 
        done

        if !i mod 1000000 = 0 then
          Printf.printf "Read %d points\n" !i
        
        incr i

      done

    with End_of_file ->
      close_in ic
  );
  
  let normalized = Array.make !i [||] in

  for k = 0 to !i - 1 do
    normalized.(k) <- Point.normalize points.(k) mins maxs
  done;
  normalized