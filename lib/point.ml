type t = float array
let create coords = Array.copy coords
let getDimension point = Array.length point
let normalize point mins maxs =
  let dim = Array.length point in
  for i = 0 to dim - 1 do
    let den = maxs.(i) -. mins.(i) in
    if den = 0.0 then
      point.(i) <- 0.0
    else
      point.(i) <- (point.(i) -. mins.(i)) /. den
  done;
  point

let euclideanDistance p1 p2 =
  let acc = ref 0.0 in
  for i = 0 to Array.length p1 - 1 do
    let d = p1.(i) -. p2.(i) in
    acc := !acc +. (d *. d)
  done;
  sqrt !acc
