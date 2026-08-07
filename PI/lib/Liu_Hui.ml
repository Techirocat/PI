let side_count s = 6. *. (Float.pow 2. s)

let rec side_length len s =
    if s <= 0 then
        len
    else
        let half_side = len /. 2. in
        let perp = Float.sqrt(1. -. (half_side *. half_side)) in
        let excess = 1. -. perp in
        let len' = Float.sqrt((excess *. excess) +. (half_side *. half_side)) in
        side_length len' (s-1)

let calc s = (side_length 1. s) *. (side_count (float s)) /. 2.

let () =
  let split_count = 9 in

  let pi = calc split_count in
  Printf.printf "%.10f\n" pi;
