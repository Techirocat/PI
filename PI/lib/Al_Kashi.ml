let rec aux n c =
    if n <= 0 then
        c
    else 
        let c' = Float.sqrt (2. -. Float.sqrt (4. -. (c *. c))) in
        aux (n-1) c'

let calc n = 3. *. Float.pow 2. (float n) *. (aux n 1.)

let () =
  let rounds = 20 in

  let pi = calc rounds in
  Printf.printf "%.10f\n" pi;
