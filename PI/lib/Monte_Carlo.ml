(* https://www.geeksforgeeks.org/dsa/estimating-value-pi-using-monte-carlo/ *)

let monte_carlo (n : int) = 
  Random.self_init ();
  let total = n * n in 

  let rec aux i circle_points square_poits = 
    if  i = total then 
      let num = 4.0 *. float_of_int circle_points in 
      let pi = num /. float_of_int square_poits in 
      pi 
    else begin
      let rand_x = Random.int (n + 1) in 
      let rand_y = Random.int (n + 1) in 

      let dist = rand_x * rand_x + rand_y * rand_y in 
      
      let new_circle_points = 
        if dist <= total then
          circle_points + 1
        else 
          circle_points
      in

      let new_square_points = square_poits + 1 in 
      aux (i + 1) new_circle_points new_square_points
    end
  in aux 0 0 0 


let () =
  let pi = monte_carlo 10002 in
  Printf.printf "%f\n" pi;
