(* https://www.geeksforgeeks.org/dsa/estimating-value-pi-using-monte-carlo/ *)

let monte_carlo (n : string) = 
  let interval = Z.of_string n in 
  let total = Z.mul interval interval in 

  let rec aux i circle_points square_poits = 
    if Z.compare i total = 0 then 
      let num = Z.mul (Z.of_int 4) circle_points in 
      let pi = Q.make num square_poits in 

      pi 
    else begin 
      let rand_x = Z.random_int (Z.add interval Z.one) in 
      let rand_y = Z.random_int (Z.add interval Z.one) in 

      let dist = Z.add (Z.mul rand_x rand_x) (Z.mul rand_y rand_y) in 
      
      let new_circle_points = 
        if Z.leq dist total then
          Z.add circle_points Z.one 
        else 
          circle_points
      in

      let new_square_points = Z.add square_poits Z.one in 
      aux (Z.add i Z.one) new_circle_points new_square_points
    end
  in aux Z.zero Z.zero Z.zero 


let () =
  let pi = monte_carlo "10000" in
  let casas_decimais = 7 in
  PI.print_pi_Q pi casas_decimais; 
