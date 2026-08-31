(* https://www.geeksforgeeks.org/python/estimating-pi-with-buffons-needle-in-python/ *)
(* https://medium.com/@Dhruvkumar_Vyas_VLSI/pi-and-probability-buffons-needle-problem-3edb869de7cb *)

let deg_to_rad deg = deg *. Float.pi /. 180.0

let calculatePi (n : string) (length : float) (space : float) = 
  let n = Z.of_string n in 
  let q_length = Q.of_float length in 
  let q_space = Q.of_float space in

  Random.self_init ();

  let half_space = space /. 2.0 in 
  let half_length = length /. 2.0 in

  let rec aux i crossings = 
    if Z.equal i n then 
      if Z.equal crossings Z.zero then failwith "Division by zero"
      else

        let num = Q.mul (Q.of_int 2) (Q.mul q_length (Q.of_bigint n)) in
        let den = Q.mul q_space (Q.of_bigint crossings) in
        Q.div num den  
    else
      
      let angle_degrees = Random.float 90.0 in 
      let angle_radians = deg_to_rad angle_degrees in 
      let distance = Random.float half_space in

      let new_crossings = 
        let a = half_length *. sin angle_radians in 
        if  distance <= a then Z.succ crossings
        else crossings
      in aux (Z.succ i) new_crossings
  in aux Z.zero Z.zero

let () = 
  let pi = calculatePi "1000000" 1.0 1.0 in 
  PI.print_pi_Q pi 15
