(* https://www.geeksforgeeks.org/dsa/calculate-pi-using-nilkanthas-series/ *)

let calculatePI n = 
  let n = Z.of_string n in

  let rec aux i pi k sign = 
    if Z.gt i n then pi
    else 

      let new_pi = 
        let a = Z.mul k (Z.add k Z.one) in 
        let b = Z.mul a (Z.add k (Z.of_int 2)) in 
        let c = Q.make (Z.of_int 4) b 
        in Q.add pi (Q.mul sign c) 
      in

      let new_sign = Q.mul Q.minus_one sign in 
      let new_k = Z.add k (Z.of_int 2)

      in aux (Z.succ i) new_pi new_k new_sign

  in aux Z.one (Q.of_int 3) (Z.of_int 2) Q.one 


let () = 
  let pi = calculatePI "10000" in
  PI.print_pi_Q pi 10
