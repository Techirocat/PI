(* https://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80 *)


let getPi n = (* n -> número de iterações *)
  let n = Z.of_string n in 
  if Z.leq n Z.minus_one then failwith "Erro"
  else 
    let rec aux k acc = 
      if Z.gt k n then 
        Q.mul (Q.of_int 4) acc
      else

      let den = Z.add (Z.mul (Z.of_int 2) k) Z.one in 
      let num = if Z.is_even k then Z.one else Z.minus_one in
      let q = Q.make num den in 
      let new_acc = Q.add acc q 
      in aux (Z.succ k) new_acc 
    in aux Z.one Q.one  

let () = 
  let pi = getPi "100000" in 
  PI.print_pi_Q pi 10;  