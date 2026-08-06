(* Zacharias Dase Zacharias Dase e Leopold Karl Schulz von Strassnitzky calcularam 200 digitos do pi em 1844 *)

(* fórmula usada (Fórmula de Strassnitzky): pi/4 = arctan(1/2) + arctan (1/5) + arctan(1/8) *)

let () = 
  let n = 350 in 
  let arctan2 = PI.arctan (Q.div Q.one (Q.of_int 2)) n in 
  let arctan5 = PI.arctan (Q.div Q.one (Q.of_int 5)) n in
  let arctan8 = PI.arctan (Q.div Q.one (Q.of_int 8)) n in

  let pi1 = Q.add arctan2 arctan5 in 
  let pi2 = Q.add arctan8 pi1 in 
  let pi3 = Q.mul pi2 (Q.of_int 4) in 

  let num = Q.num pi3 in 
  let den = Q.den pi3 in 

  let d = Z.pow (Z.of_int 10) n in 

  let pi = Z.div (Z.mul num d) den in 

  let decimas = String.sub (Z.to_string pi) 1 n in 
  PI.check decimas;

  Printf.printf "3.%s\n" (String.sub (Z.to_string pi) 1 200)
  
  

