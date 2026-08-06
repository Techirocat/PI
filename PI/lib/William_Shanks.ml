(*William Shanks em 1874 calculou 527 digitos do PI*)

(* fórmula usada (Fórmula de John Machin): pi/4 = 4 * arctan(1/5) - arctan(1/239) *)

let () = 
  let n = 527 in 
  let arctan5 = PI.arctan (Q.div Q.one (Q.of_int 5)) n in 
  let arctan239 = PI.arctan (Q.div Q.one (Q.of_int 239)) n in

  let pi1 = Q.mul (Q.of_int 16) arctan5 in 
  let pi2 = Q.mul (Q.of_int 4) arctan239 in 
  let pi3 = Q.sub pi1 pi2 in 

  let num = Q.num pi3 in 
  let den = Q.den pi3 in 

  let d = Z.pow (Z.of_int 10) n in 

  let pi = Z.div (Z.mul num d) den in 
  let decimas = String.sub (Z.to_string pi) 1 n in 

  PI.check decimas; 
  Printf.printf "3.%s\n" decimas


