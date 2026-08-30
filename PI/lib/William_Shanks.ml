(*William Shanks em 1874 calculou 527 digitos do PI*)

(* fórmula usada (Fórmula de John Machin): pi/4 = 4 * arctan(1/5) - arctan(1/239) *)

let () = 
  let n = 527 in 
  let arctan5 = PI.arctan (Q.div Q.one (Q.of_int 5)) n in 
  let arctan239 = PI.arctan (Q.div Q.one (Q.of_int 239)) n in

  let pi1 = Q.mul (Q.of_int 16) arctan5 in 
  let pi2 = Q.mul (Q.of_int 4) arctan239 in 
  let pi = Q.sub pi1 pi2 in 

  let decimas = PI.get_decimas pi n in 
  PI.check decimas;
  PI.print_pi_Q pi n

