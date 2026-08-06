(* William Rutherford calculou 152 digitos do pi em 1841 *)

(* fórmula usada: pi/4 = 4 * arctan(1/5) - arctan (1/70) + arctan(1/99) *)

let () = 
  let n = 152 in 
  let arctan5 = PI.arctan (Q.div Q.one (Q.of_int 5)) n in 
  let arctan70 = PI.arctan (Q.div Q.one (Q.of_int 70)) n in
  let arctan99 = PI.arctan (Q.div Q.one (Q.of_int 99)) n in

  let pi1 = Q.sub (Q.mul (Q.of_int 4) arctan5) arctan70 in 
  let pi2 = Q.add arctan99 pi1 in 
  let pi3 = Q.mul pi2 (Q.of_int 4) in 

  let num = Q.num pi3 in 
  let den = Q.den pi3 in 

  let d = Z.pow (Z.of_int 10) n in 

  let pi = Z.div (Z.mul num d) den in 

  let decimas = String.sub (Z.to_string pi) 1 n in 
  PI.check decimas;

  Printf.printf "3.%s\n" (String.sub (Z.to_string pi) 1 n)
  
  

