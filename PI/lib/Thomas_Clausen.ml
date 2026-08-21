(*https://mathshistory.st-andrews.ac.uk/Biographies/Clausen/*)

(* Thomas Clausen em 1847 calculou 248 digitos do PI *)

(* fórmula usada: pi/4 = 2 * arctan(1/3) + arctan(1/7) *)

let () = 
  let decimal_val = 248 in 
  let n = 300 in 
  let arctan3 = PI.arctan (Q.div Q.one (Q.of_int 3)) n in 
  let arctan7 = PI.arctan (Q.div Q.one (Q.of_int 7)) n in

  let pi1 = Q.mul (Q.of_int 2) arctan3 in 
  let pi2 = Q.add pi1 arctan7 in 
  let pi3 = Q.mul (Q.of_int 4) pi2 in 

  let num = Q.num pi3 in 
  let den = Q.den pi3 in 

  let d = Z.pow (Z.of_int 10) decimal_val in 

  let pi = Z.div (Z.mul num d) den in 
  let decimas = String.sub (Z.to_string pi) 1 decimal_val in 

  PI.check decimas; 
  Printf.printf "3.%s\n" decimas


