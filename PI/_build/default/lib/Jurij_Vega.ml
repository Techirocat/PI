(* Jurij Vega calculou 126 digitos do pi em 1789
    e em 1794 136 digitos*)

(* fórmula usada (Fórmula de Euler de 1755): pi/4 = 5 * arctan(1/7) + 2 * arctan (3/79) *)

let () = 
  let n = 136 in 
  let arctan7 = PI.arctan (Q.div Q.one (Q.of_int 7)) n in 
  let arctan79 = PI.arctan (Q.div (Q.of_int 3) (Q.of_int 79)) n in

  let pi1 = Q.mul (Q.of_int 5) arctan7 in 
  let pi2 = Q.mul (Q.of_int 2) arctan79 in 
  let pi3 = Q.mul (Q.of_int 4) (Q.add pi1 pi2) in 

  let num = Q.num pi3 in 
  let den = Q.den pi3 in 

  let d = Z.pow (Z.of_int 10) n in 

  let pi = Z.div (Z.mul num d) den in 
  let decimas = String.sub (Z.to_string pi) 1 n in 

  PI.check decimas;
  Printf.printf "3.%s\n" (String.sub (Z.to_string pi) 1 n)
  
  

