(* https://vixra.org/pdf/2312.0074v1.pdf *)
(* Abraham Sharp em 1699 calculo 71 digitos do pi *)


let () =
  let sqrt3 = "1.732050807568877293527446341505872366942805253810380628055806979451933016908800037081146186757248576" in 
  let n = 150 in 
  let decimal_val = 71 in 
  let a = PI.arctan (Q.div Q.one (Q.of_string sqrt3)) n in 

  let pi1 = Q.mul (Q.of_int 6) a in 
  
  let num = Q.num pi1 in 
  let den = Q.den pi1 in 

  let d = Z.pow (Z.of_int 10) decimal_val in 

  let pi = Z.div (Z.mul num d) den in 
  let decimas = String.sub (Z.to_string pi) 1 decimal_val in 

  PI.check decimas; 
  Printf.printf "3.%s\n" decimas

