(* https://vixra.org/pdf/2312.0074v1.pdf *)
(* Abraham Sharp em 1699 calculo 71 digitos do pi *)


let () =
  let sqrt3 = "1.732050807568877293527446341505872366942805253810380628055806979451933016908800037081146186757248576" in 
  let n = 150 in 
  let decimal_val = 71 in 

  let a = PI.arctan (Q.div Q.one (Q.of_string sqrt3)) n in 
  let pi1 = Q.mul (Q.of_int 6) a in 
 
  let decimas = PI.get_decimas pi1 decimal_val in 
  PI.check decimas;
  PI.print_pi_Q pi1 decimal_val;

