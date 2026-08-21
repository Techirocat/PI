(*https://www.cambridge.org/core/journals/mathematical-gazette/article/abs/1889-evaluation-of-are-shanks-figures-correct/04A6DD7F3F63B89B81419042C6386B13*)

(*D. F. Ferguson bateu o recorde de casas decimais do PI 4 vezes
  - Em 1946 com 620 casas decimais
  - Em 1947 com 710 casas decimais
  - Novamente em 1947 com 808 casas decimais
  - Em 1949 com 1120 casas decimais
*)

(* fórmula usada: pi/4 = 3 * arctan(1/4) + arctan(1/20) + arctan(1/1985) *)

let () = 
  Printf.printf 
  "D. F. Ferguson bateu o recorde de casas decimais do PI 4 vezes:
  \t1 - Em 1946 com 620 casas decimais
  \t2 - Em 1947 com 710 casas decimais
  \t3 - Novamente em 1947 com 808 casas decimais
  \t4 - Em 1949 com 1120 casas decimais\nSeleciona uma opção para ver o respetivo número de casas décimais: ";

  let op = read_int () in 
  if op < 1 || op > 4 then exit 0;

  let decimal_val = 
    if op = 1 then 620 
    else if op = 2 then 710
    else if op = 3 then 808
    else 1120
  in 


  let n = 1000 in 
  let arctan4 = PI.arctan (Q.div Q.one (Q.of_int 4)) n in 
  let arctan20 = PI.arctan (Q.div Q.one (Q.of_int 20)) n in
  let arctan1985 = PI.arctan (Q.div Q.one (Q.of_int 1985)) n in
  
  let pi1 = Q.mul (Q.of_int 3) arctan4 in 
  let pi2 = Q.add arctan20 arctan1985 in 
  let pi3 = Q.add pi1 pi2 in 
  let pi4 = Q.mul (Q.of_int 4) pi3 in 

  let num = Q.num pi4 in 
  let den = Q.den pi4 in 

  let d = Z.pow (Z.of_int 10) decimal_val in 

  let pi = Z.div (Z.mul num d) den in 
  let decimas = String.sub (Z.to_string pi) 1 decimal_val in 

  PI.check decimas; 
  Printf.printf "\n\n3.%s\n" decimas


