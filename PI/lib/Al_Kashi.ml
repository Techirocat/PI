let sqrt' x = PI.sqrt x 500 500

let rec aux n c =
    if n <= 0 then
        c
    else 
        let c' = sqrt' (Q.sub (Q.of_int 2) (sqrt' (Q.sub (Q.of_int 4) (Q.mul c c)))) in
        aux (n-1) c'

let ( *: ) a b = Q.mul a b

let calc n = (Q.of_int 3) *: (Q.mul_2exp Q.one n) *: (aux n Q.one)

let () =
  let n = 16 in
  let rounds = 30 in

  let pi = calc rounds in

  let num = Q.num pi in 
  let den = Q.den pi in 

  let d = Z.pow (Z.of_int 10) n in 

  let pi' = Z.div (Z.mul num d) den in 
  let decimas = String.sub (Z.to_string pi') 1 n in 

  PI.check decimas;
  Printf.printf "3.%s\n" decimas
