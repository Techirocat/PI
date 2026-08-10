let sqrt' x = PI.sqrt x 50 250

let ( *: ) = Q.mul
let (+:) = Q.add
let (-:) = Q.sub
let (/:) = Q.div
let (?=) = Q.of_int

(* Assumindo que theta = 30º *)
let first_a = (?= 6) *: ((sqrt' (?= 3)) /: (?= 3))
let first_b = ?= 3

let next_a a b = ((?= 2) *: a *: b) /: (a +: b)
let next_b a' b = sqrt' (a' *: b)

let rec calc n a b = 
    if n <= 1 then
        (a, b)
    else
        let a' = next_a a b in
        let b' = next_b a' b in
        calc (n-1) a' b'

let () =
  let n = 32 in
  let rounds = 60 in

  let min_pi, max_pi = calc rounds first_a first_b in
  let pi = Q.div (Q.add min_pi max_pi) (Q.of_int 2) in

  let num = Q.num pi in 
  let den = Q.den pi in 

  let d = Z.pow (Z.of_int 10) n in 

  let pi' = Z.div (Z.mul num d) den in 
  let decimas = String.sub (Z.to_string pi') 1 n in 

  PI.check decimas;
  Printf.printf "3.%s\n" decimas
