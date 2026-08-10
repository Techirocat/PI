(**
   Retirado de: https://mathscholar.org/2019/02/simple-proofs-archimedes-calculation-of-pi/
*)

(* Primeira iteração *)
let theta k = Float.pi /. 3. /. (Float.pow 2. k)

let first_a k = 3. *. (Float.pow 2. k) *. (Float.tan @@ theta k)

let first_b k = 3. *. (Float.pow 2. k) *. (Float.sin @@ theta k)

(* Assumindo que theta = 30º *)
let first_a1 = 6. *. ((Float.sqrt 3.) /. 3.)
let first_b1 = 3.

let next_a a b = (2. *. a *. b) /. (a +. b)
let next_b a' b = Float.sqrt(a' *. b)

let rec calc n a b = 
    if n <= 1 then
        (a, b)
    else
        let a' = next_a a b in
        let b' = next_b a' b in
        calc (n-1) a' b'

let () =
  let rounds = 6 in

  let min_pi, max_pi = calc rounds first_a1 first_b1 in
  Printf.printf "Minimum: %.2f\n" min_pi;
  Printf.printf "Maximum: %.2f\n" max_pi
