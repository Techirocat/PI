
(*Madhava de Sangamagrama em 1400 consegui os 10 primeiros algarismos do PI
utilizando a fórmula de Leibniz
*)

(* TODO: implementar novamente mas usando Zarith *)

let calc n =
  let rec aux i sum = 
    if i >= n then 
      sum *. 4.0
    else 
      let den = (2.0 *. (float_of_int i)) +. 1.0 in 
      let res = 
        if i mod 2 = 0 then
          1.0 /. den
        else 
          (-1.0) /. den 
      in 
      aux (i + 1) (sum +. res) 
  in aux 0 0.0


let () =
  let rounds = 9000000000 in

  let pi = calc rounds in
  Printf.printf "%.10f\n" pi
