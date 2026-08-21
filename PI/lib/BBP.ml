(* https://observablehq.com/@rreusser/computing-with-the-bailey-borwein-plouffe-formula *)


let s j n =
  let j = Z.of_int j in  
  let left = 
    let rec aux k l =
      if Z.compare k n > 0 then l
      else
        let r = Z.add (Z.mul (Z.of_int 8)  k) j in 
        let exp = Z.sub n k in
        let a = Z.to_float (Z.powm (Z.of_int 16) exp r) in 
        let b = Z.to_float r in 
        let c = l +. (a /. b) in 
      
        let new_l = mod_float c 1.0 
        in aux (Z.add k Z.one) new_l
    in aux Z.zero 0.0
  in 

  let right = 
    let rec aux k r = 
      let a = Z.to_float (Z.add (Z.mul (Z.of_int 8) k) j) in 
      let exp = Z.to_float (Z.sub n k) in
      let new_r = r +. (16.0 ** exp) /. a in 

      if r = new_r then 
        r 
      else
        aux (Z.add k Z.one) new_r 
    in aux (Z.add n Z.one) 0.0
  in

  left +. right


  let piBBP d n = (* d -> posção; n -> número de digitos*)
  let new_d = Z.sub (Z.of_string d) Z.one in 

  let s1 = s 1 new_d in
  let s4 = s 4 new_d in 
  let s5 = s 5 new_d in 
  let s6 = s 6 new_d in  

  let a = (4.0 *. s1) -. (2.0 *. s4) -. s5 -. s6 in 
  let a = a -. floor a in 

  let m = (16.0 ** (float_of_int n)) *. a in 
  int_of_float m



let () =
  let m = piBBP "0" 10 in 
  Printf.printf "%X\n" m
