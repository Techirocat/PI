(* https://observablehq.com/@rreusser/computing-with-the-bailey-borwein-plouffe-formula *)


let modPow b e m = 
  let rec aux k a y = 
    if k <= 0 then
      y
    else 
      let new_y = 
        if k land 1 = 1 then (y * a) mod m 
        else y 
      in 

      let new_a = (a * a) mod m in 
      let new_k = k lsr 1 in 

      aux new_k new_a new_y
  in aux e (b mod m) 1  
         


let s j n = 
  let left = 
    let rec aux k l =
      if k > n then l
      else
        let r = 8 * k + j in 
        let a = float_of_int (modPow 16 (n-k) r) in 
        let b = float_of_int r in 
        let c = l +. (a /. b) in 
      
        let new_l = mod_float c 1.0 
        in aux (k + 1) new_l
    in aux 0 0.0
  in 

  let right = 
    let rec aux k r = 
      let a = float_of_int (8 * k + j) in 
      let exp = float_of_int (n - k) in 
      let new_r = r +. (16.0 ** exp) /. a in 

      if r = new_r then 
        r 
      else
        aux (k + 1) new_r 
    in aux (n + 1) 0.0
  in

  left +. right


let piBBP d n = 
  let new_d = d - 1 in 

  let s1 = s 1 new_d in
  let s4 = s 4 new_d in 
  let s5 = s 5 new_d in 
  let s6 = s 6 new_d in  

  let a = (4.0 *. s1) -. (2.0 *. s4) -. s5 -. s6 in 
  let a = a -. floor a in 

  let m = (16.0 ** (float_of_int n)) *. a in 
  int_of_float m



let () = 
  let m = piBBP 1 1 in 
  Printf.printf "%d" m
