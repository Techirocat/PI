(* https://pt.wikipedia.org/wiki/Produto_de_Wallis *)
(* https://mindyourdecisions.com/blog/2016/10/12/the-wallis-product-formula-for-pi-and-its-proof/ *)


let calculatePI n = 
  let n = Z.of_string n in 
  
  let rec aux i half = 
    if Z.gt i n then 
      Q.mul half (Q.of_int 2) 
    else
      
      let i2 = Z.mul (Z.of_int 2) i in 

      let left = 
        let den = Z.sub i2 Z.one in 
        Q.make i2 den 
      in 

      let right = 
        let den = Z.add i2 Z.one in 
        Q.make i2 den 
      in 

      let new_half = Q.mul half (Q.mul left right)
      in aux (Z.succ i) new_half 
  in aux Z.one Q.one 


let () = 
  let pi = calculatePI "100000" in 
  PI.print_pi_Q pi 10


