let () = print_endline "Hello, World!"

let base = Q.(///) (Z.of_int 16) (Z.of_int 9)
let pi = Q.mul base base

let () = Printf.printf "pi = %a\n" Q.output pi
let () = Printf.printf "pi = %f\n" (Q.to_float pi)
