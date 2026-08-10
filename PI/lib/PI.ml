(* Biblioteca com algumas funções auxiliares *)

(* Valor to PI retirado do site: http://www.geom.uiuc.edu/~huberty/math5337/groupe/digits.html 

  Foram copiadas as 10 primeiras linhas -> 778 digitos, quando for preciso de mais logo se adiciona mais...
 *)

let pi_decimas = "141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067982148086513282306647093844609550582231725359408128481117450284102701938521105559644622948954930381964428810975665933446128475648233786783165271201909145648566923460348610454326648213393607260249141273724587006606315588174881520920962829254091715364367892590360011330530548820466521384146951941511609433057270365759591953092186117381932611793105118548074462379962749567351885752724891227938183011949129833673362440656643086021394946395224737190702179860943702770539217176293176752384674818467669405132000568127145263560827785771342757789609173637178721468440901224953430146549585371050792279689258923542019956112129021960864034418159813629774771309960518707211349999998372978049"

let check decimas = 
  let len = String.length decimas in 
  let pi_len = String.length pi_decimas in 

  if len > pi_len then 
    Printf.printf "PI.ml: É necessário aumentar o número de decimas\n"
  else 
    let rec aux i = 
      if i = len then 
        Printf.printf "PI.ml: As décimas estão corretas\n"
      else if pi_decimas.[i] <> decimas.[i] then
        Printf.printf "PI.ml: As décimas não estão corretas, erro na posição %d\n" i
      else aux (i + 1)
    in aux 0


let check_bool decimas = 
  let len = String.length decimas in 
  let pi_len = String.length pi_decimas in 

  if len > pi_len then 
    failwith "PI.ml: É necessário aumentar o número de decimas"
  else 
    let rec aux i = 
      if i = len then 
        true
      else if pi_decimas.[i] <> decimas.[i] then
        false
      else aux (i + 1)
    in aux 0

let arctan x n = 
  let x2 = Q.mul x x in 

  let rec aux x_pow (i:int) sum = 
    if i > n then 
      sum
    else 
      let den = Q.of_int (i * 2 - 1) in 
      let term = Q.div x_pow den in 
    
      let next_pow = Q.mul x_pow x2 in 

      if i mod 2 = 0 then 
        aux next_pow (i + 1) (Q.add sum (Q.neg term))
      else 
        aux next_pow (i + 1) (Q.add sum term)
  in aux x 1 Q.zero 

let truncate x (p : int) =
    let alg = Z.log2 (Q.den x) in
    let diff = alg - p in
    if diff <= 0 then
        x
    else
        let n = Q.num x in
        let d = Q.den x in
        let divi = Z.pow (Z.of_int 2) diff in
        let x' = Q.make (Z.div n divi) (Z.div d divi) in
        x'

(** Heron's Method *)
let sqrt x n p =
    let first = Q.div x (Q.of_int 2) in
    let rec aux curr it = 
        if it <= 0 then
            curr
        else
            let next = Q.div (Q.add (Q.div x curr) curr) (Q.of_int 2) in
            aux (truncate next p) (it-1)
    in
    aux first n

let pow b (e : int) =
    if e < 0 then
        invalid_arg "Not implemented"
    else if e = 0 then
        Q.one
    else
        let rec aux acc mult i =
            if i <= 0 then
                acc
            else
                aux (Q.mul acc mult) mult (i-1) 
        in
        aux Q.one b e
