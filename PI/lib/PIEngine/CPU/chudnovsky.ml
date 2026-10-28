open Z 

let ( + ) = Stdlib.( + )
let ( - ) = Stdlib.( - )
let ( * ) = Stdlib.( * )
let ( / ) = Stdlib.( / )
let ( land ) = Stdlib.( land )

let constant_a = of_int 13591409
let constant_b = of_int 545140134
let constant_c3 = of_string "10939058860032000"


let combine (p1, q1, t1) (p2, q2, t2) = 
    let p = mul p1 p2 in
    let q = mul q1 q2 in
    let t = add (mul t1 q2) (mul p1 t2) 
in (p, q, t)

let compute_leaf (k : int) = (* O código do repo repetia estes passos umas quantas vezes *) 
    if k = 0 then
        (one, one, constant_a)
    else 
        let zk = of_int k in 
        let p1 = of_int (6 * k - 5) in 
        let p2 = of_int (2 * k - 1) in 
        let p3 = of_int (6 * k - 1) in 
        let p = mul (mul p1 p2) p3 in 

        (*   Q = k^3 * (C^3 / 24)  *)
        let zk_power_3 = mul (mul zk zk) zk in  
        let q = mul zk_power_3 constant_c3 in
        let t = mul p (add (mul constant_b zk) constant_a) in

        let t = if k land 1 = 1 then neg t else t 
        in (p, q, t)

let rec compute_bs (n1 : int) (n2 : int) = (* Função que vai ser chamada dentro do novo Domain *)
    if n2 - n1 = 1 then
        compute_leaf n1            
    else
        let m = n1 + (n2 - n1) / 2 in (*igual binary search*)
        let left_interval = compute_bs n1 m in 
        let right_interval = compute_bs m n2 in 
        combine left_interval right_interval

let rec compute_bs_p (n1 : int) (n2 : int) domains = (* Função que será executada pelo Domain "Principal" dividindo o trabalho em diferentes novos Domains  *)
    if domains <= 1 || (n2 - n1) < 1000 then (**A diferença pode ser alterada posterriormente conformo nós encontrarmos um resultado melhor*)
        compute_bs n1 n2 
    else
        let rec split_interval left right d = (*left, right [left, right], d - domains*)
            if d = 1 || (right - left) <= 1 then (* se só temos um dominio ou se o intervalo é uma folha passamos à função sequencial*)
                compute_bs left right 
            else
                (*Dos Dominios que temos acesso, metade vamos usar para os intervalos do meio para a esquerda e os restantes ficam para a direita*)
                let left_domains = d / 2 in 
                let right_domains = d - left_domains in 
                let m = left + (right - left) / 2 in (*A lá binary search*)

                let left_spawn = Domain.spawn (fun () -> split_interval left m left_domains) in 

                let right_res = split_interval m right right_domains in (* Não criamos um novo Domain para o right para não desperdiçar o Domain atual *)
                let left_res = Domain.join left_spawn in 
                combine left_res right_res
        in split_interval n1 n2 domains



(** [compute_pi digits] computes pi to the specified number of decimal [digits]. *)
let compute_pi (digits : int)  =
    let domains = Domain.recommended_domain_count () in (*Isto estava fora da função principal*)

    let terms = (digits / 14) + 2 in
    let (_, q, t) = compute_bs_p 0 terms domains in
    let d = of_int 426880 in

    (* Eu pensei que isto era uma constante mas parece que eu estava errado :( ... )*)
    (*TODO: Procurar uma maneira mais eficiente de fazer a sqrt*)
    let e = of_int 10005 in
    let exp = digits * 2 in
    let scaled_e = mul e (pow (of_int 10) exp) in
    let sqrt_e = sqrt scaled_e in

    let numerator = mul (mul d sqrt_e) q in
    let denominator = t in

    let pi = div numerator denominator in
    let s = to_string pi in
    let len = String.length s in

    if len <= digits then
        "0." ^ (String.make (digits - len) '0') ^ s
    else
        let dot_pos = len - digits in
        (String.sub s 0 dot_pos) ^ "." ^ (String.sub s dot_pos digits)
