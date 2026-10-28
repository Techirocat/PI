(* test_pi.ml *)
open Pi_cpu.Chudnovsky

(* Registos oficiais consolidados dos últimos 50 dígitos de Pi *)
(*https://www.numberworld.org/y-cruncher/*)

let known_tails = [
  (100,         "58209749445923078164062862089986280348253421170679");
  (1_000,       "18577805321712268066130019278766111959092164201989");
  (10_000,      "46101264836999892256959688159205600101655256375678");
  (100_000,     "70150789337728658035712790913767420805655493624646");
  (1_000_000,   "56787961303311646283996346460422090106105779458151");
  (10_000_000,  "65326553194060999469787333810631719481735348955897");
  (100_000_000, "75722031752074898161168313937514970581120187751592");
  (1_000_000_000, "71398682093196353628204612755715171395115275045519");
  (1_200_000_000, "72912593081212086869129310622322863444636299583604");
  (1_500_000_000, "28645378082135603814855914764072758306381217050377");
  (2_000_000_000, "86430813147294569162304083903758389223023009559510");
  (10_000_000_000, "94039945818580686529237500809231062441314758821220");
]

let decimals = 50 (*Estou a usaar 50 pois é o tamanho que está no site referido em cima *)

(** Extrai apenas os dígitos numéricos de uma string ignorando espaços ou quebras de linha *)
let sanitize_digits (s : string) : string =
	let buf = Buffer.create (String.length s) in
	String.iter (fun c -> if c >= '0' && c <= '9' then Buffer.add_char buf c) s;
	Buffer.contents buf

let verify_tail_file ~(digits : int) (filename : string) : bool =

	In_channel.with_open_bin filename (fun ic ->
 		let total = in_channel_length ic in
    	let read_size = min (3 * decimals) total in seek_in ic (total - read_size); (*move o cursor para a posição indicada*)


    	let digits_only = sanitize_digits (really_input_string ic read_size) in (*Pegamos os ultimos 150 chars do ficheiro e removemos os espaços e as quebras de linha*)
    	let d_len = String.length digits_only in

    	if d_len < decimals then begin 
    		Printf.eprintf "[ERRO] Ficheiro contém menos de 50 dígitos numéricos.\n";
    		false
		end
    	else
    		let actual = String.sub digits_only (d_len - decimals) decimals in (*Pegamos  apenas nos ultimos 50 chars para comparar*)
      		match List.assoc_opt digits known_tails with (*função que compara a string obtida com a lista dos valores conhecidos, função bastante prática*)
      		| None ->
        		Printf.printf "[AVISO] Sem referência para %d dígitos.\nCauda: ...%s\n" digits actual;
          		false
      		| Some expected when String.equal actual expected ->
        		Printf.printf "[PASS] Ficheiro %s (%d dígitos): Cauda de 50 dígitos correta!\n" filename digits;
          		true
      		| Some expected ->
        		Printf.eprintf "[FAIL] Ficheiro %s (%d dígitos): Divergência!\n  Esperado: ...%s\n  Obtido:   ...%s\n" filename digits expected actual;
        		false
	)

let run_tests digits =
	Printf.printf "=== Iniciando Validação dos Últimos 50 Dígitos ===\n\n";

	let filename = "pi.txt" in
		if Sys.file_exists filename then 
    	let ok = verify_tail_file ~digits:digits filename in
    	assert ok
  	else
		Printf.printf "Erro: Ficheiro não encontrado";

	Printf.printf "\nTodos Concluidos.\n"



let print_tails () =
  Printf.printf "Referências disponíveis (número de dígitos):\n";
  List.iter (fun (d, _) -> Printf.printf "  %d\n" d) known_tails


let () =
  if Array.length Sys.argv < 2 
  then begin
    Printf.printf "Uso: %s <digitos>\n" Sys.argv.(0);
    exit 1
  end;
  match Sys.argv.(1) with
  | "help" -> print_tails ()
  | arg ->
    match int_of_string_opt arg with 
    | Some digits -> run_tests digits
    | None -> Printf.printf "Erro: '%s' não é um número válido.\n" Sys.argv.(1); exit 1
