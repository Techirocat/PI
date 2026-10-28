# Chudnovsky em paralelo (CPU)

Cálculo de π com o algoritmo de Chudnovsky, usando *binary splitting* e
paralelismo com `Domain` do OCaml 5.

## Ficheiros

- `chudnovsky.ml` — implementação do algoritmo
- `run_chudnovsky.ml` — calcula π e grava o resultado em `pi.txt`
- `test_pi.ml` — verifica os últimos 50 dígitos do `pi.txt`
- `utils.ml` — funções auxiliares

## Compilar

Na raiz do projeto:

```
dune build
```

## Calcular π

A partir desta pasta (`lib/PIEngine/CPU`):

```
dune exec ./run_chudnovsky.exe -- <digitos>
```

Exemplo:

```
dune exec ./run_chudnovsky.exe -- 100_000
```

O resultado é gravado em `pi.txt`, na pasta onde corres o comando.
Sem argumento, são calculados 1000 dígitos.

## Testar

O teste compara os últimos 50 dígitos do `pi.txt` com valores de referência
do [y-cruncher](https://www.numberworld.org/y-cruncher/).

```
dune exec ./test_pi.exe -- <digitos>
```

O número de dígitos tem de ser **o mesmo** que usaste no `run_chudnovsky`.
Caso contrário, o teste compara o ficheiro com a referência errada e falha.

Exemplo completo:

```
dune exec ./run_chudnovsky.exe -- 100000
dune exec ./test_pi.exe -- 100000
```

Para ver os números de dígitos com referência disponível:

```
dune exec ./test_pi.exe -- help
```

### Resultados

- `[PASS]` — os últimos 50 dígitos estão corretos
- `[FAIL]` — os dígitos não batem certo com a referência
- `[AVISO]` — não existe referência para esse número de dígitos
