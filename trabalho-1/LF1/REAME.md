# Trabalho 1

Implementação em *Haskell* de um interpretador da linguagem **LF1** definida pela gramática [LF1.cf](trabalho-1/LF1/LF1.cf).

## Instruções de Compilação

É recomendado que a compilação seja feita através da ferramenta [Stack](https://docs.haskellstack.org/en/stable/) com o comando a seguir:

```shell
stack install --local-bin-path .
```

## Instruções de Execução (Windows)

A execução do programa pode ser feita diretamente no terminal com o seguinte comando:

```shell
Interpret.exe < [NOME_DO_ARQUIVO.lf1]
```

Alternativamente, também é possível utilizar o [Stack](https://docs.haskellstack.org/en/stable/):

```shell
stack run < [NOME_DO_ARQUIVO.lf1]
```
