# Painel IPCA — Inflação Brasileira

Relatório reproduzível em [Quarto](https://quarto.org) que analisa o IPCA
(Índice Nacional de Preços ao Consumidor Amplo) com dados direto das APIs
do Banco Central do Brasil e do IBGE.

## Painéis gerados

| Seção | Descrição |
|---|---|
| Variação mensal | Barras dos últimos 24 meses com rótulos individuais |
| Acumulado 12 meses | Linha do IPCA 12m vs. meta CMN com banda ±1,5 p.p. |
| Sazonalidade | Uma linha por ano (2015–atual); ano corrente em destaque |
| Contribuições | Barras horizontais ordenadas por `variação × peso / 100` |

## Fontes de dados

| Dado | API | Série/Tabela |
|---|---|---|
| IPCA mensal | SGS/BCB via `rbcb` | Série 433 |
| Meta de inflação | SGS/BCB via `rbcb` | Série 13521 |
| IPCA por grupos | SIDRA/IBGE via `sidrar` | Tabela 7060 |

## Estrutura do projeto

```
.
├── _quarto.yml            # configuração global do projeto Quarto
├── relatorio_ipca.qmd     # documento principal
├── R/
│   ├── coleta.R           # funções de coleta (BCB + IBGE)
│   ├── tratamento.R       # funções puras de transformação
│   └── graficos.R         # funções ggplot2 + salvar_grafico()
└── README.md
```

Diretórios gerados em build (ignorados pelo Git):

```
_site/      # HTML final
_freeze/    # cache de execução (freeze: auto)
output/     # PNGs exportados por salvar_grafico()
```

## Pré-requisitos

- [R](https://www.r-project.org/) ≥ 4.4
- [Quarto](https://quarto.org/docs/get-started/) ≥ 1.4

Pacotes R necessários (instalados automaticamente na primeira renderização
se ausentes, via `install.packages` no chunk `setup`):

```r
c("rbcb", "sidrar", "dplyr", "tidyr", "lubridate",
  "slider", "ggplot2", "scales", "knitr", "httr")
```

> **Nota:** `rbcb` está no CRAN desde a versão 0.1.14. Caso a instalação
> automática falhe, instale manualmente:
> ```r
> install.packages("rbcb")
> ```

## Como rodar

### Renderização completa (recomendado)

```bash
quarto render
```

Gera `_site/relatorio_ipca.html` (HTML autocontido, sem dependências
externas) e os quatro PNGs em `output/`.

### Renderizar apenas o relatório

```bash
quarto render relatorio_ipca.qmd
```

### Preview com recarregamento automático

```bash
quarto preview relatorio_ipca.qmd
```

### Forçar re-execução dos chunks (ignorar freeze)

```bash
quarto render --cache-refresh
```

### Via RStudio

Abra `relatorio_ipca.qmd` e clique em **Render**, ou use o atalho
`Ctrl + Shift + K`.

## Atualização dos dados

O YAML contém `execute: freeze: auto`, portanto os chunks só são
re-executados quando os arquivos-fonte (`R/*.R`, `relatorio_ipca.qmd`)
são modificados. Para forçar a coleta de dados novos sem alterar o código:

```bash
# Remove o cache de freeze do arquivo e re-executa
rm -rf _freeze/relatorio_ipca
quarto render
```

## Licença

MIT — veja [LICENSE](LICENSE) para detalhes.
