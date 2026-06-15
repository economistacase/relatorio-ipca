# =============================================================================
# R/tratamento.R
# Funções puras de tratamento e preparação dos dados de inflação (IPCA)
#
# Todas as funções são puras: recebem tibbles, devolvem tibbles (ou listas de
# tibbles) sem efeitos colaterais nem dependências de estado global.
#
# Dependências:
#   - dplyr     : manipulação de dados
#   - tidyr     : pivot / expansão
#   - lubridate : extração de componentes de data
#   - slider    : janelas móveis (slide_dbl)
# =============================================================================

library(dplyr)
library(tidyr)
library(lubridate)
library(slider)


# ── Nomes dos meses em pt-BR (independente do locale do sistema) ────────────
.MESES_PTBR_EXTENSO <- c(
  "janeiro", "fevereiro", "março", "abril", "maio", "junho",
  "julho", "agosto", "setembro", "outubro", "novembro", "dezembro"
)

.MESES_PTBR_ABREV <- c(
  "jan", "fev", "mar", "abr", "mai", "jun",
  "jul", "ago", "set", "out", "nov", "dez"
)


# -----------------------------------------------------------------------------
#' Formata uma data em pt-BR, sem depender do locale do sistema
#'
#' \code{format(data, "%B")} e \code{format(data, "%b")} dependem do locale
#' \code{LC_TIME} do sistema operacional, que em ambientes de CI (ex.:
#' GitHub Actions) é tipicamente inglês — produzindo "May" em vez de "maio".
#' Esta função usa uma tabela fixa de nomes em português.
#'
#' @param data \code{Date}.
#' @param formato \code{"extenso"} -> \code{"maio de 2026"};
#'   \code{"abrev"} -> \code{"mai/2026"}.
#'
#' @return \code{character}.
#'
#' @examples
#' \dontrun{
#'   formatar_data_ptbr(as.Date("2026-05-01"), "extenso")  # "maio de 2026"
#'   formatar_data_ptbr(as.Date("2026-05-01"), "abrev")    # "mai/2026"
#' }
# -----------------------------------------------------------------------------
formatar_data_ptbr <- function(data, formato = c("extenso", "abrev")) {
  formato <- match.arg(formato)
  mes <- lubridate::month(data)
  ano <- lubridate::year(data)

  if (formato == "extenso") {
    sprintf("%s de %d", .MESES_PTBR_EXTENSO[mes], ano)
  } else {
    sprintf("%s/%d", .MESES_PTBR_ABREV[mes], ano)
  }
}


# -----------------------------------------------------------------------------
#' Calcula o IPCA acumulado em 12 meses (janela móvel)
#'
#' Para cada mês \eqn{t}, acumula as 12 variações mensais mais recentes —
#' incluindo o próprio mês — pelo método do número-índice encadeado:
#' \deqn{\text{acum\_12m}_t = \left(\prod_{i=0}^{11}\left(1 +
#'   \frac{x_{t-i}}{100}\right) - 1\right) \times 100}
#'
#' Os primeiros 11 meses da série recebem \code{NA} porque a janela ainda
#' não está completa (\code{.complete = TRUE} no \pkg{slider}).
#'
#' @param df \code{tibble} com ao menos as colunas:
#'   \describe{
#'     \item{data}{\code{Date} — primeiro dia do mês de referência.}
#'     \item{ipca_mm}{\code{double} — variação percentual mensal.}
#'   }
#'   Tipicamente o retorno de \code{coletar_ipca_mensal()}.
#'
#' @return O mesmo \code{tibble} de entrada, ordenado por \code{data},
#'   acrescido da coluna:
#'   \describe{
#'     \item{acum_12m}{\code{double} — variação acumulada em 12 meses,
#'       em pontos percentuais. \code{NA} para os primeiros 11 meses.}
#'   }
#'
#' @examples
#' \dontrun{
#'   ipca <- coletar_ipca_mensal()
#'   ipca_com_acum <- calcular_acumulado_12m(ipca)
#' }
# -----------------------------------------------------------------------------
calcular_acumulado_12m <- function(df) {
  stopifnot(
    is.data.frame(df),
    all(c("data", "ipca_mm") %in% names(df))
  )

  df |>
    dplyr::arrange(data) |>
    dplyr::mutate(
      acum_12m = slider::slide_dbl(
        .x        = ipca_mm,
        .f        = ~ (prod(1 + .x / 100) - 1) * 100,
        .before   = 11L,
        .complete = TRUE        # NA enquanto janela incompleta
      )
    )
}


# -----------------------------------------------------------------------------
#' Calcula o IPCA acumulado no ano (janeiro a dezembro)
#'
#' Para cada mês \eqn{t} dentro de um ano calendário, acumula todas as
#' variações mensais desde janeiro do mesmo ano pelo método encadeado:
#' \deqn{\text{acum\_ano}_t = \left(\prod_{m=\text{jan}}^{t}
#'   \left(1 + \frac{x_m}{100}\right) - 1\right) \times 100}
#'
#' O acumulado reinicia automaticamente em cada janeiro (primeiro mês de
#' cada grupo de ano).
#'
#' @param df \code{tibble} com ao menos as colunas \code{data} e
#'   \code{ipca_mm}. Tipicamente o retorno de \code{coletar_ipca_mensal()}.
#'
#' @return O mesmo \code{tibble} de entrada, ordenado por \code{data},
#'   acrescido da coluna:
#'   \describe{
#'     \item{acum_ano}{\code{double} — variação acumulada no ano até o mês
#'       corrente, em pontos percentuais.}
#'   }
#'
#' @examples
#' \dontrun{
#'   ipca <- coletar_ipca_mensal()
#'   ipca_com_acum_ano <- calcular_acumulado_ano(ipca)
#' }
# -----------------------------------------------------------------------------
calcular_acumulado_ano <- function(df) {
  stopifnot(
    is.data.frame(df),
    all(c("data", "ipca_mm") %in% names(df))
  )

  df |>
    dplyr::arrange(data) |>
    dplyr::mutate(.ano = lubridate::year(data)) |>
    dplyr::group_by(.ano) |>
    dplyr::mutate(
      acum_ano = (cumprod(1 + ipca_mm / 100) - 1) * 100
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-.ano)
}


# -----------------------------------------------------------------------------
#' Prepara os dados de IPCA mensal para visualização sazonal
#'
#' Filtra a série histórica a partir de \code{ano_inicio}, decompõe a data em
#' \code{mes} e \code{ano}, e adiciona metadados para estilização em gráficos
#' sazonais (uma linha por ano, eixo-x = mês):
#' \itemize{
#'   \item \code{destaque} — \code{TRUE} para o ano mais recente da série,
#'     permitindo ao camada de visualização aplicar cor ou espessura diferente.
#'   \item \code{ano_label} — rótulo textual do ano para legendas.
#' }
#'
#' @param df \code{tibble} com ao menos as colunas \code{data} e
#'   \code{ipca_mm}. Tipicamente o retorno de \code{coletar_ipca_mensal()}.
#' @param ano_inicio \code{integer} — primeiro ano a incluir na análise.
#'   Padrão: \code{2015}.
#'
#' @return \code{tibble} filtrado e enriquecido com as colunas adicionais:
#'   \describe{
#'     \item{mes}{\code{integer} — número do mês (1–12).}
#'     \item{ano}{\code{integer} — ano de referência.}
#'     \item{ano_label}{\code{character} — ano formatado como string (ex.: "2024").}
#'     \item{destaque}{\code{logical} — \code{TRUE} se o ano for o mais recente
#'       presente nos dados.}
#'   }
#'
#' @examples
#' \dontrun{
#'   ipca <- coletar_ipca_mensal()
#'   df_sazonal <- preparar_sazonal(ipca, ano_inicio = 2015)
#'   # Em ggplot2: aes(x = mes, y = ipca_mm, colour = ano_label,
#'   #                  linewidth = destaque)
#' }
# -----------------------------------------------------------------------------
preparar_sazonal <- function(df, ano_inicio = 2015) {
  stopifnot(
    is.data.frame(df),
    all(c("data", "ipca_mm") %in% names(df)),
    is.numeric(ano_inicio), length(ano_inicio) == 1L
  )

  ano_max <- lubridate::year(max(df$data, na.rm = TRUE))

  df |>
    dplyr::mutate(
      ano = lubridate::year(data),
      mes = lubridate::month(data)
    ) |>
    dplyr::filter(ano >= as.integer(ano_inicio)) |>
    dplyr::mutate(
      ano_label = as.character(ano),
      destaque  = (ano == ano_max)
    ) |>
    dplyr::arrange(ano, mes)
}


# -----------------------------------------------------------------------------
#' Prepara as contribuições dos grupos ao IPCA mensal
#'
#' Calcula, para cada grupo de despesa e cada mês, a contribuição em pontos
#' percentuais ao IPCA cheio pela fórmula padrão do IBGE:
#' \deqn{\text{contribuicao}_{i,t} = \frac{\text{variacao}_{i,t} \times
#'   \text{peso}_{i,t}}{100}}
#'
#' A soma das contribuições dos 9 grupos deve reproduzir a variação mensal
#' do IPCA cheio (com pequenas diferenças de arredondamento). Esta
#' propriedade é verificada internamente: o elemento \code{sanity} do
#' retorno contém, por mês, a soma das contribuições
#' (\code{soma_contribuicoes}), a soma dos pesos (\code{soma_pesos}, que
#' deve ser ≈ 100) e um flag \code{ok_pesos} indicando se \eqn{|soma_pesos -
#' 100| < 0{,}5}.
#'
#' @param df_grupos \code{tibble} com ao menos as colunas:
#'   \describe{
#'     \item{data}{\code{Date} — primeiro dia do mês.}
#'     \item{grupo_cod}{\code{character} — código numérico do grupo.}
#'     \item{grupo}{\code{character} — rótulo do grupo.}
#'     \item{variacao}{\code{double} — variação percentual mensal do grupo.}
#'     \item{peso}{\code{double} — peso do grupo no IPCA.}
#'   }
#'   Tipicamente o retorno de \code{coletar_ipca_grupos()}.
#'
#' @return Lista nomeada com três elementos:
#'   \describe{
#'     \item{historico}{\code{tibble} com todas as observações históricas,
#'       acrescido da coluna \code{contribuicao}.}
#'     \item{mes_atual}{\code{tibble} — subconjunto de \code{historico}
#'       filtrado para o mês mais recente disponível.}
#'     \item{sanity}{\code{tibble} com \code{data}, \code{soma_contribuicoes},
#'       \code{soma_pesos} e \code{ok_pesos}, para verificação da consistência
#'       interna dos dados.}
#'   }
#'
#' @examples
#' \dontrun{
#'   grupos <- coletar_ipca_grupos()
#'   result  <- preparar_contribuicoes(grupos)
#'
#'   result$mes_atual      # contribuições do mês mais recente
#'   result$sanity         # diagnóstico de consistência
#'
#'   # Identificar meses com pesos inconsistentes:
#'   dplyr::filter(result$sanity, !ok_pesos)
#' }
# -----------------------------------------------------------------------------
preparar_contribuicoes <- function(df_grupos) {
  stopifnot(
    is.data.frame(df_grupos),
    all(c("data", "grupo_cod", "grupo", "variacao", "peso") %in%
          names(df_grupos))
  )

  # ── Histórico completo com contribuição ─────────────────────────────────
  historico <- df_grupos |>
    dplyr::arrange(data, grupo_cod) |>
    dplyr::mutate(
      contribuicao = variacao * peso / 100
    )

  # ── Recorte do mês mais recente ─────────────────────────────────────────
  data_max  <- max(historico$data, na.rm = TRUE)
  mes_atual <- dplyr::filter(historico, data == data_max)

  # ── Sanity check: soma de pesos ≈ 100, soma de contribuições ≈ IPCA ─────
  sanity <- historico |>
    dplyr::group_by(data) |>
    dplyr::summarise(
      soma_contribuicoes = sum(contribuicao, na.rm = TRUE),
      soma_pesos         = sum(peso,         na.rm = TRUE),
      .groups            = "drop"
    ) |>
    dplyr::mutate(
      ok_pesos = abs(soma_pesos - 100) < 0.5   # tolerância de arredondamento
    )

  n_inconsistentes <- sum(!sanity$ok_pesos, na.rm = TRUE)
  if (n_inconsistentes > 0L) {
    warning(
      sprintf(
        paste0(
          "[preparar_contribuicoes] %d mês(es) com soma de pesos fora de ",
          "[99,5 ; 100,5]. Verifique `$sanity` para detalhes."
        ),
        n_inconsistentes
      ),
      call. = FALSE
    )
  }

  list(
    historico  = historico,
    mes_atual  = mes_atual,
    sanity     = sanity
  )
}


# -----------------------------------------------------------------------------
#' Expande a meta anual de inflação para frequência mensal
#'
#' A série 13521 do BCB registra a meta central de inflação com uma entrada
#' por ano. Esta função realiza um \emph{left join} por ano entre a série
#' mensal do IPCA e a meta anual, propagando o valor da meta para todos os
#' meses do respectivo ano.
#'
#' Meses cujo ano não possui meta cadastrada recebem \code{NA} em
#' \code{meta_inflacao} (ex.: anos futuros ainda não deliberados pelo CMN).
#'
#' @param df_ipca \code{tibble} com ao menos as colunas \code{data}
#'   (\code{Date}) e \code{ipca_mm} (\code{double}). Tipicamente o retorno
#'   de \code{coletar_ipca_mensal()}.
#' @param df_meta \code{tibble} com ao menos as colunas \code{data}
#'   (\code{Date}) e \code{meta_inflacao} (\code{double}). Tipicamente o
#'   retorno de \code{coletar_meta_inflacao()}. A frequência esperada é
#'   anual (uma linha por ano); em caso de múltiplas linhas por ano, é
#'   mantida apenas a primeira após ordenação por \code{data}.
#'
#' @return \code{tibble} com todas as colunas de \code{df_ipca} e a coluna
#'   adicional:
#'   \describe{
#'     \item{meta_inflacao}{\code{double} — meta central de inflação para o
#'       ano do respectivo mês, em pontos percentuais.}
#'   }
#'
#' @examples
#' \dontrun{
#'   df_ipca <- coletar_ipca_mensal()
#'   df_meta <- coletar_meta_inflacao()
#'   df_completo <- preparar_meta_mensal(df_ipca, df_meta)
#'
#'   # Verificar anos sem meta:
#'   dplyr::filter(df_completo, is.na(meta_inflacao)) |>
#'     dplyr::distinct(lubridate::year(data))
#' }
# -----------------------------------------------------------------------------
preparar_meta_mensal <- function(df_ipca, df_meta) {
  stopifnot(
    is.data.frame(df_ipca),
    all(c("data", "ipca_mm") %in% names(df_ipca)),
    is.data.frame(df_meta),
    all(c("data", "meta_inflacao") %in% names(df_meta))
  )

  # Garantir uma única entrada por ano na série de metas (pegar a primeira
  # ocorrência caso a série venha com datas intra-anuais)
  meta_anual <- df_meta |>
    dplyr::arrange(data) |>
    dplyr::mutate(.ano = lubridate::year(data)) |>
    dplyr::distinct(.ano, .keep_all = TRUE) |>
    dplyr::select(.ano, meta_inflacao)

  # Expandir para mensal via join por ano
  df_ipca |>
    dplyr::arrange(data) |>
    dplyr::mutate(.ano = lubridate::year(data)) |>
    dplyr::left_join(meta_anual, by = ".ano") |>
    dplyr::select(-.ano)
}
