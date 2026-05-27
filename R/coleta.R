# =============================================================================
# R/coleta.R
# Funções de coleta de dados de inflação (IPCA) via APIs do BCB e IBGE
#
# Dependências:
#   - rbcb   : acesso ao SGS (Sistema Gerenciador de Séries Temporais) do BCB
#   - sidrar : acesso à API SIDRA do IBGE
#   - dplyr  : manipulação de dados
#   - tidyr  : transformação para formato tidy
#   - lubridate : manipulação de datas
# =============================================================================

library(rbcb)
library(sidrar)
library(dplyr)
library(tidyr)
library(lubridate)


# -----------------------------------------------------------------------------
#' Coleta o IPCA mensal (variação mensal)
#'
#' Busca a série 433 do SGS/BCB, que corresponde à variação percentual mensal
#' do IPCA (Índice Nacional de Preços ao Consumidor Amplo) divulgado pelo IBGE.
#'
#' @param data_inicio Data de início da série no formato "YYYY-MM-DD".
#'   Padrão: "1994-07-01" (início do Plano Real).
#' @param data_fim Data de fim da série no formato "YYYY-MM-DD".
#'   Padrão: data de hoje.
#'
#' @return Um \code{tibble} com as colunas:
#'   \describe{
#'     \item{data}{Data de referência do mês (\code{Date}), no primeiro dia do mês.}
#'     \item{ipca_mm}{Variação percentual mensal do IPCA (\code{double}).}
#'   }
#'
#' @examples
#' \dontrun{
#'   ipca <- coletar_ipca_mensal()
#'   ipca_recente <- coletar_ipca_mensal(data_inicio = "2020-01-01")
#' }
#'
#' @seealso \url{https://www.bcb.gov.br/estatisticas/tabelaespecial}
# -----------------------------------------------------------------------------
coletar_ipca_mensal <- function(
    data_inicio = "1994-07-01",
    data_fim    = as.character(Sys.Date())
) {
  serie_bruta <- rbcb::get_series(
    code       = 433,
    start_date = data_inicio,
    end_date   = data_fim,
    as         = "tibble"
  )

  resultado <- serie_bruta |>
    dplyr::rename(
      data    = date,
      ipca_mm = `433`
    ) |>
    dplyr::mutate(
      data = as.Date(data)
    ) |>
    dplyr::arrange(data)

  resultado
}


# -----------------------------------------------------------------------------
#' Coleta a meta de inflação fixada pelo CMN
#'
#' Busca a série 13521 do SGS/BCB, que registra a meta central de inflação
#' definida pelo Conselho Monetário Nacional (CMN) para cada ano.
#'
#' @param data_inicio Data de início da série no formato "YYYY-MM-DD".
#'   Padrão: "1999-01-01" (início do regime de metas).
#' @param data_fim Data de fim da série no formato "YYYY-MM-DD".
#'   Padrão: data de hoje.
#'
#' @return Um \code{tibble} com as colunas:
#'   \describe{
#'     \item{data}{Data de referência (\code{Date}).}
#'     \item{meta_inflacao}{Meta central de inflação em pontos percentuais (\code{double}).}
#'   }
#'
#' @examples
#' \dontrun{
#'   metas <- coletar_meta_inflacao()
#' }
#'
#' @seealso \url{https://www.bcb.gov.br/monetariasob/politica_e_notas}
# -----------------------------------------------------------------------------
coletar_meta_inflacao <- function(
    data_inicio = "1999-01-01",
    data_fim    = as.character(Sys.Date())
) {
  serie_bruta <- rbcb::get_series(
    code       = 13521,
    start_date = data_inicio,
    end_date   = data_fim,
    as         = "tibble"
  )

  resultado <- serie_bruta |>
    dplyr::rename(
      data           = date,
      meta_inflacao  = `13521`
    ) |>
    dplyr::mutate(
      data = as.Date(data)
    ) |>
    dplyr::arrange(data)

  resultado
}


# -----------------------------------------------------------------------------
#' Coleta o IPCA por grupos de despesa (variação e peso)
#'
#' Consulta a tabela 7060 da API SIDRA/IBGE em duas chamadas separadas:
#' \itemize{
#'   \item Variável 63 — variação percentual mensal de cada grupo;
#'   \item Variável 66 — peso do grupo no índice cheio.
#' }
#' Cobre os 9 grupos da classificação IPCA (c315):
#' \itemize{
#'   \item 7170 — Alimentação e bebidas
#'   \item 7445 — Habitação
#'   \item 7486 — Artigos de residência
#'   \item 7558 — Vestuário
#'   \item 7625 — Transportes
#'   \item 7660 — Saúde e cuidados pessoais
#'   \item 7712 — Despesas pessoais
#'   \item 7766 — Educação
#'   \item 7786 — Comunicação
#' }
#'
#' @param data_inicio Período inicial no formato \code{"YYYYMM"}.
#'   Padrão: \code{"199407"} (julho de 1994).
#' @param data_fim Período final no formato \code{"YYYYMM"}.
#'   Padrão: mês corrente calculado automaticamente.
#'
#' @return Um \code{tibble} no formato tidy com as colunas:
#'   \describe{
#'     \item{data}{Data de referência do mês (\code{Date}), no primeiro dia do mês.}
#'     \item{grupo_cod}{Código numérico do grupo IPCA (\code{character}).}
#'     \item{grupo}{Rótulo do grupo de despesa (\code{character}).}
#'     \item{variacao}{Variação percentual mensal do grupo (\code{double}).}
#'     \item{peso}{Peso do grupo no IPCA cheio em pontos percentuais (\code{double}).}
#'   }
#'
#' @examples
#' \dontrun{
#'   grupos <- coletar_ipca_grupos()
#'   grupos_recentes <- coletar_ipca_grupos(data_inicio = "202001")
#' }
#'
#' @seealso \url{https://sidra.ibge.gov.br/tabela/7060}
# -----------------------------------------------------------------------------
coletar_ipca_grupos <- function(
    data_inicio = "199407",
    data_fim    = format(Sys.Date(), "%Y%m")
) {
  # Códigos dos 9 grupos do IPCA (classificação c315)
  codigos_grupos <- c(7170, 7445, 7486, 7558, 7625, 7660, 7712, 7766, 7786)
  c315_param     <- paste(codigos_grupos, collapse = ",")

  periodo_param  <- paste0(data_inicio, "-", data_fim)

  # ── Chamada 1: variação mensal (variável 63) ──────────────────────────────
  raw_variacao <- sidrar::get_sidra(
    api = paste0(
      "/t/7060/n1/all",
      "/v/63",
      "/p/", periodo_param,
      "/c315/", c315_param
    )
  )

  # ── Chamada 2: peso no índice (variável 66) ───────────────────────────────
  raw_peso <- sidrar::get_sidra(
    api = paste0(
      "/t/7060/n1/all",
      "/v/66",
      "/p/", periodo_param,
      "/c315/", c315_param
    )
  )

  # ── Função auxiliar: padroniza cada data frame bruto ─────────────────────
  .limpar_sidra <- function(df, nome_valor) {
    df |>
      dplyr::as_tibble() |>
      dplyr::select(
        periodo     = `Mês (Código)`,
        grupo       = `Geral, grupo, subgrupo, item e subitem`,
        grupo_cod   = `Geral, grupo, subgrupo, item e subitem (Código)`,
        valor       = Valor
      ) |>
      dplyr::mutate(
        data      = lubridate::ym(periodo),
        grupo_cod = as.character(grupo_cod),
        valor     = as.double(valor)
      ) |>
      dplyr::select(data, grupo_cod, grupo, valor) |>
      dplyr::rename(!!nome_valor := valor)
  }

  variacao_limpa <- .limpar_sidra(raw_variacao, "variacao")
  peso_limpo     <- .limpar_sidra(raw_peso,     "peso")

  # ── Une as duas séries e ordena ──────────────────────────────────────────
  resultado <- dplyr::left_join(
    variacao_limpa,
    peso_limpo,
    by = c("data", "grupo_cod", "grupo")
  ) |>
    dplyr::arrange(data, grupo_cod)

  resultado
}
