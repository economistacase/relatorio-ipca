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
library(httr)


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
  # A API do SGS trunca respostas muito longas e descarta silenciosamente as
  # observações mais recentes: um pedido único de 1994-07 até hoje devolve a
  # série até o penúltimo mês, deixando o relatório um mês defasado. Baixar em
  # janelas evita o truncamento (ver .baixar_sgs_em_janelas).
  resultado <- .baixar_sgs_em_janelas(
    code       = 433,
    data_inicio = data_inicio,
    data_fim    = data_fim
  ) |>
    dplyr::rename(ipca_mm = valor)

  resultado
}


# -----------------------------------------------------------------------------
#' Baixa uma série do SGS/BCB em janelas, evitando truncamento da API
#'
#' A API do SGS impõe um limite de observações por requisição. Quando ele é
#' excedido, a resposta vem truncada no fim da série — sem erro e sem aviso —,
#' omitindo justamente os meses mais recentes. Esta função quebra o intervalo
#' solicitado em janelas de \code{anos_por_janela} anos, consulta cada uma
#' separadamente e reúne os resultados, deduplicando sobreposições.
#'
#' @param code Código numérico da série no SGS.
#' @param data_inicio Data de início no formato "YYYY-MM-DD".
#' @param data_fim Data de fim no formato "YYYY-MM-DD".
#' @param anos_por_janela Tamanho de cada janela, em anos. Padrão: 10.
#'
#' @return Um \code{tibble} com as colunas \code{data} e \code{valor}.
#'
#' @keywords internal
# -----------------------------------------------------------------------------
.baixar_sgs_em_janelas <- function(
    code,
    data_inicio,
    data_fim,
    anos_por_janela = 10L
) {
  inicio <- as.Date(data_inicio)
  fim    <- as.Date(data_fim)

  if (is.na(inicio) || is.na(fim) || inicio > fim) {
    stop("Intervalo inválido: verifique data_inicio e data_fim.", call. = FALSE)
  }

  inicios <- seq(inicio, fim, by = paste(anos_por_janela, "years"))
  fins    <- c(inicios[-1] - 1L, fim)

  pedacos <- Map(
    function(ini, f) {
      bruto <- rbcb::get_series(
        code       = code,
        start_date = as.character(ini),
        end_date   = as.character(f),
        as         = "tibble"
      )

      # get_series nomeia a coluna de valores com o próprio código da série
      bruto |>
        dplyr::as_tibble() |>
        stats::setNames(c("data", "valor")) |>
        dplyr::mutate(
          data  = as.Date(data),
          valor = as.double(valor)
        )
    },
    inicios,
    fins
  )

  dplyr::bind_rows(pedacos) |>
    dplyr::distinct(data, .keep_all = TRUE) |>
    dplyr::arrange(data)
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
  resultado <- .baixar_sgs_em_janelas(
    code        = 13521,
    data_inicio = data_inicio,
    data_fim    = data_fim
  ) |>
    dplyr::rename(meta_inflacao = valor)

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

  # WAF da API SIDRA rejeita requisições sem User-Agent de navegador
  # (comum em runners de CI/cloud, ex.: GitHub Actions)
  httr::set_config(httr::add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
  ))

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
