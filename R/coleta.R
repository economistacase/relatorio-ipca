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
  .usar_user_agent_navegador()

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


# -----------------------------------------------------------------------------
#' Define um User-Agent de navegador para as chamadas à API SIDRA
#'
#' O WAF da API SIDRA rejeita requisições sem User-Agent de navegador, o que
#' derruba a coleta em runners de CI/cloud (ex.: GitHub Actions).
#'
#' @return \code{NULL}, invisivelmente. Chamada por efeito colateral.
#'
#' @keywords internal
# -----------------------------------------------------------------------------
.usar_user_agent_navegador <- function() {
  httr::set_config(httr::add_headers(
    `User-Agent` = paste0(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 ",
      "(KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
    )
  ))
  invisible(NULL)
}


# -----------------------------------------------------------------------------
#' Extrai o prefixo numérico do rótulo hierárquico do IBGE
#'
#' Os rótulos da classificação c315 carregam o código hierárquico antes do
#' primeiro ponto: \code{"1101053.Feijão - macáçar"} -> \code{"1101053"}.
#' Rótulos sem prefixo numérico (como \code{"Índice geral"}) devolvem
#' \code{""}.
#'
#' @param rotulo \code{character}.
#'
#' @return \code{character} com o prefixo, ou \code{""}.
#'
#' @keywords internal
# -----------------------------------------------------------------------------
.prefixo_c315 <- function(rotulo) {
  partes  <- strsplit(as.character(rotulo), ".", fixed = TRUE)
  prefixo <- vapply(
    partes,
    function(x) if (length(x) == 0L) "" else x[[1L]],
    character(1L)
  )
  ifelse(grepl("^[0-9]+$", prefixo), prefixo, "")
}


# -----------------------------------------------------------------------------
#' Classifica o nível hierárquico de um rótulo da classificação c315
#'
#' O nível é determinado pela quantidade de dígitos do prefixo numérico:
#' 1 dígito = grupo, 2 = subgrupo, 4 = item, 7 = subitem. Sem prefixo
#' numérico, trata-se do índice geral.
#'
#' @param rotulo \code{character} — rótulo completo do IBGE.
#'
#' @return \code{character} com um de: \code{"geral"}, \code{"grupo"},
#'   \code{"subgrupo"}, \code{"item"}, \code{"subitem"} ou \code{"outro"}.
#'
#' @keywords internal
# -----------------------------------------------------------------------------
.nivel_c315 <- function(rotulo) {
  n <- nchar(.prefixo_c315(rotulo))

  dplyr::case_when(
    n == 0L ~ "geral",
    n == 1L ~ "grupo",
    n == 2L ~ "subgrupo",
    n == 4L ~ "item",
    n == 7L ~ "subitem",
    TRUE    ~ "outro"
  )
}


# -----------------------------------------------------------------------------
#' Remove o prefixo numérico do rótulo hierárquico do IBGE
#'
#' \code{"1101053.Feijão - macáçar (fradinho)"} ->
#' \code{"Feijão - macáçar (fradinho)"}. Rótulos sem prefixo numérico são
#' devolvidos inalterados.
#'
#' @param rotulo \code{character}.
#'
#' @return \code{character} sem o prefixo.
#'
#' @keywords internal
# -----------------------------------------------------------------------------
.rotulo_sem_prefixo <- function(rotulo) {
  rotulo  <- as.character(rotulo)
  prefixo <- .prefixo_c315(rotulo)

  ifelse(
    nzchar(prefixo),
    substring(rotulo, nchar(prefixo) + 2L),
    rotulo
  )
}


# =============================================================================
# Catálogo de séries do SGS/BCB usadas nas análises complementares
#
# Os códigos abaixo foram conferidos contra a divulgação de outubro/2026:
#   - a média dos cinco núcleos reproduz 0,36% (set/26) e 0,22% (out/26),
#     e 4,50% no acumulado em 12 meses, valores reportados pelo mercado;
#   - a série 10844 reproduz a desaceleração de serviços de 0,61% para 0,11%;
#   - a série 21379 reproduz o índice de difusão de 54,9% para 61,3%;
#   - livres (11428) e monitorados (4449), ponderados, reconstroem o índice
#     cheio em set/26 (0,82%) e out/26 (0,14%).
# =============================================================================

#' Cinco núcleos de inflação acompanhados pelo BCB (Estudo Especial 102/2021)
NUCLEOS_SGS <- c(
  "MS"  = 4466L,    # médias aparadas com suavização
  "EX0" = 11427L,   # exclusão — EX0
  "EX3" = 27839L,   # exclusão — EX3
  "DP"  = 16122L,   # dupla ponderação
  "P55" = 28750L    # percentil 55
)

#' Rótulos extensos dos núcleos, para legendas e tabelas
NUCLEOS_ROTULOS <- c(
  "MS"  = "Médias aparadas com suavização",
  "EX0" = "Exclusão EX0",
  "EX3" = "Exclusão EX3",
  "DP"  = "Dupla ponderação",
  "P55" = "Percentil 55"
)

#' Decomposição dos preços livres por durabilidade do bem, mais serviços
CLASSIFICACAO_SGS <- c(
  "Bens não duráveis" = 10841L,
  "Bens semiduráveis" = 10842L,
  "Bens duráveis"     = 10843L,
  "Serviços"          = 10844L
)

#' Preços livres versus preços monitorados (administrados)
LIVRES_MONITORADOS_SGS <- c(
  "Preços livres"      = 11428L,
  "Preços monitorados" = 4449L
)

#' Índice de difusão do IPCA (% de subitens com variação positiva)
DIFUSAO_SGS <- 21379L

#' INPC — variação percentual mensal
INPC_SGS <- 188L


# -----------------------------------------------------------------------------
#' Coleta um conjunto nomeado de séries do SGS/BCB em formato longo
#'
#' Percorre um vetor nomeado de códigos do SGS, baixa cada série com
#' \code{.baixar_sgs_em_janelas()} (imune ao truncamento da API) e empilha os
#' resultados em um único \code{tibble} no formato longo.
#'
#' @param series Vetor nomeado de códigos do SGS. Os nomes viram rótulos na
#'   coluna \code{serie}. Ex.: \code{c("Serviços" = 10844)}.
#' @param data_inicio Data de início no formato "YYYY-MM-DD".
#'   Padrão: \code{"2004-01-01"}.
#' @param data_fim Data de fim no formato "YYYY-MM-DD". Padrão: hoje.
#'
#' @return \code{tibble} com as colunas \code{serie} (\code{character}),
#'   \code{codigo} (\code{integer}), \code{data} (\code{Date}) e
#'   \code{valor} (\code{double}), ordenado por série e data.
#'
#' @examples
#' \dontrun{
#'   nucleos <- coletar_series_sgs(NUCLEOS_SGS, data_inicio = "2015-01-01")
#' }
# -----------------------------------------------------------------------------
coletar_series_sgs <- function(
    series,
    data_inicio = "2004-01-01",
    data_fim    = as.character(Sys.Date())
) {
  stopifnot(
    is.numeric(series), length(series) > 0L,
    !is.null(names(series)), all(nzchar(names(series)))
  )

  pedacos <- Map(
    function(rotulo, codigo) {
      .baixar_sgs_em_janelas(
        code        = codigo,
        data_inicio = data_inicio,
        data_fim    = data_fim
      ) |>
        dplyr::mutate(
          serie  = rotulo,
          codigo = as.integer(codigo),
          .before = 1L
        )
    },
    names(series),
    unname(series)
  )

  dplyr::bind_rows(pedacos) |>
    dplyr::arrange(serie, data)
}


# -----------------------------------------------------------------------------
#' Coleta o IPCA na classificação c315 em qualquer nível de desagregação
#'
#' Generaliza \code{coletar_ipca_grupos()} para toda a hierarquia da tabela
#' 7060: índice geral, 9 grupos, 19 subgrupos, 51 itens e 377 subitens.
#'
#' O nível de cada linha é inferido do próprio rótulo do IBGE, que carrega o
#' código hierárquico como prefixo: \code{"1.Alimentação e bebidas"} (grupo),
#' \code{"11.Alimentação no domicílio"} (subgrupo), \code{"1101.Cereais..."}
#' (item) e \code{"1101053.Feijão..."} (subitem). O número de dígitos do
#' prefixo identifica o nível sem depender de uma tabela de códigos fixa.
#'
#' @param data_inicio Período inicial no formato \code{"YYYYMM"}.
#' @param data_fim Período final no formato \code{"YYYYMM"}.
#'   Padrão: mês corrente.
#' @param codigos Códigos c315 a consultar, separados por vírgula, ou
#'   \code{"all"} para toda a hierarquia. Padrão: \code{"all"}.
#'
#' @return \code{tibble} com as colunas \code{data}, \code{cod},
#'   \code{nome_completo} (rótulo do IBGE, com prefixo numérico),
#'   \code{nome} (rótulo limpo, sem prefixo), \code{nivel}
#'   (\code{"geral"}, \code{"grupo"}, \code{"subgrupo"}, \code{"item"} ou
#'   \code{"subitem"}), \code{variacao} e \code{peso}.
#'
#' @examples
#' \dontrun{
#'   # Toda a hierarquia nos últimos 13 meses
#'   h <- coletar_ipca_c315("202510", "202610")
#'   dplyr::filter(h, nivel == "subitem")
#' }
# -----------------------------------------------------------------------------
coletar_ipca_c315 <- function(
    data_inicio,
    data_fim = format(Sys.Date(), "%Y%m"),
    codigos  = "all"
) {
  .usar_user_agent_navegador()

  periodo <- paste0(data_inicio, "-", data_fim)

  bruto <- sidrar::get_sidra(
    api = paste0(
      "/t/7060/n1/all",
      "/v/63,66",
      "/p/", periodo,
      "/c315/", codigos
    )
  )

  longo <- bruto |>
    dplyr::as_tibble() |>
    dplyr::select(
      periodo       = `Mês (Código)`,
      nome_completo = `Geral, grupo, subgrupo, item e subitem`,
      cod           = `Geral, grupo, subgrupo, item e subitem (Código)`,
      variavel      = `Variável`,
      valor         = Valor
    ) |>
    dplyr::mutate(
      data  = lubridate::ym(periodo),
      cod   = as.character(cod),
      valor = as.double(valor),
      campo = dplyr::if_else(
        grepl("Peso", variavel, fixed = TRUE), "peso", "variacao"
      )
    )

  longo |>
    dplyr::select(data, cod, nome_completo, campo, valor) |>
    tidyr::pivot_wider(names_from = campo, values_from = valor) |>
    dplyr::mutate(
      nivel = .nivel_c315(nome_completo),
      nome  = .rotulo_sem_prefixo(nome_completo)
    ) |>
    dplyr::select(data, cod, nome_completo, nome, nivel, variacao, peso) |>
    dplyr::arrange(data, cod)
}


# -----------------------------------------------------------------------------
#' Coleta o IPCA do índice geral para as 16 áreas pesquisadas pelo IBGE
#'
#' O IPCA é apurado em 16 áreas: 10 regiões metropolitanas (nível territorial
#' \code{n7}) e 6 municípios (nível \code{n6}). As duas consultas são feitas
#' separadamente e normalizadas em uma coluna única \code{area}.
#'
#' @param data_inicio Período inicial no formato \code{"YYYYMM"}.
#' @param data_fim Período final no formato \code{"YYYYMM"}.
#'   Padrão: mês corrente.
#'
#' @return \code{tibble} com \code{data}, \code{area} (\code{character}),
#'   \code{tipo} (\code{"Região metropolitana"} ou \code{"Município"}) e
#'   \code{variacao} (\code{double}).
#'
#' @examples
#' \dontrun{
#'   areas <- coletar_ipca_areas("202610", "202610")
#'   dplyr::arrange(areas, dplyr::desc(variacao))
#' }
# -----------------------------------------------------------------------------
coletar_ipca_areas <- function(
    data_inicio,
    data_fim = format(Sys.Date(), "%Y%m")
) {
  .usar_user_agent_navegador()

  periodo <- paste0(data_inicio, "-", data_fim)

  .consultar <- function(nivel, coluna_area, tipo) {
    bruto <- sidrar::get_sidra(
      api = paste0(
        "/t/7060/", nivel, "/all",
        "/v/63",
        "/p/", periodo,
        "/c315/7169"
      )
    ) |>
      dplyr::as_tibble()

    bruto |>
      dplyr::transmute(
        data     = lubridate::ym(`Mês (Código)`),
        area     = as.character(.data[[coluna_area]]),
        tipo     = tipo,
        variacao = as.double(Valor)
      )
  }

  rm_areas  <- .consultar("n7", "Região Metropolitana até 2020",
                          "Região metropolitana")
  mun_areas <- .consultar("n6", "Município", "Município")

  dplyr::bind_rows(rm_areas, mun_areas) |>
    dplyr::filter(!is.na(variacao)) |>
    dplyr::arrange(data, area)
}


# -----------------------------------------------------------------------------
#' Coleta as expectativas do boletim Focus para o IPCA mensal
#'
#' Consulta a API de expectativas de mercado do BCB e devolve, para cada mês
#' de referência, a projeção mediana mais recente anterior à divulgação do
#' resultado efetivo. Serve para medir a \emph{surpresa inflacionária}:
#' a diferença entre o IPCA realizado e o que o mercado projetava.
#'
#' Usa \code{baseCalculo == 0}, que considera todas as instituições que
#' responderam nos últimos 30 dias (amostra mais ampla).
#'
#' @param data_inicio Data inicial das coletas do Focus ("YYYY-MM-DD").
#'   Padrão: 24 meses atrás.
#'
#' @return \code{tibble} com \code{data} (mês de referência, \code{Date}),
#'   \code{mediana}, \code{media}, \code{n_respondentes} e
#'   \code{data_coleta} (data da última coleta usada).
#'   Devolve \code{NULL} se a API estiver indisponível.
#'
#' @examples
#' \dontrun{
#'   focus <- coletar_focus_ipca()
#' }
# -----------------------------------------------------------------------------
coletar_focus_ipca <- function(
    data_inicio = as.character(Sys.Date() - 760L)
) {
  bruto <- tryCatch(
    rbcb::get_market_expectations(
      type       = "monthly",
      indic      = "IPCA",
      start_date = data_inicio,
      `$top`     = 10000L
    ),
    error = function(e) {
      warning(
        sprintf("[coletar_focus_ipca] Focus indisponível: %s",
                conditionMessage(e)),
        call. = FALSE
      )
      NULL
    }
  )

  if (is.null(bruto) || nrow(bruto) == 0L) return(NULL)

  bruto |>
    dplyr::as_tibble() |>
    dplyr::filter(baseCalculo == 0L) |>
    dplyr::mutate(
      data_coleta = as.Date(Data),
      data        = lubridate::ym(
        paste0(substr(DataReferencia, 4L, 7L), substr(DataReferencia, 1L, 2L))
      )
    ) |>
    # Só interessam meses de referência já divulgados.
    dplyr::filter(data < lubridate::floor_date(Sys.Date(), "month")) |>
    # A projeção válida é a última coletada ANTES da divulgação. O IBGE
    # divulga o IPCA por volta do dia 10 do mês seguinte ao de referência;
    # o corte no dia 8 é conservador e evita contaminar a "expectativa" com
    # revisões feitas depois de o número já ser conhecido.
    dplyr::filter(data_coleta < data %m+% months(1L) + 7L) |>
    dplyr::group_by(data) |>
    dplyr::slice_max(data_coleta, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      data,
      mediana        = as.double(Mediana),
      media          = as.double(Media),
      n_respondentes = as.integer(numeroRespondentes),
      data_coleta
    ) |>
    dplyr::arrange(data)
}
