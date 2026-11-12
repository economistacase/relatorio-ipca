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


# =============================================================================
# Tratamento das análises complementares: núcleos, classificação por
# durabilidade, difusão, surpresa frente ao Focus, abertura por item e
# desagregação regional.
# =============================================================================


# -----------------------------------------------------------------------------
#' Acumula em 12 meses uma ou mais séries empilhadas
#'
#' Versão generalizada de \code{calcular_acumulado_12m()} para \code{tibbles}
#' no formato longo, com várias séries identificadas por uma coluna de grupo.
#' Aplica o mesmo encadeamento multiplicativo, por grupo.
#'
#' @param df \code{tibble} longo com colunas de data, grupo e valor.
#' @param col_grupo Nome da coluna que identifica a série.
#'   Padrão: \code{"serie"}.
#' @param col_valor Nome da coluna com a variação mensal.
#'   Padrão: \code{"valor"}.
#'
#' @return O mesmo \code{tibble}, acrescido da coluna \code{acum_12m}
#'   (\code{NA} nos 11 primeiros meses de cada série).
#'
#' @examples
#' \dontrun{
#'   nucleos <- coletar_series_sgs(NUCLEOS_SGS)
#'   acumular_12m_por_serie(nucleos)
#' }
# -----------------------------------------------------------------------------
acumular_12m_por_serie <- function(
    df,
    col_grupo = "serie",
    col_valor = "valor"
) {
  stopifnot(
    is.data.frame(df),
    all(c("data", col_grupo, col_valor) %in% names(df))
  )

  df |>
    dplyr::arrange(.data[[col_grupo]], data) |>
    dplyr::group_by(.data[[col_grupo]]) |>
    dplyr::mutate(
      acum_12m = slider::slide_dbl(
        .x        = .data[[col_valor]],
        .f        = ~ (prod(1 + .x / 100) - 1) * 100,
        .before   = 11L,
        .complete = TRUE
      )
    ) |>
    dplyr::ungroup()
}


# -----------------------------------------------------------------------------
#' Prepara os cinco núcleos de inflação e sua média
#'
#' Calcula, mês a mês, a média simples dos cinco núcleos acompanhados pelo
#' BCB e acrescenta essa média como uma série adicional. Em seguida acumula
#' todas as séries em 12 meses.
#'
#' A média simples dos cinco núcleos é a medida usualmente citada em análises
#' de conjuntura — é ela que o mercado reporta como "a média dos núcleos".
#'
#' @param df_nucleos \code{tibble} longo devolvido por
#'   \code{coletar_series_sgs(NUCLEOS_SGS)}, com colunas \code{serie},
#'   \code{data} e \code{valor}.
#'
#' @return Lista com três elementos:
#'   \describe{
#'     \item{series}{\code{tibble} longo com os cinco núcleos mais a série
#'       \code{"Média dos núcleos"}, com \code{acum_12m}.}
#'     \item{media}{\code{tibble} apenas com a série da média.}
#'     \item{mes_atual}{\code{tibble} com os valores do mês mais recente.}
#'   }
#'
#' @examples
#' \dontrun{
#'   nuc <- preparar_nucleos(coletar_series_sgs(NUCLEOS_SGS))
#'   nuc$mes_atual
#' }
# -----------------------------------------------------------------------------
preparar_nucleos <- function(df_nucleos) {
  stopifnot(
    is.data.frame(df_nucleos),
    all(c("serie", "data", "valor") %in% names(df_nucleos))
  )

  # Só meses com os cinco núcleos disponíveis entram na média, para que a
  # série da média não oscile por mudança de composição.
  n_esperado <- dplyr::n_distinct(df_nucleos$serie)

  media <- df_nucleos |>
    dplyr::group_by(data) |>
    dplyr::summarise(
      # n_itens vem antes: em summarise() as expressões são avaliadas em
      # sequência, e calcular `valor` primeiro faria `n_itens` contar a
      # própria média (sempre 1) em vez dos núcleos de origem.
      n_itens = sum(!is.na(valor)),
      valor   = mean(valor, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(n_itens == n_esperado) |>
    dplyr::transmute(
      serie  = "Média dos núcleos",
      codigo = NA_integer_,
      data,
      valor
    )

  series <- dplyr::bind_rows(df_nucleos, media) |>
    acumular_12m_por_serie()

  data_max <- max(series$data, na.rm = TRUE)

  list(
    series    = series,
    media     = dplyr::filter(series, serie == "Média dos núcleos"),
    mes_atual = dplyr::filter(series, data == data_max)
  )
}


# -----------------------------------------------------------------------------
#' Confronta o IPCA realizado com a mediana projetada no boletim Focus
#'
#' A \emph{surpresa inflacionária} é a diferença, em pontos percentuais,
#' entre o IPCA efetivamente divulgado e a mediana das projeções de mercado
#' coletadas imediatamente antes da divulgação. Surpresa positiva significa
#' inflação acima do esperado.
#'
#' @param df_focus \code{tibble} devolvido por \code{coletar_focus_ipca()},
#'   ou \code{NULL} se o Focus estiver indisponível.
#' @param df_ipca \code{tibble} com \code{data} e \code{ipca_mm}.
#'
#' @return \code{tibble} com \code{data}, \code{realizado}, \code{projetado},
#'   \code{surpresa}, \code{n_respondentes} e \code{data_coleta}, ordenado
#'   por data. Devolve \code{NULL} se \code{df_focus} for \code{NULL}.
#'
#' @examples
#' \dontrun{
#'   preparar_surpresa_focus(coletar_focus_ipca(), coletar_ipca_mensal())
#' }
# -----------------------------------------------------------------------------
preparar_surpresa_focus <- function(df_focus, df_ipca) {
  if (is.null(df_focus) || nrow(df_focus) == 0L) return(NULL)

  stopifnot(
    is.data.frame(df_ipca),
    all(c("data", "ipca_mm") %in% names(df_ipca))
  )

  df_focus |>
    dplyr::inner_join(
      dplyr::select(df_ipca, data, ipca_mm),
      by = "data"
    ) |>
    dplyr::transmute(
      data,
      realizado = ipca_mm,
      projetado = mediana,
      surpresa  = ipca_mm - mediana,
      n_respondentes,
      data_coleta
    ) |>
    dplyr::arrange(data)
}


# -----------------------------------------------------------------------------
#' Seleciona os itens de maior impacto no IPCA do mês
#'
#' Calcula a contribuição em pontos percentuais de cada elemento da
#' hierarquia c315 (\code{contribuição = variação × peso / 100}) no mês mais
#' recente e devolve os \code{n} de maior impacto positivo e os \code{n} de
#' maior impacto negativo.
#'
#' Ordenar por contribuição, e não por variação, corrige a distorção do
#' ranking usual: um subitem pode subir 20% e mover o índice em 0,06 p.p.,
#' enquanto outro sobe 2% e move 0,10 p.p. por ter peso muito maior.
#'
#' @param df_c315 \code{tibble} devolvido por \code{coletar_ipca_c315()}.
#' @param n Quantidade de itens em cada direção. Padrão: 8.
#' @param nivel Nível hierárquico a considerar. Padrão: \code{"subitem"}.
#'
#' @return Lista com:
#'   \describe{
#'     \item{altas}{\code{tibble} com os \code{n} maiores impactos positivos.}
#'     \item{baixas}{\code{tibble} com os \code{n} maiores impactos negativos.}
#'     \item{combinado}{\code{tibble} com ambos, para gráfico divergente.}
#'     \item{data_ref}{\code{Date} do mês de referência.}
#'   }
#'
#' @examples
#' \dontrun{
#'   h <- coletar_ipca_c315("202510", "202610")
#'   top <- preparar_top_itens(h, n = 8)
#'   top$altas
#' }
# -----------------------------------------------------------------------------
preparar_top_itens <- function(df_c315, n = 8L, nivel = "subitem") {
  stopifnot(
    is.data.frame(df_c315),
    all(c("data", "nome", "nivel", "variacao", "peso") %in% names(df_c315)),
    is.numeric(n), length(n) == 1L, n >= 1L
  )

  data_ref <- max(df_c315$data, na.rm = TRUE)

  base <- df_c315 |>
    dplyr::filter(nivel == !!nivel, data == data_ref) |>
    dplyr::mutate(contribuicao = variacao * peso / 100) |>
    dplyr::filter(!is.na(contribuicao))

  altas <- base |>
    dplyr::filter(contribuicao > 0) |>
    dplyr::slice_max(contribuicao, n = n, with_ties = FALSE) |>
    dplyr::mutate(direcao = "Pressão de alta")

  baixas <- base |>
    dplyr::filter(contribuicao < 0) |>
    dplyr::slice_min(contribuicao, n = n, with_ties = FALSE) |>
    dplyr::mutate(direcao = "Pressão de baixa")

  combinado <- dplyr::bind_rows(altas, baixas) |>
    dplyr::arrange(contribuicao) |>
    dplyr::mutate(nome = factor(nome, levels = unique(nome)))

  list(
    altas     = altas,
    baixas    = baixas,
    combinado = combinado,
    data_ref  = data_ref
  )
}


# -----------------------------------------------------------------------------
#' Isola a abertura de Alimentação: dentro e fora do domicílio
#'
#' Extrai os subgrupos 7171 (Alimentação no domicílio) e 7432 (Alimentação
#' fora do domicílio) da hierarquia c315. A distinção importa porque os dois
#' respondem a choques diferentes — o primeiro a safra, câmbio e clima; o
#' segundo, muito mais inercial, a salários e aluguel.
#'
#' @param df_c315 \code{tibble} devolvido por \code{coletar_ipca_c315()}.
#'
#' @return \code{tibble} com \code{data}, \code{nome}, \code{variacao} e
#'   \code{peso}, apenas para os dois subgrupos.
#'
#' @examples
#' \dontrun{
#'   preparar_alimentacao(coletar_ipca_c315("202510", "202610"))
#' }
# -----------------------------------------------------------------------------
preparar_alimentacao <- function(df_c315) {
  stopifnot(
    is.data.frame(df_c315),
    all(c("data", "cod", "nome", "variacao", "peso") %in% names(df_c315))
  )

  df_c315 |>
    dplyr::filter(cod %in% c("7171", "7432")) |>
    dplyr::select(data, cod, nome, variacao, peso) |>
    dplyr::arrange(data, cod)
}


# -----------------------------------------------------------------------------
#' Ordena as 16 áreas pesquisadas pelo IPCA do mês de referência
#'
#' @param df_areas \code{tibble} devolvido por \code{coletar_ipca_areas()}.
#' @param data_ref \code{Date} do mês a recortar. Padrão: o mais recente.
#'
#' @return \code{tibble} com \code{area}, \code{tipo} e \code{variacao},
#'   ordenado de forma decrescente, com \code{area} convertida em
#'   \code{factor} na ordem do ranking (pronta para \code{geom_col}).
#'
#' @examples
#' \dontrun{
#'   preparar_areas(coletar_ipca_areas("202610", "202610"))
#' }
# -----------------------------------------------------------------------------
preparar_areas <- function(df_areas, data_ref = NULL) {
  stopifnot(
    is.data.frame(df_areas),
    all(c("data", "area", "tipo", "variacao") %in% names(df_areas))
  )

  if (is.null(data_ref)) data_ref <- max(df_areas$data, na.rm = TRUE)

  df_areas |>
    dplyr::filter(data == data_ref) |>
    dplyr::arrange(variacao) |>
    dplyr::mutate(area = factor(area, levels = unique(area)))
}
