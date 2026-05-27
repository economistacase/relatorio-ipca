# =============================================================================
# R/graficos.R
# Funções de visualização do painel de inflação (IPCA) — ggplot2
#
# Paleta corporativa:
#   Primária   #282f6b  (azul-marinho)
#   Secundária #d97706  (âmbar)
#   Terciária  #059669  (verde)
#   Cinza      #6b7280
#
# Tema base: theme_minimal(base_size = 11)
#
# Dependências:
#   - ggplot2   >= 3.4.0 (usa `linewidth` em vez de `size` para linhas)
#   - dplyr
#   - lubridate
#   - scales
# =============================================================================

library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

# ── Paleta e tema ─────────────────────────────────────────────────────────────
.COR_PRIMARIA   <- "#282f6b"
.COR_SECUNDARIA <- "#d97706"
.COR_TERCIARIA  <- "#059669"
.COR_CINZA      <- "#6b7280"

.tema_base <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = 12),
      plot.subtitle    = ggplot2::element_text(colour = .COR_CINZA, size = 10),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.y     = ggplot2::element_text(size = 9, colour = .COR_CINZA),
      plot.caption     = ggplot2::element_text(size = 8, colour = .COR_CINZA,
                                               hjust = 0)
    )
}


# -----------------------------------------------------------------------------
#' Gráfico de barras — IPCA variação mensal (últimos 24 meses)
#'
#' Exibe as variações mensais do IPCA dos últimos 24 meses em barras
#' verticais. O eixo-y é truncado via \code{coord_cartesian} (as barras não
#' são cortadas, apenas a área de plotagem é ajustada para dar espaço aos
#' rótulos). Barras positivas recebem a cor primária; negativas, a secundária.
#' O valor numérico de cada barra é anotado com \code{geom_text} acima (ou
#' abaixo, quando negativo).
#'
#' @param df \code{tibble} com ao menos as colunas:
#'   \describe{
#'     \item{data}{\code{Date} — primeiro dia do mês.}
#'     \item{ipca_mm}{\code{double} — variação percentual mensal.}
#'   }
#'   Tipicamente o retorno de \code{coletar_ipca_mensal()}.
#'
#' @return Objeto \code{ggplot}.
#'
#' @examples
#' \dontrun{
#'   ipca <- coletar_ipca_mensal()
#'   grafico_ipca_mensal(ipca)
#' }
# -----------------------------------------------------------------------------
grafico_ipca_mensal <- function(df) {
  stopifnot(
    is.data.frame(df),
    all(c("data", "ipca_mm") %in% names(df))
  )

  # ── Filtro: últimos 24 meses ──────────────────────────────────────────────
  data_max   <- max(df$data, na.rm = TRUE)
  data_corte <- data_max %m-% months(23L)          # 24 meses inclusive

  df_plot <- df |>
    dplyr::filter(data >= data_corte) |>
    dplyr::arrange(data) |>
    dplyr::mutate(positivo = ipca_mm >= 0)

  # ── Limites do eixo-y com folga para os rótulos ───────────────────────────
  amplitude <- diff(range(df_plot$ipca_mm, na.rm = TRUE))
  pad       <- max(amplitude * 0.18, 0.05)         # mínimo de 0,05 p.p.

  ylim_inf <- min(0, min(df_plot$ipca_mm, na.rm = TRUE)) - pad
  ylim_sup <- max(df_plot$ipca_mm, na.rm = TRUE)   + pad

  # ── Gráfico ───────────────────────────────────────────────────────────────
  ggplot2::ggplot(
    df_plot,
    ggplot2::aes(x = data, y = ipca_mm, fill = positivo)
  ) +
    ggplot2::geom_col(width = 20) +                # largura em dias
    ggplot2::geom_text(
      ggplot2::aes(
        label = format(round(ipca_mm, 2L), nsmall = 2L),
        vjust = ifelse(positivo, -0.45, 1.45)
      ),
      size   = 2.6,
      colour = .COR_CINZA,
      fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(
      values = c("TRUE"  = .COR_PRIMARIA,
                 "FALSE" = .COR_SECUNDARIA),
      guide  = "none"
    ) +
    ggplot2::scale_x_date(
      date_breaks = "3 months",
      date_labels = "%b\n%Y",
      expand      = ggplot2::expansion(add = 15)
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 0.01, suffix = "%")
    ) +
    ggplot2::coord_cartesian(ylim = c(ylim_inf, ylim_sup)) +
    ggplot2::labs(
      title    = "IPCA — Variação Mensal",
      subtitle = "Últimos 24 meses  |  em % a.m.",
      x        = NULL,
      y        = NULL,
      caption  = "Fonte: BCB/SGS — Série 433"
    ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3,
                        colour = .COR_CINZA, linetype = "solid") +
    .tema_base() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}


# -----------------------------------------------------------------------------
#' Gráfico de linha — IPCA acumulado em 12 meses vs. meta variável
#'
#' Sobrepõe três camadas:
#' \enumerate{
#'   \item Banda de tolerância: meta ± 1,5 p.p., preenchida em verde com
#'     transparência. Desenhada como retângulos anuais (\code{geom_rect})
#'     para respeitar a mudança da meta ano a ano sem interpolação.
#'   \item Linha da meta central: traço pontilhado verde, também em degraus
#'     anuais (\code{geom_step}).
#'   \item Linha do IPCA acumulado em 12 meses (\code{acum_12m}), em azul.
#' }
#' Uma caixa com o último valor 12m é posicionada no canto superior direito
#' do painel via \code{annotate("label", x = Inf, y = Inf)}.
#'
#' @param df \code{tibble} com ao menos as colunas \code{data} e
#'   \code{acum_12m}. Tipicamente o retorno de
#'   \code{calcular_acumulado_12m()}.
#' @param df_meta \code{tibble} com ao menos as colunas \code{data} e
#'   \code{meta_inflacao} (frequência anual ou mensal). Tipicamente o retorno
#'   de \code{coletar_meta_inflacao()} ou \code{preparar_meta_mensal()}.
#'
#' @return Objeto \code{ggplot}.
#'
#' @examples
#' \dontrun{
#'   df_ipca <- coletar_ipca_mensal() |> calcular_acumulado_12m()
#'   df_meta <- coletar_meta_inflacao()
#'   grafico_ipca_12m(df_ipca, df_meta)
#' }
# -----------------------------------------------------------------------------
grafico_ipca_12m <- function(df, df_meta) {
  stopifnot(
    is.data.frame(df),
    all(c("data", "acum_12m") %in% names(df)),
    is.data.frame(df_meta),
    all(c("data", "meta_inflacao") %in% names(df_meta))
  )

  # ── Normalizar meta: uma linha por ano ────────────────────────────────────
  meta_anual <- df_meta |>
    dplyr::arrange(data) |>
    dplyr::mutate(.ano = lubridate::year(data)) |>
    dplyr::distinct(.ano, .keep_all = TRUE) |>
    dplyr::select(.ano, meta_inflacao)

  # ── Série principal (apenas meses com janela completa) ───────────────────
  df_plot <- df |>
    dplyr::filter(!is.na(acum_12m)) |>
    dplyr::arrange(data) |>
    dplyr::mutate(.ano = lubridate::year(data)) |>
    dplyr::left_join(meta_anual, by = ".ano") |>
    dplyr::select(-.ano)

  # ── Retângulos anuais para a banda meta ± 1,5 ────────────────────────────
  # Cada ano recebe um rect independente → sem interpolação entre metas
  bandas <- df_plot |>
    dplyr::mutate(.ano = lubridate::year(data)) |>
    dplyr::group_by(.ano) |>
    dplyr::summarise(
      x_min = min(data),
      x_max = max(data) + lubridate::days(15L),  # estende até ~meio do mês
      meta  = dplyr::first(meta_inflacao),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(meta))

  # ── Último valor para a caixa de destaque ────────────────────────────────
  ultimo <- dplyr::slice_tail(df_plot, n = 1L)
  label_ultimo <- sprintf(
    "Acum. 12m\n%s%%",
    format(round(ultimo$acum_12m, 2L), nsmall = 2L)
  )

  # ── Gráfico ───────────────────────────────────────────────────────────────
  ggplot2::ggplot(df_plot, ggplot2::aes(x = data)) +

    # Banda meta ± 1,5 p.p.
    ggplot2::geom_rect(
      data         = bandas,
      ggplot2::aes(xmin = x_min, xmax = x_max,
                   ymin = meta - 1.5, ymax = meta + 1.5),
      fill         = .COR_TERCIARIA,
      alpha        = 0.12,
      inherit.aes  = FALSE
    ) +

    # Meta central em degraus anuais
    ggplot2::geom_step(
      ggplot2::aes(y = meta_inflacao),
      colour    = .COR_TERCIARIA,
      linewidth = 0.6,
      linetype  = "dashed",
      na.rm     = TRUE
    ) +

    # IPCA 12m
    ggplot2::geom_line(
      ggplot2::aes(y = acum_12m),
      colour    = .COR_PRIMARIA,
      linewidth = 0.9
    ) +

    # Ponto no último mês
    ggplot2::geom_point(
      data   = ultimo,
      ggplot2::aes(y = acum_12m),
      colour = .COR_PRIMARIA,
      size   = 2.5
    ) +

    # Caixa com o último valor — canto superior direito
    ggplot2::annotate(
      geom          = "label",
      x             = Inf,
      y             = Inf,
      label         = label_ultimo,
      hjust         = 1.07,
      vjust         = 1.25,
      fill          = .COR_PRIMARIA,
      colour        = "white",
      size          = 3.2,
      fontface      = "bold",
      label.padding = ggplot2::unit(0.35, "lines"),
      label.r       = ggplot2::unit(0.2,  "lines"),
      label.size    = NA
    ) +

    ggplot2::scale_x_date(
      date_breaks = "2 years",
      date_labels = "%Y",
      expand      = ggplot2::expansion(mult = c(0.01, 0.02))
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 0.1, suffix = "%")
    ) +
    ggplot2::labs(
      title    = "IPCA — Acumulado em 12 Meses",
      subtitle = "Linha verde tracejada: meta CMN  |  Banda: meta ± 1,5 p.p.",
      x        = NULL,
      y        = NULL,
      caption  = "Fontes: BCB/SGS — Séries 433 e 13521"
    ) +
    .tema_base()
}


# -----------------------------------------------------------------------------
#' Gráfico sazonal — IPCA mensal, uma linha por ano
#'
#' Exibe a variação mensal do IPCA ao longo dos meses do ano (eixo-x: 1–12),
#' desenhando uma linha para cada ano presente em \code{df_saz}. Anos
#' anteriores são desenhados em tons de cinza claro com traço fino; o ano
#' mais recente recebe a cor primária e traço mais espesso, destacando-o
#' visualmente.
#'
#' A legenda é montada via \code{scale_colour_manual}: anos anteriores
#' compartilham gradiente cinza; o ano atual usa \code{.COR_PRIMARIA}. O
#' parâmetro \code{linewidth} (ggplot2 >= 3.4.0) é mapeado via
#' \code{scale_linewidth_manual}, mas sua legenda é suprimida para não
#' duplicar a legenda de cores.
#'
#' @param df_saz \code{tibble} produzido por \code{preparar_sazonal()}, com
#'   ao menos as colunas:
#'   \describe{
#'     \item{mes}{\code{integer} — número do mês (1–12).}
#'     \item{ano}{\code{integer} — ano de referência.}
#'     \item{ipca_mm}{\code{double} — variação percentual mensal.}
#'     \item{ano_label}{\code{character} — rótulo do ano para a legenda.}
#'     \item{destaque}{\code{logical} — \code{TRUE} se for o ano mais recente.}
#'   }
#'
#' @return Objeto \code{ggplot}.
#'
#' @examples
#' \dontrun{
#'   ipca     <- coletar_ipca_mensal()
#'   df_saz   <- preparar_sazonal(ipca, ano_inicio = 2015)
#'   grafico_sazonal(df_saz)
#' }
# -----------------------------------------------------------------------------
grafico_sazonal <- function(df_saz) {
  stopifnot(
    is.data.frame(df_saz),
    all(c("mes", "ano", "ipca_mm", "ano_label", "destaque") %in%
          names(df_saz))
  )

  anos_ord <- sort(unique(df_saz$ano))
  n_anos   <- length(anos_ord)
  ano_max  <- max(anos_ord)

  # ── Paleta: cinzas para anos anteriores, primária para o atual ────────────
  if (n_anos > 1L) {
    cinzas <- grDevices::colorRampPalette(
      c("#d1d5db", "#9ca3af")
    )(n_anos - 1L)
  } else {
    cinzas <- character(0L)
  }
  cores_anos <- stats::setNames(
    c(cinzas, .COR_PRIMARIA),
    as.character(anos_ord)
  )

  # ── Espessuras: fina para passado, grossa para atual ─────────────────────
  espessuras <- stats::setNames(
    c(rep(0.45, n_anos - 1L), 1.2),
    as.character(anos_ord)
  )

  # ── Rótulos dos meses em pt-BR ────────────────────────────────────────────
  meses_br <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                "Jul", "Ago", "Set", "Out", "Nov", "Dez")

  ggplot2::ggplot(
    df_saz,
    ggplot2::aes(
      x         = mes,
      y         = ipca_mm,
      colour    = ano_label,
      linewidth = ano_label,
      group     = ano_label
    )
  ) +
    ggplot2::geom_line(na.rm = TRUE) +

    # Linha de referência: zero
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.25,
                        colour = .COR_CINZA, linetype = "dotted") +

    ggplot2::scale_colour_manual(
      values = cores_anos,
      breaks = as.character(anos_ord),   # ordem crescente na legenda
      name   = "Ano"
    ) +
    ggplot2::scale_linewidth_manual(
      values = espessuras,
      guide  = "none"                    # suprime legenda duplicada
    ) +
    ggplot2::scale_x_continuous(
      breaks = 1:12,
      labels = meses_br,
      expand = ggplot2::expansion(add = 0.3)
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 0.01, suffix = "%")
    ) +
    ggplot2::labs(
      title    = "IPCA — Sazonalidade Mensal",
      subtitle = sprintf(
        "De %d a %d  |  linha mais espessa = ano atual",
        min(anos_ord), ano_max
      ),
      x        = NULL,
      y        = NULL,
      caption  = "Fonte: BCB/SGS — Série 433"
    ) +
    .tema_base() +
    ggplot2::theme(
      legend.position  = "right",
      legend.key.width = ggplot2::unit(1.5, "lines"),
      panel.grid.major.x = ggplot2::element_blank()
    )
}


# -----------------------------------------------------------------------------
#' Gráfico de barras horizontais — contribuições dos grupos ao IPCA
#'
#' Exibe a contribuição de cada grupo de despesa ao IPCA do mês em barras
#' horizontais ordenadas da maior para a menor contribuição. A contribuição
#' é calculada como:
#' \deqn{\text{contribuicao}_i = \frac{\text{variacao}_i \times
#'   \text{peso}_i}{100}}
#' Se a coluna \code{contribuicao} já existir no \code{tibble} de entrada,
#' ela é usada diretamente; caso contrário, é calculada a partir de
#' \code{variacao} e \code{peso}.
#'
#' Barras positivas recebem a cor primária; negativas, a secundária. O
#' rótulo em p.p. é posicionado à direita (contribuição positiva) ou à
#' esquerda (negativa) da barra para evitar sobreposição.
#'
#' @param df \code{tibble} com ao menos as colunas:
#'   \describe{
#'     \item{grupo}{\code{character} — rótulo do grupo de despesa.}
#'     \item{contribuicao}{\code{double} — contribuição em p.p. \emph{ou}}
#'     \item{variacao + peso}{\code{double} — usados para calcular a
#'       contribuição quando \code{contribuicao} não está presente.}
#'   }
#'   Tipicamente \code{preparar_contribuicoes(df_grupos)$mes_atual}.
#'
#' @return Objeto \code{ggplot}.
#'
#' @examples
#' \dontrun{
#'   grupos  <- coletar_ipca_grupos()
#'   contrib <- preparar_contribuicoes(grupos)
#'   grafico_contribuicoes(contrib$mes_atual)
#' }
# -----------------------------------------------------------------------------
grafico_contribuicoes <- function(df) {
  stopifnot(is.data.frame(df), "grupo" %in% names(df))

  # ── Garantir coluna contribuicao ──────────────────────────────────────────
  if (!"contribuicao" %in% names(df)) {
    stopifnot(all(c("variacao", "peso") %in% names(df)))
    df <- dplyr::mutate(df, contribuicao = variacao * peso / 100)
  }

  # ── Ordenar grupos pela contribuição (maior → topo) ───────────────────────
  df_plot <- df |>
    dplyr::mutate(
      grupo    = reorder(grupo, contribuicao),   # base R, sem forcats
      positivo = contribuicao >= 0
    )

  # ── Limites do eixo-x com folga para rótulos ─────────────────────────────
  amp   <- diff(range(df_plot$contribuicao, na.rm = TRUE))
  pad_x <- max(amp * 0.22, 0.02)

  xlim_inf <- min(0, min(df_plot$contribuicao, na.rm = TRUE)) - pad_x
  xlim_sup <- max(0, max(df_plot$contribuicao, na.rm = TRUE)) + pad_x

  # ── Referência temporal para o título ────────────────────────────────────
  subtitulo <- if ("data" %in% names(df)) {
    data_ref <- max(df$data, na.rm = TRUE)
    format(data_ref, "Referência: %b/%Y")
  } else {
    "Contribuição em pontos percentuais (p.p.)"
  }

  # ── Gráfico ───────────────────────────────────────────────────────────────
  ggplot2::ggplot(
    df_plot,
    ggplot2::aes(x = contribuicao, y = grupo, fill = positivo)
  ) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = sprintf("%+.2f p.p.", contribuicao),
        hjust = ifelse(positivo, -0.12, 1.12)
      ),
      size     = 3.1,
      colour   = .COR_CINZA,
      fontface = "bold"
    ) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.35,
                        colour = .COR_CINZA) +
    ggplot2::scale_fill_manual(
      values = c("TRUE"  = .COR_PRIMARIA,
                 "FALSE" = .COR_SECUNDARIA),
      guide  = "none"
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(accuracy = 0.01, suffix = " p.p.",
                                    style_positive = "plus")
    ) +
    ggplot2::coord_cartesian(xlim = c(xlim_inf, xlim_sup)) +
    ggplot2::labs(
      title    = "Contribuição dos Grupos ao IPCA",
      subtitle = subtitulo,
      x        = "Contribuição (p.p.)",
      y        = NULL,
      caption  = "Fonte: IBGE/SIDRA — Tabela 7060"
    ) +
    .tema_base() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank()
    )
}


# -----------------------------------------------------------------------------
#' Salva um objeto ggplot em disco (diretório output/)
#'
#' Função auxiliar usada pelo relatório Quarto para persistir cada gráfico
#' como arquivo PNG em \code{output/}. O diretório é criado automaticamente
#' caso não exista.
#'
#' @param plot Objeto \code{ggplot} a ser salvo.
#' @param nome \code{character} — nome do arquivo \emph{sem extensão}
#'   (ex.: \code{"ipca_mensal"}). O arquivo será gravado como
#'   \code{output/<nome>.png}.
#' @param largura \code{numeric} — largura em polegadas. Padrão: \code{9}.
#' @param altura \code{numeric} — altura em polegadas. Padrão: \code{5}.
#' @param dpi \code{numeric} — resolução em pontos por polegada.
#'   Padrão: \code{150}.
#' @param dir \code{character} — caminho do diretório de saída.
#'   Padrão: \code{"output"}.
#'
#' @return Invisível: caminho completo do arquivo salvo (\code{character}).
#'
#' @examples
#' \dontrun{
#'   g <- grafico_ipca_mensal(ipca_raw)
#'   salvar_grafico(g, "ipca_mensal")
#'   # → output/ipca_mensal.png
#' }
# -----------------------------------------------------------------------------
salvar_grafico <- function(plot,
                           nome,
                           largura = 9,
                           altura  = 5,
                           dpi     = 150,
                           dir     = "output") {
  stopifnot(inherits(plot, "ggplot"), is.character(nome), nchar(nome) > 0L)

  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    message(sprintf("[salvar_grafico] Diretório '%s' criado.", dir))
  }

  caminho <- file.path(dir, paste0(nome, ".png"))

  ggplot2::ggsave(
    filename = caminho,
    plot     = plot,
    width    = largura,
    height   = altura,
    dpi      = dpi,
    bg       = "white"
  )

  message(sprintf("[salvar_grafico] Salvo: %s", caminho))
  invisible(caminho)
}
