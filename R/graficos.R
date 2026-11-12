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


# =============================================================================
# Visualizações das análises complementares
#
# Paleta categórica (séries simultâneas de identidades diferentes):
#   #4954a8  azul     — irmão mais claro do azul-marinho da marca
#   #c2410c  terracota
#   #0d9488  teal
#   #a21caf  magenta
#
# A paleta foi validada para daltonismo: a pior separação entre cores
# adjacentes é ΔE 13,6 (deuteranopia) e 27,1 em visão normal, acima do piso
# de 8. O azul-marinho da marca (#282f6b) é escuro demais para conviver com
# outras cores em um mesmo gráfico (fica fora da banda de luminosidade), por
# isso segue reservado para séries únicas e para o destaque — papel que já
# cumpre nos gráficos das seções anteriores.
#
# Codificação por sinal (alta/baixa) continua usando o par azul-marinho /
# âmbar já estabelecido no relatório, com cinza como neutro.
# =============================================================================

.PALETA_CATEGORICA <- c("#4954a8", "#c2410c", "#0d9488", "#a21caf")


# -----------------------------------------------------------------------------
#' Afasta verticalmente rótulos que se sobreporiam
#'
#' Quando duas séries terminam em valores próximos, seus rótulos diretos
#' colidem e viram um borrão ilegível. Esta função desloca os rótulos o
#' mínimo necessário para garantir um espaçamento vertical, preservando a
#' ordem original dos valores — o rótulo continua apontando para a série
#' certa, apenas deslocado.
#'
#' Evita a dependência de \pkg{ggrepel}, que não faz parte do projeto nem
#' está instalada no runner de CI.
#'
#' @param valores \code{double} — posições verticais desejadas.
#' @param gap Espaçamento mínimo entre rótulos, na unidade dos dados.
#'
#' @return \code{double} do mesmo comprimento e na mesma ordem de
#'   \code{valores}, com as posições ajustadas.
#'
#' @keywords internal
# -----------------------------------------------------------------------------
.espacar_rotulos <- function(valores, gap) {
  if (length(valores) <= 1L || !is.finite(gap) || gap <= 0) return(valores)

  ordem     <- order(valores)
  ajustados <- valores[ordem]

  for (i in seq_along(ajustados)[-1L]) {
    if (ajustados[i] - ajustados[i - 1L] < gap) {
      ajustados[i] <- ajustados[i - 1L] + gap
    }
  }

  # Recentra o bloco para não empurrar tudo para cima
  deslocamento <- mean(ajustados) - mean(valores)
  ajustados    <- ajustados - deslocamento

  resultado        <- numeric(length(valores))
  resultado[ordem] <- ajustados
  resultado
}


# -----------------------------------------------------------------------------
#' Gráfico divergente — itens de maior impacto no IPCA do mês
#'
#' Barras horizontais ordenadas por contribuição em pontos percentuais, com
#' as pressões de alta (azul-marinho) acima e as de baixa (âmbar) abaixo de
#' um eixo zero. Cada barra recebe rótulo direto com o valor, de modo que a
#' identidade não dependa apenas da cor.
#'
#' @param df \code{tibble} \code{$combinado} devolvido por
#'   \code{preparar_top_itens()}, com \code{nome} (\code{factor} já ordenado),
#'   \code{contribuicao}, \code{variacao} e \code{direcao}.
#'
#' @return Objeto \code{ggplot}.
#'
#' @examples
#' \dontrun{
#'   top <- preparar_top_itens(coletar_ipca_c315("202510", "202610"))
#'   grafico_top_itens(top$combinado)
#' }
# -----------------------------------------------------------------------------
grafico_top_itens <- function(df) {
  stopifnot(
    is.data.frame(df),
    all(c("nome", "contribuicao", "variacao") %in% names(df))
  )

  df_plot <- dplyr::mutate(df, positivo = contribuicao >= 0)

  folga <- max(abs(df_plot$contribuicao), na.rm = TRUE) * 0.45

  ggplot2::ggplot(
    df_plot,
    ggplot2::aes(x = contribuicao, y = nome, fill = positivo)
  ) +
    ggplot2::geom_col(width = 0.68) +
    ggplot2::geom_vline(xintercept = 0, colour = .COR_CINZA, linewidth = 0.4) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = sprintf("%+.3f p.p. (%+.2f%%)", contribuicao, variacao),
        hjust = ifelse(positivo, -0.08, 1.08)
      ),
      size     = 2.5,
      colour   = .COR_CINZA,
      fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = .COR_PRIMARIA, "FALSE" = .COR_SECUNDARIA),
      guide  = "none"
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(accuracy = 0.01),
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    ggplot2::coord_cartesian(xlim = c(-folga, folga) +
                               range(df_plot$contribuicao, na.rm = TRUE)) +
    ggplot2::labs(
      x = "Contribuição ao IPCA do mês (p.p.)",
      y = NULL
    ) +
    .tema_base() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y        = ggplot2::element_text(size = 8.5)
    )
}


# -----------------------------------------------------------------------------
#' Gráfico de linhas — núcleos de inflação acumulados em 12 meses
#'
#' Os cinco núcleos aparecem em cinza, recessivos, e a média deles em
#' azul-marinho com traço espesso — o mesmo recurso de destaque usado no
#' gráfico sazonal. Isso evita uma paleta de seis cores para séries que o
#' leitor consulta como um feixe, e não individualmente.
#'
#' Uma faixa de referência marca o intervalo de tolerância da meta, para
#' leitura imediata de quanto o miolo da inflação está distante do alvo.
#'
#' @param df_series \code{tibble} \code{$series} de \code{preparar_nucleos()},
#'   com \code{serie}, \code{data} e \code{acum_12m}.
#' @param meta_valor Meta central vigente, em p.p. \code{NA} omite a faixa.
#' @param anos_recentes Quantos anos exibir. Padrão: 6.
#'
#' @return Objeto \code{ggplot}.
#'
#' @examples
#' \dontrun{
#'   nuc <- preparar_nucleos(coletar_series_sgs(NUCLEOS_SGS))
#'   grafico_nucleos(nuc$series, meta_valor = 3)
#' }
# -----------------------------------------------------------------------------
grafico_nucleos <- function(df_series, meta_valor = NA_real_,
                            anos_recentes = 6L) {
  stopifnot(
    is.data.frame(df_series),
    all(c("serie", "data", "acum_12m") %in% names(df_series))
  )

  data_max   <- max(df_series$data, na.rm = TRUE)
  data_corte <- data_max %m-% months(12L * as.integer(anos_recentes))

  df_plot <- df_series |>
    dplyr::filter(!is.na(acum_12m), data >= data_corte) |>
    dplyr::mutate(eh_media = serie == "Média dos núcleos")

  individuais <- dplyr::filter(df_plot, !eh_media)
  media       <- dplyr::filter(df_plot,  eh_media)

  rotulo_final <- media |>
    dplyr::slice_max(data, n = 1L, with_ties = FALSE)

  g <- ggplot2::ggplot()

  if (!is.na(meta_valor)) {
    g <- g +
      ggplot2::annotate(
        "rect",
        xmin = min(df_plot$data), xmax = max(df_plot$data),
        ymin = meta_valor - 1.5,  ymax = meta_valor + 1.5,
        fill = .COR_TERCIARIA, alpha = 0.08
      ) +
      ggplot2::geom_hline(
        yintercept = meta_valor,
        colour     = .COR_TERCIARIA,
        linetype   = "dashed",
        linewidth  = 0.5
      )
  }

  g +
    ggplot2::geom_line(
      data = individuais,
      ggplot2::aes(x = data, y = acum_12m, group = serie),
      colour    = .COR_CINZA,
      alpha     = 0.45,
      linewidth = 0.5
    ) +
    ggplot2::geom_line(
      data = media,
      ggplot2::aes(x = data, y = acum_12m),
      colour    = .COR_PRIMARIA,
      linewidth = 1.2
    ) +
    ggplot2::geom_point(
      data   = rotulo_final,
      ggplot2::aes(x = data, y = acum_12m),
      colour = .COR_PRIMARIA,
      size   = 2.2
    ) +
    ggplot2::geom_text(
      data = rotulo_final,
      ggplot2::aes(x = data, y = acum_12m,
                   label = sprintf("%.2f%%", acum_12m)),
      hjust    = -0.18,
      size     = 3,
      colour   = .COR_PRIMARIA,
      fontface = "bold"
    ) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand      = ggplot2::expansion(mult = c(0.02, 0.10))
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 0.1, suffix = "%")
    ) +
    ggplot2::labs(
      x       = NULL,
      y       = "Acumulado em 12 meses",
      caption = paste0(
        "Linhas cinza: os cinco núcleos (MS, EX0, EX3, DP, P55). ",
        "Linha azul: média simples dos cinco."
      )
    ) +
    .tema_base()
}


# -----------------------------------------------------------------------------
#' Gráfico de linhas — abertura do IPCA por natureza do bem
#'
#' Compara, no acumulado em 12 meses, as quatro categorias em que o BCB
#' decompõe os preços do IPCA: bens não duráveis, semiduráveis, duráveis e
#' serviços. Usa a paleta categórica validada, com legenda e rótulo direto
#' no fim de cada linha — identidade nunca depende só da cor.
#'
#' @param df \code{tibble} longo com \code{serie}, \code{data} e
#'   \code{acum_12m}.
#' @param anos_recentes Quantos anos exibir. Padrão: 6.
#'
#' @return Objeto \code{ggplot}.
#'
#' @examples
#' \dontrun{
#'   cl <- acumular_12m_por_serie(coletar_series_sgs(CLASSIFICACAO_SGS))
#'   grafico_classificacao(cl)
#' }
# -----------------------------------------------------------------------------
grafico_classificacao <- function(df, anos_recentes = 6L) {
  stopifnot(
    is.data.frame(df),
    all(c("serie", "data", "acum_12m") %in% names(df))
  )

  data_max   <- max(df$data, na.rm = TRUE)
  data_corte <- data_max %m-% months(12L * as.integer(anos_recentes))

  df_plot <- dplyr::filter(df, !is.na(acum_12m), data >= data_corte)

  gap <- diff(range(df_plot$acum_12m, na.rm = TRUE)) * 0.045

  finais <- df_plot |>
    dplyr::group_by(serie) |>
    dplyr::slice_max(data, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(y_rotulo = .espacar_rotulos(acum_12m, gap))

  ggplot2::ggplot(
    df_plot,
    ggplot2::aes(x = data, y = acum_12m, colour = serie)
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = .COR_CINZA,
                        linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.85) +
    ggplot2::geom_point(data = finais, size = 2) +
    ggplot2::geom_text(
      data = finais,
      ggplot2::aes(y = y_rotulo, label = sprintf("%.1f%%", acum_12m)),
      hjust    = -0.22,
      size     = 2.8,
      fontface = "bold",
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = .PALETA_CATEGORICA, name = NULL) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand      = ggplot2::expansion(mult = c(0.02, 0.12))
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 1, suffix = "%")
    ) +
    ggplot2::labs(x = NULL, y = "Acumulado em 12 meses") +
    .tema_base() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text     = ggplot2::element_text(size = 8.5)
    )
}


# -----------------------------------------------------------------------------
#' Gráfico de linha — índice de difusão do IPCA
#'
#' O índice de difusão mede a proporção de subitens com variação positiva no
#' mês. Serve para distinguir inflação concentrada em poucos itens de
#' inflação disseminada: um índice cheio baixo com difusão alta indica que o
#' alívio veio de poucos preços muito negativos, não de arrefecimento geral.
#'
#' A linha de referência em 50% separa os regimes — acima, a maioria dos
#' preços sobe.
#'
#' @param df \code{tibble} com \code{data} e \code{valor} (difusão em %).
#' @param anos_recentes Quantos anos exibir. Padrão: 6.
#'
#' @return Objeto \code{ggplot}.
#'
#' @examples
#' \dontrun{
#'   dif <- coletar_series_sgs(c("Difusão" = DIFUSAO_SGS))
#'   grafico_difusao(dif)
#' }
# -----------------------------------------------------------------------------
grafico_difusao <- function(df, anos_recentes = 6L) {
  stopifnot(
    is.data.frame(df),
    all(c("data", "valor") %in% names(df))
  )

  data_max   <- max(df$data, na.rm = TRUE)
  data_corte <- data_max %m-% months(12L * as.integer(anos_recentes))

  df_plot <- dplyr::filter(df, !is.na(valor), data >= data_corte)

  final <- dplyr::slice_max(df_plot, data, n = 1L, with_ties = FALSE)

  ggplot2::ggplot(df_plot, ggplot2::aes(x = data, y = valor)) +
    ggplot2::geom_hline(
      yintercept = 50,
      colour     = .COR_CINZA,
      linetype   = "dashed",
      linewidth  = 0.5
    ) +
    ggplot2::geom_line(colour = .COR_PRIMARIA, linewidth = 0.9) +
    ggplot2::geom_point(data = final, colour = .COR_PRIMARIA, size = 2.2) +
    ggplot2::geom_text(
      data = final,
      ggplot2::aes(label = sprintf("%.1f%%", valor)),
      hjust    = -0.2,
      size     = 3,
      colour   = .COR_PRIMARIA,
      fontface = "bold"
    ) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand      = ggplot2::expansion(mult = c(0.02, 0.10))
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 1, suffix = "%")
    ) +
    ggplot2::labs(
      x       = NULL,
      y       = "Subitens com variação positiva",
      caption = "Linha tracejada em 50%: metade dos subitens subindo."
    ) +
    .tema_base()
}


# -----------------------------------------------------------------------------
#' Gráfico de barras — surpresa inflacionária frente ao Focus
#'
#' Cada barra é a diferença, em pontos percentuais, entre o IPCA divulgado e
#' a mediana projetada pelo mercado na última coleta do Focus anterior à
#' divulgação. Barras acima de zero (azul-marinho) indicam inflação acima do
#' esperado; abaixo (âmbar), abaixo do esperado.
#'
#' @param df \code{tibble} devolvido por \code{preparar_surpresa_focus()}.
#' @param n_meses Quantos meses exibir. Padrão: 12.
#'
#' @return Objeto \code{ggplot}.
#'
#' @examples
#' \dontrun{
#'   s <- preparar_surpresa_focus(coletar_focus_ipca(), coletar_ipca_mensal())
#'   grafico_surpresa(s)
#' }
# -----------------------------------------------------------------------------
grafico_surpresa <- function(df, n_meses = 12L) {
  stopifnot(
    is.data.frame(df),
    all(c("data", "surpresa") %in% names(df))
  )

  df_plot <- df |>
    dplyr::arrange(data) |>
    dplyr::slice_tail(n = as.integer(n_meses)) |>
    dplyr::mutate(positivo = surpresa >= 0)

  amplitude <- diff(range(df_plot$surpresa, na.rm = TRUE))
  pad       <- max(amplitude * 0.22, 0.03)

  ggplot2::ggplot(
    df_plot,
    ggplot2::aes(x = data, y = surpresa, fill = positivo)
  ) +
    ggplot2::geom_col(width = 20) +
    ggplot2::geom_hline(yintercept = 0, colour = .COR_CINZA,
                        linewidth = 0.4) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = sprintf("%+.2f", surpresa),
        vjust = ifelse(positivo, -0.45, 1.45)
      ),
      size     = 2.6,
      colour   = .COR_CINZA,
      fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = .COR_PRIMARIA, "FALSE" = .COR_SECUNDARIA),
      guide  = "none"
    ) +
    ggplot2::scale_x_date(
      date_breaks = "2 months",
      date_labels = "%b\n%Y",
      expand      = ggplot2::expansion(add = 18)
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 0.01)
    ) +
    ggplot2::coord_cartesian(
      ylim = range(df_plot$surpresa, na.rm = TRUE) + c(-pad, pad)
    ) +
    ggplot2::labs(
      x       = NULL,
      y       = "Realizado − projetado (p.p.)",
      caption = paste0(
        "Projeção: mediana do Focus na última coleta anterior à divulgação. ",
        "Acima de zero, inflação surpreendeu para cima."
      )
    ) +
    .tema_base()
}


# -----------------------------------------------------------------------------
#' Gráfico de barras — IPCA do mês nas 16 áreas pesquisadas
#'
#' Ranking horizontal das 10 regiões metropolitanas e 6 municípios em que o
#' IBGE apura o IPCA, com codificação por sinal e rótulo direto em cada
#' barra. A dispersão entre áreas revela quanto do resultado nacional é
#' choque local (tarifa de energia, transporte urbano) e quanto é geral.
#'
#' @param df \code{tibble} devolvido por \code{preparar_areas()}, com
#'   \code{area} (\code{factor} ordenado), \code{tipo} e \code{variacao}.
#' @param ipca_brasil Variação do índice cheio no mês, em %, para traçar a
#'   linha de referência nacional. \code{NA} omite a linha.
#'
#' @return Objeto \code{ggplot}.
#'
#' @examples
#' \dontrun{
#'   a <- preparar_areas(coletar_ipca_areas("202610", "202610"))
#'   grafico_areas(a, ipca_brasil = 0.14)
#' }
# -----------------------------------------------------------------------------
grafico_areas <- function(df, ipca_brasil = NA_real_) {
  stopifnot(
    is.data.frame(df),
    all(c("area", "variacao") %in% names(df))
  )

  df_plot <- dplyr::mutate(df, positivo = variacao >= 0)

  folga <- max(abs(df_plot$variacao), na.rm = TRUE) * 0.30

  g <- ggplot2::ggplot(
    df_plot,
    ggplot2::aes(x = variacao, y = area, fill = positivo)
  ) +
    ggplot2::geom_col(width = 0.68)

  if (!is.na(ipca_brasil)) {
    g <- g +
      ggplot2::geom_vline(
        xintercept = ipca_brasil,
        colour     = .COR_TERCIARIA,
        linetype   = "dashed",
        linewidth  = 0.6
      )
  }

  g +
    ggplot2::geom_vline(xintercept = 0, colour = .COR_CINZA,
                        linewidth = 0.4) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = sprintf("%+.2f%%", variacao),
        hjust = ifelse(positivo, -0.12, 1.12)
      ),
      size     = 2.6,
      colour   = .COR_CINZA,
      fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = .COR_PRIMARIA, "FALSE" = .COR_SECUNDARIA),
      guide  = "none"
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_number(accuracy = 0.1, suffix = "%")
    ) +
    ggplot2::coord_cartesian(
      xlim = range(df_plot$variacao, na.rm = TRUE) + c(-folga, folga)
    ) +
    ggplot2::labs(
      x       = "Variação no mês",
      y       = NULL,
      caption = if (is.na(ipca_brasil)) NULL else
        "Linha verde tracejada: IPCA do Brasil no mês."
    ) +
    .tema_base() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y        = ggplot2::element_text(size = 8.5)
    )
}


# -----------------------------------------------------------------------------
#' Gráfico de linhas — alimentação dentro e fora do domicílio
#'
#' Duas séries em acumulado de 12 meses, com legenda e rótulo direto. A
#' alimentação no domicílio responde a safra, clima e câmbio; a de fora do
#' domicílio é muito mais inercial, puxada por salários e aluguel. Quando as
#' duas divergem, o choque é de oferta; quando convergem, há repasse.
#'
#' @param df \code{tibble} com \code{data}, \code{nome} e \code{acum_12m}.
#' @param anos_recentes Quantos anos exibir. Padrão: 5.
#'
#' @return Objeto \code{ggplot}.
#'
#' @examples
#' \dontrun{
#'   al <- preparar_alimentacao(h) |>
#'     acumular_12m_por_serie(col_grupo = "nome", col_valor = "variacao")
#'   grafico_alimentacao(al)
#' }
# -----------------------------------------------------------------------------
grafico_alimentacao <- function(df, anos_recentes = 5L) {
  stopifnot(
    is.data.frame(df),
    all(c("data", "nome", "acum_12m") %in% names(df))
  )

  data_max   <- max(df$data, na.rm = TRUE)
  data_corte <- data_max %m-% months(12L * as.integer(anos_recentes))

  df_plot <- dplyr::filter(df, !is.na(acum_12m), data >= data_corte)

  gap <- diff(range(df_plot$acum_12m, na.rm = TRUE)) * 0.045

  finais <- df_plot |>
    dplyr::group_by(nome) |>
    dplyr::slice_max(data, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(y_rotulo = .espacar_rotulos(acum_12m, gap))

  ggplot2::ggplot(
    df_plot,
    ggplot2::aes(x = data, y = acum_12m, colour = nome)
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = .COR_CINZA,
                        linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.95) +
    ggplot2::geom_point(data = finais, size = 2) +
    ggplot2::geom_text(
      data = finais,
      ggplot2::aes(y = y_rotulo, label = sprintf("%.1f%%", acum_12m)),
      hjust       = -0.22,
      size        = 2.8,
      fontface    = "bold",
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(
      values = .PALETA_CATEGORICA[1:2],
      name   = NULL
    ) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand      = ggplot2::expansion(mult = c(0.02, 0.12))
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 1, suffix = "%")
    ) +
    ggplot2::labs(x = NULL, y = "Acumulado em 12 meses") +
    .tema_base() +
    ggplot2::theme(legend.position = "bottom")
}
