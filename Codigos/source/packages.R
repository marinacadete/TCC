# ============================================================
# Setup
# ============================================================

# Instala/Carrega pacman
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  tidyverse, data.table, pROC,
  readxl, readr, ggcorrplot, cowplot,
  RColorBrewer, scales, nortest,
  skimr, xtable, geobr, sf, ggrepel,
  abjutils, grDevices, wordcloud,
  MASS
)

# Evitar que MASS mascare dplyr::select
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("year", "lubridate")

# ============================================================
# Opções globais
# ============================================================

options(
  scipen = 999,
  OutDec = ","
)

# ============================================================
# Funções utilitárias
# ============================================================

# Percentual (frequência relativa) a partir de frequências absolutas
percent <- function(absolute, digits = 2) {
  round(100 * absolute / sum(absolute, na.rm = TRUE), digits)
}

# Frequências absoluta e relativa de um vetor categórico
vector_frequencies <- function(vector) {
  vector %>%
    table(useNA = "ifany") %>%
    as_tibble() %>%
    dplyr::mutate(
      relative = paste0(percent(n), "%")
    ) %>%
    dplyr::rename(
      groups   = .data[[1]],
      absolute = n
    )
}

# Tema para remover eixos (útil em mapas)
no_axis <- theme(
  axis.title = element_blank(),
  axis.text  = element_blank(),
  axis.ticks = element_blank()
)

# ============================================================
# Quadro resumo (LaTeX)
# ============================================================

print_quadro_resumo <- function(
    data,
    var_name,
    title = "Medidas resumo da(o) [nome da variável]",
    label = "quad:quadro_resumo1"
) {
  var_name <- rlang::ensym(var_name)
  
  resumo <- data %>%
    dplyr::summarise(
      `Média`         = round(mean(!!var_name, na.rm = TRUE), 2),
      `Desvio Padrão` = round(sd(!!var_name, na.rm = TRUE), 2),
      `Variância`     = round(var(!!var_name, na.rm = TRUE), 2),
      `Mínimo`        = round(min(!!var_name, na.rm = TRUE), 2),
      `1º Quartil`    = round(quantile(!!var_name, probs = .25, na.rm = TRUE), 2),
      `Mediana`       = round(quantile(!!var_name, probs = .50, na.rm = TRUE), 2),
      `3º Quartil`    = round(quantile(!!var_name, probs = .75, na.rm = TRUE), 2),
      `Máximo`        = round(max(!!var_name, na.rm = TRUE), 2)
    ) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Estatística") %>%
    dplyr::rename(Valor = V1)
  
  # Define table-format dinamicamente para o siunitx (S[])
  # (Aqui só há uma coluna numérica: Valor)
  numCount <- resumo$Valor %>%
    as.numeric() %>%
    abs() %>%
    suppressWarnings() %>%
    max(na.rm = TRUE) %>%
    { if (is.finite(.)) floor(log10(.) + 1) else 1 }
  
  latex <- stringr::str_c(
    "\\begin{quadro}[H]
\t\\setlength{\\tabcolsep}{9pt}
\t\\renewcommand{\\arraystretch}{1.20}
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{| l | S[table-format = ", numCount, ".2] |}
\t\\hline
\t\t\\textbf{Estatística} & \\textbf{Valor} \\\\
\t\t\\hline
", sep = ""
  )
  
  corpo <- resumo %>%
    dplyr::mutate(
      linha = paste0("\t\t", Estatística, " & ", Valor, " \\\\")
    ) %>%
    dplyr::pull(linha) %>%
    paste(collapse = "\n")
  
  fim <- stringr::str_c(
    "
\t\t\\hline
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep = ""
  )
  
  writeLines(paste0(latex, corpo, fim))
}
