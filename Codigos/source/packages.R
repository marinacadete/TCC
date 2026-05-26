# ============================================================================ #
#                 Pacotes/Paleta de Cores/Funcoes 
# ============================================================================ #

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  tidyverse, data.table, pROC,
  readxl, readr, ggcorrplot, cowplot,
  RColorBrewer, scales, nortest,
  skimr, xtable, geobr, sf, ggrepel,
  abjutils, grDevices, wordcloud,
  MASS, AER, ragg, gamlss 
)

library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("recode", "dplyr")
conflict_prefer("year", "lubridate")

options(scipen = 999, OutDec = ",")

# ---- Funções -----------------------------------------------------------------

percent <- function(absolute, digits = 2) {
  round(100 * absolute / sum(absolute, na.rm = TRUE), digits)
}

vector_frequencies <- function(vector) {
  vector %>%
    table(useNA = "ifany") %>%
    as_tibble() %>%
    mutate(relative = paste0(percent(n), "%")) %>%
    rename(groups = .data[[1]], absolute = n)
}

no_axis <- theme(
  axis.title = element_blank(),
  axis.text  = element_blank(),
  axis.ticks = element_blank()
)

# ---- Paletas -----------------------------------------------------------------

cinzas_5 <- c("#1a1a1a", "#555555", "#888888", "#bbbbbb", "#e0e0e0")
cinzas_4 <- c("#e0e0e0", "#bbbbbb", "#888888", "#555555", "#1a1a1a")
cinzas_3 <- c("#bbbbbb", "#1a1a1a")
cinzas_2 <- c("#1a1a1a", "#bbbbbb")

ggsave <- function(...) ggplot2::ggsave(..., device = cairo_pdf)