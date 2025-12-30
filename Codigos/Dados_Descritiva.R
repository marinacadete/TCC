# ============================================================================ #
#                       Códigos da Descritiva
# ============================================================================ #
source("Codigos/Trat_Dados.R")
source("rdocs/Taxa.R")

# UNIVARIADA ----
# ---------------------------------------------------------------------------- # 
# Taxa de Homicídios Femininos por Ano
# ---------------------------------------------------------------------------- #
obitos_fem_br <- sim %>%
  mutate(ano = year(DTOBITO)) %>%
  count(ano, name = "obitos_fem")

taxa_fem_br <- obitos_fem_br %>%
  left_join(pop_fem_br, by = "ano") %>%
  mutate(taxa_100k = (obitos_fem / pop_fem) * 1e5) %>%
  arrange(ano); taxa_fem_br

# ---------------------------------------------------------------------------- # 
# Total de Óbitos por Hora/Período
# ---------------------------------------------------------------------------- #
sim_hora <- sim %>%
  filter(!is.na(HORAOBITO)) %>%
  mutate(Hora = lubridate::hour(HORAOBITO))

tab_hora_2023 <- sim_hora %>%
  filter(Ano == 2023) %>%
  count(Hora, name = "obitos")
tab_hora_prev <- sim_hora  %>%
  filter(Ano %in% 2020:2022) %>%
  count(Ano, Hora, name = "obitos")

tab_periodo_hora <- sim %>%
  count(Ano, Periodo_Obito, name = "obitos") %>% 
  dplyr::mutate(Ano = as.character(Ano)) %>% 
  tidyr::pivot_wider(names_from = Ano, values_from = obitos, names_sort = TRUE) %>% 
  dplyr::arrange(Periodo_Obito)

ggplot() +
  geom_col(data = tab_hora_2023, aes(x = Hora, y = obitos)) +
  geom_line(data = tab_hora_prev, aes(x = Hora, y = obitos, 
                                      color = factor(Ano)), 
            linewidth = 1) +
  geom_point(data = tab_hora_prev, aes(x = Hora, 
                                       y = obitos, 
                                       color = factor(Ano)),
             size = 1.8) +
  labs(x = "Hora", y = "Óbitos", color = "Ano") +
  theme_minimal(base_size = 12)

pop_fem_regiao <- dados_projecao %>%
  filter(LOCAL %in% c("Centro-Oeste", "Nordeste", "Norte", 
                    "Sudeste", "Sul"),
         IDADE >= 14, 
         IDADE <= 64) %>%
  mutate(ano = as.integer(ano),
         pop = as.numeric(pop)) %>%
  group_by(Regiao = LOCAL, ano) %>%
  summarise(pop_fem = sum(pop, na.rm = TRUE), .groups = "drop")

obitos_fem_regiao <- sim %>%
  mutate(ano = year(DTOBITO)) %>%
  count(Regiao, ano, name = "obitos_fem")

taxa_fem_regiao <- obitos_fem_regiao %>%
  left_join(pop_fem_regiao, by = c("Regiao", "ano")) %>%
  mutate(taxa_100k = (obitos_fem / pop_fem) * 1e5) %>%
  arrange(Regiao, ano)

taxa_regiao_wide <- taxa_fem_regiao %>%
  dplyr::select(Regiao, ano, taxa_100k) %>%
  pivot_wider(
    names_from = ano,
    values_from = taxa_100k
  )

# ---------------------------------------------------------------------------- # 
# Total de Obito por Região
# ---------------------------------------------------------------------------- #
# TAXA
pop_fem_regiao <- dados_projecao %>%
  filter(LOCAL %in% c("Centro-Oeste", "Nordeste", "Norte", 
                      "Sudeste", "Sul"),
         IDADE >= 14, 
         IDADE <= 64) %>%
  mutate(ano = as.integer(ano),
         pop = as.numeric(pop)) %>%
  group_by(Regiao = LOCAL, ano) %>%
  summarise(pop_fem = sum(pop, na.rm = TRUE), .groups = "drop")

obitos_fem_regiao <- sim %>%
  mutate(ano = year(DTOBITO)) %>%
  count(Regiao, ano, name = "obitos_fem")

taxa_fem_regiao <- obitos_fem_regiao %>%
  left_join(pop_fem_regiao, by = c("Regiao", "ano")) %>%
  mutate(taxa_100k = (obitos_fem / pop_fem) * 1e5) %>%
  arrange(Regiao, ano)

taxa_regiao_ano <- taxa_fem_regiao %>%
  dplyr::select(Regiao, ano, taxa_100k) %>%
  pivot_wider(
    names_from = ano,
    values_from = taxa_100k
  ); taxa_regiao_ano


# OCORRÊNCIA
obitos_fem_regiao_2 <- sim %>%
  mutate(ano = year(DTOBITO)) %>%
  count(Regiao, ano, name = "obitos_fem") %>% 
  tidyr::pivot_wider(names_from = ano, values_from = obitos_fem, names_sort = TRUE);
  

# ---------------------------------------------------------------------------- # 
# Total de Obito por UF
# ---------------------------------------------------------------------------- #

sim_uf <- sim %>%
  filter(!is.na(munResUf))

tab_uf <- sim_uf %>%
  count(Ano, munResUf, name = "obitos")

tabela_uf_2020_2023 <- tab_uf %>%
  dplyr::filter(Ano %in% 2020:2023) %>%
  tidyr::complete(
    Ano = 2020:2023,
    munResUf = sort(unique(tab_uf$munResUf)),
    fill = list(obitos = 0)
  ) %>%
  dplyr::mutate(Ano = as.character(Ano)) %>%
  tidyr::pivot_wider(names_from = Ano, values_from = obitos, names_sort = TRUE) %>%
  dplyr::arrange(munResUf)

g_uf_2023 <- sim_uf %>%
  filter(Ano == 2023) %>%
  count(munResUf, name = "obitos") %>%
  ggplot(aes(x = munResUf, y = obitos)) +
  geom_col() +
  coord_flip() +
  labs(x = "Município", y = "Óbitos") +
  theme_minimal(base_size = 12)

# ---------------------------------------------------------------------------- # 
# Total de Obito por Idade/Faixa Etária 
# ---------------------------------------------------------------------------- #

sim_idade <- sim %>%
  filter(!is.na(IDADEanos))

tab_idade <- sim_idade %>%
  count(Ano, IDADEanos, name = "obitos")

g_idade_2023 <- sim_idade %>%
  filter(Ano == 2023) %>%
  count(IDADEanos, name = "obitos") %>%
  ggplot(aes(x = IDADEanos, y = obitos)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  labs(x = "Idade (anos)", y = "Óbitos") +
  theme_minimal(base_size = 12)

breaks_q5 <- seq(0, 100, by = 5)
labels_q5 <- paste0(seq(0, 95, by = 5), "-", seq(4, 99, by = 5))

tab_idade_q5 <- sim_idade %>%
  mutate(faixa5 = cut(
    IDADEanos,
    breaks = breaks_q5,
    include.lowest = TRUE,
    right = TRUE,
    labels = labels_q5
  )) %>%
  count(Ano, faixa5, name = "obitos")

tab_idade_q5_1 <- sim_idade %>%
  mutate(faixa5 = cut(
    IDADEanos,
    breaks = breaks_q5,
    include.lowest = TRUE,
    right = TRUE,
    labels = labels_q5
  )) %>%
  count(Ano, faixa5, name = "obitos") %>%
  pivot_wider(
    names_from = Ano,
    values_from = obitos,
    values_fill = 0
  ) %>%
  arrange(faixa5)

g_idade_q5_2023 <- tab_idade_q5 %>%
  filter(Ano == 2023) %>%
  ggplot(aes(x = faixa5, y = obitos)) +
  geom_col() +
  labs(x = "Faixa etária (anos)", y = "Óbitos") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---------------------------------------------------------------------------- # 
# Total de Raça/Cor 
# ---------------------------------------------------------------------------- #

sim_raca <- sim %>%
  filter(!is.na(RACACOR))

tab_raca <- sim_raca %>%
  count(Ano, RACACOR, name = "obitos")

tabela_raca_2020_2023 <- tab_raca %>%
  dplyr::filter(Ano %in% 2020:2023) %>%
  tidyr::complete(
    Ano = 2020:2023,
    RACACOR = sort(unique(tab_raca$RACACOR)),
    fill = list(obitos = 0)
  ) %>%
  dplyr::mutate(Ano = as.character(Ano)) %>%
  tidyr::pivot_wider(names_from = Ano, values_from = obitos, names_sort = TRUE) %>%
  dplyr::arrange(RACACOR)

g_raca_2023 <- sim_raca %>%
  filter(Ano == 2023) %>%
  count(RACACOR, name = "obitos") %>%
  ggplot(aes(x = RACACOR, y = obitos)) +
  geom_col() +
  coord_flip() +
  labs(x = "Raça/Cor", y = "Óbitos") +
  theme_minimal(base_size = 12)

# ---------------------------------------------------------------------------- # 
# Total de Obito por Estado Civil
# ---------------------------------------------------------------------------- #

sim_estciv <- sim %>%
  filter(!is.na(ESTCIV))

tab_estciv <- sim_estciv %>%
  count(Ano, ESTCIV, name = "obitos")

tabela_estciv_2020_2023 <- tab_estciv %>%
  dplyr::filter(Ano %in% 2020:2023) %>%
  tidyr::complete(
    Ano = 2020:2023,
    ESTCIV = sort(unique(tab_estciv$ESTCIV)),
    fill = list(obitos = 0)
  ) %>%
  dplyr::mutate(Ano = as.character(Ano)) %>%
  tidyr::pivot_wider(names_from = Ano, values_from = obitos, names_sort = TRUE) %>%
  dplyr::arrange(ESTCIV)

g_estciv_2023 <- sim_estciv %>%
  filter(Ano == 2023) %>%
  count(ESTCIV, name = "obitos") %>%
  ggplot(aes(x = ESTCIV, y = "obitos")) +
  geom_col(aes(y = obitos)) +
  coord_flip() +
  labs(x = "Estado civil", y = "Óbitos") +
  theme_minimal(base_size = 12)

# ---------------------------------------------------------------------------- # 
# Total de Obito por Escolaridade 
# ---------------------------------------------------------------------------- #
sim_esc <- sim %>%
  filter(!is.na(ESC))

tab_esc <- sim_esc %>%
  count(Ano, ESC, name = "obitos")

tabela_esc_2020_2023 <- tab_esc %>%
  dplyr::filter(Ano %in% 2020:2023) %>%
  tidyr::complete(
    Ano = 2020:2023,
    ESC = sort(unique(tab_esc$ESC)),
    fill = list(obitos = 0)
  ) %>%
  dplyr::mutate(Ano = as.character(Ano)) %>%
  tidyr::pivot_wider(names_from = Ano, values_from = obitos, names_sort = TRUE) %>%
  dplyr::arrange(ESC)

g_esc_2023 <- sim_esc %>%
  filter(Ano == 2023) %>%
  count(ESC, name = "obitos") %>%
  ggplot(aes(x = ESC, y = obitos)) +
  geom_col() +
  coord_flip() +
  labs(x = "Escolaridade", y = "Óbitos") +
  theme_minimal(base_size = 12)

# ---------------------------------------------------------------------------- # 
# Total de Obito por Local de Ocorrência 
# ---------------------------------------------------------------------------- #

sim_loco <- sim %>%
  filter(!is.na(LOCOCOR))

tab_loco <- sim_loco %>%
  count(Ano, LOCOCOR, name = "obitos")

tabela_loco_2020_2023 <- tab_loco %>%
  dplyr::filter(Ano %in% 2020:2023) %>%
  tidyr::complete(
    Ano = 2020:2023,
    LOCOCOR = sort(unique(tab_loco$LOCOCOR)),
    fill = list(obitos = 0)
  ) %>%
  dplyr::mutate(Ano = as.character(Ano)) %>%
  tidyr::pivot_wider(names_from = Ano, values_from = obitos, names_sort = TRUE) %>%
  dplyr::arrange(LOCOCOR)

g_loco_2023 <- sim_loco %>%
  filter(Ano == 2023) %>%
  count(LOCOCOR, name = "obitos") %>%
  ggplot(aes(x = LOCOCOR, y = obitos)) +
  geom_col() +
  coord_flip() +
  labs(x = "Local", y = "Óbitos") +
  theme_minimal(base_size = 12)

# ---------------------------------------------------------------------------- # 
# Total de Obito por Causa Base (CID-10)
# ---------------------------------------------------------------------------- #

sim_causa <- sim %>%
  filter(!is.na(causa_categoria))

tab_causa <- sim_causa %>%
  count(Ano, causa_categoria, name = "obitos")

g_causa_2020_2023 <- ggplot(tab_causa, aes(x = causa_categoria, 
                                           y = obitos, fill = factor(Ano))) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(x = "CID-10", y = "Óbitos") +
  theme_minimal(base_size = 12)

g_causa_2023 <- sim_causa %>%
  filter(Ano == 2023) %>%
  count(causa_categoria, name = "obitos") %>%
  ggplot(aes(x = causa_categoria, y = obitos)) +
  geom_col() +
  coord_flip() +
  labs(x = "CID-10", y = "Óbitos") +
  theme_minimal(base_size = 12)


# ---------------------------------------------------------------------------- # 
# Total de Obito por Cid-10 Agrupado
# ---------------------------------------------------------------------------- #

sim_cid10 <- sim %>%
  filter(!is.na(causa_categoria))

tab_cid10 <- sim_cid10 %>%
  count(Ano, causa_categoria, name = "obitos")


g_cid10_2023 <- sim_cid10 %>%
  filter(Ano == 2023) %>%
  count(causa_categoria, name = "obitos") %>%
  ggplot(aes(x = causa_categoria, y = obitos)) +
  geom_col() +
  coord_flip() +
  labs(x = "CID-10 (agrupada)", y = "Óbitos") +
  theme_minimal(base_size = 12)

g_cid10_2023

tab_cid10_larga <- tab_cid10 %>%
  pivot_wider(names_from = Ano, values_from = obitos, values_fill = 0) %>%
  arrange(causa_categoria)

tab_cid10_larga


tab_cid10_ano <- sim_cid10 %>%
  count(Ano, causa_categoria, name = "obitos")
tab_cid10_ano1 <- sim_cid10 %>% 
  count(Ano, causa_categoria, name = "obitos") %>% 
  pivot_wider(
    names_from = Ano,
    values_from = obitos,
    values_fill = 0
  )

g_cid10_ano <- ggplot(tab_cid10_ano, aes(x = Ano, 
                                         y = obitos, 
                                         color = causa_categoria, 
                                         group = causa_categoria)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(x = "Ano", y = "Número de óbitos", color = "CID-10") +
  scale_x_continuous(breaks = sort(unique(tab_cid10_ano$Ano))) +
  theme_minimal(base_size = 12)

# BIVARIADO ----

sim2023 <- sim %>% filter(Ano == 2023)

# ---------------------------------------------------------------------------- #
# Hora Óbito 2023 por CID-10
# ---------------------------------------------------------------------------- #
hora_CID_2023 <- sim2023 %>%
  mutate(Hora = lubridate::hour(HORAOBITO)) %>%
  count(Hora, causa_categoria, name = "obitos")

ggplot(hora_CID_2023, aes(x = Hora, y = obitos, shape = causa_categoria)) +
  geom_line(linewidth = 0.1) +
  geom_point(size = 2) +
  labs(
    x = "Hora do óbito",
    y = "Óbitos",
    shape = "CID-10"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.05, 0.95),
    legend.justification = c("left", "top"), 
    legend.background = element_rect(fill = alpha("white", 0.8), color = "gray80"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold")
  ) +
  guides(shape = guide_legend(ncol = 1, byrow = TRUE))

ggsave("Analise_files/hora_cid.pdf", width = 158, height = 93, units = "mm")

hora_agrup_CID_2023 <- sim2023 %>%
  mutate(Hora = lubridate::hour(HORAOBITO)) %>%
  count(Periodo_Obito, causa_categoria, name = "obitos") %>% 
  pivot_wider(
    names_from = Periodo_Obito,
    values_from = obitos,
    values_fill = 0
    ) %>%
  arrange(causa_categoria)

xtable::xtable(hora_agrup_CID_2023)

# ---------------------------------------------------------------------------- #
# Região 2023 por CID-10
# ---------------------------------------------------------------------------- #
reg_CID_2023 <- sim2023 %>%
  count(Regiao, causa_categoria, name = "obitos") %>%
  group_by(Regiao) %>%
  mutate(prop = obitos / sum(obitos)) %>%
  ungroup()

ggplot(reg_CID_2023, aes(x = Regiao, y = prop, fill = causa_categoria)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_grey(
    start = 0.2,  # mais escuro
    end = 0.9,    # mais claro
    name = "CID-10"
  ) +
  labs(
    x = "Região",
    y = "Proporção de óbitos (%)",
    fill = "CID-10"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.box = "vertical",
    legend.box.just = "center"
  ) +
  guides(fill = guide_legend(ncol = 1, byrow = TRUE))

ggsave("Analise_files/reg_cid.pdf", width = 158, height = 105, units = "mm")

reg_agrup_CID_2023 <- sim2023 %>%
  count(Regiao, causa_categoria, name = "obitos") %>%
  group_by(Regiao) %>%
  mutate(prop = obitos / sum(obitos)) %>%
  ungroup() %>% 
  mutate(
    prop = round(prop * 100, 1)  # transformar proporção em %
  ) %>%
  pivot_wider(
    names_from = Regiao,
    values_from = c(obitos, prop),
    values_fill = 0
  ) %>%
  arrange(causa_categoria)

xtable::xtable(reg_agrup_CID_2023)
# ---------------------------------------------------------------------------- #
# UF 2023 por CID-10
# ---------------------------------------------------------------------------- #
uf_CID_2023 <- sim2023 %>%
  count(munResUf, causa_categoria, name = "obitos") %>%
  group_by(munResUf) %>%
  mutate(prop = obitos / sum(obitos)) %>%
  ungroup()

ggplot(uf_CID_2023, aes(x = munResUf, y = prop, fill = causa_categoria)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  scale_fill_grey(
    start = 0.2,  # mais escuro
    end = 0.9,    # mais claro
    name = "CID-10"
  ) +
  labs(
    x = "Unidade da Federação",
    y = "Proporção de óbitos (%)",
    fill = "CID-10"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.box = "vertical",
    legend.box.just = "center"
  ) +
  guides(fill = guide_legend(ncol = 1, byrow = TRUE))

ggsave("Analise_files/uf_cid.pdf", width = 158, height = 93, units = "mm")
# ---------------------------------------------------------------------------- #
# Faixa Etária 2023 e CID-10
# ---------------------------------------------------------------------------- #
idade_CID_2023 <- sim2023 %>%
  count(fe_quinque, causa_categoria, name = "obitos") %>%
  group_by(fe_quinque) %>%
  mutate(prop = obitos / sum(obitos)) %>%
  ungroup()

ggplot(idade_CID_2023, aes(x = fe_quinque, y = prop, fill = causa_categoria)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_grey(
    start = 0.2,  # mais escuro
    end = 0.9,    # mais claro
    name = "CID-10"
  ) +
  labs(
    x = "Faixa Etária",
    y = "Proporção de óbitos (%)",
    fill = "CID-10"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.box = "vertical",
    legend.box.just = "center"
  ) +
  guides(fill = guide_legend(ncol = 1, byrow = TRUE))

ggsave("Analise_files/idade_cid.pdf", width = 158, height = 93, units = "mm")

ggplot(idade_CID_2023, aes(x = fe_quinque, y = obitos, fill = causa_categoria)) +
  geom_col(position = "dodge") +
  labs(x = "Raça/Cor", y = "Óbitos", fill = "causa_categoria") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("Analise_files/idade2_cid.pdf", width = 158, height = 93, units = "mm")
# ---------------------------------------------------------------------------- #
# Raça/Cor 2023 e CID-10
# ---------------------------------------------------------------------------- #
raca_CID_2023 <- sim2023 %>%
  count(RACACOR, causa_categoria, name = "obitos") %>%
  group_by(RACACOR) %>%
  mutate(prop = obitos / sum(obitos)) %>%
  ungroup()

ggplot(raca_CID_2023, aes(x = RACACOR, y = prop, fill = causa_categoria)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_grey(
    start = 0.2,  # mais escuro
    end = 0.9,    # mais claro
    name = "CID-10"
  ) +
  labs(
    x = "Raça/Cor",
    y = "Proporção de óbitos (%)",
    fill = "CID-10"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.box = "vertical",
    legend.box.just = "center"
  ) +
  guides(fill = guide_legend(ncol = 1, byrow = TRUE))

ggsave("Analise_files/raca_cid.pdf", width = 158, height = 93, units = "mm")

ggplot(raca_CID_2023, aes(x = RACACOR, y = obitos, fill = causa_categoria)) +
  geom_col(position = "dodge") +
  labs(x = "Raça/Cor", y = "Óbitos", fill = "causa_categoria") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("Analise_files/raca2_cid.pdf", width = 158, height = 93, units = "mm")
# ---------------------------------------------------------------------------- #
# Estado Civil 2023 e CID-10
# ---------------------------------------------------------------------------- #
estciv_CID_2023 <- sim2023 %>%
  count(ESTCIV, causa_categoria, name = "obitos") %>%
  group_by(ESTCIV) %>%
  mutate(prop = obitos / sum(obitos)) %>%
  ungroup()

ggplot(estciv_CID_2023, aes(x = ESTCIV, y = prop, fill = causa_categoria)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_grey(
    start = 0.2,  # mais escuro
    end = 0.9,    # mais claro
    name = "CID-10"
  ) +
  labs(
    x = "Estado Civil",
    y = "Proporção de óbitos (%)",
    fill = "CID-10"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.box = "vertical",
    legend.box.just = "center"
  ) +
  guides(fill = guide_legend(ncol = 1, byrow = TRUE))

ggsave("Analise_files/estciv_cid.pdf", width = 158, height = 93, units = "mm")

ggplot(estciv_CID_2023, aes(x = ESTCIV, y = obitos, fill = causa_categoria)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(x = "Estado civil", y = "Óbitos", fill = "causa_categoria") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("Analise_files/estciv2_cid.pdf", width = 158, height = 93, units = "mm")
# ---------------------------------------------------------------------------- #
# Escolaridade 2023 e CID-10
# ---------------------------------------------------------------------------- #
esc_CID_2023 <- sim2023 %>%
  count(ESC, causa_categoria, name = "obitos") %>%
  group_by(ESC) %>%
  mutate(prop = obitos / sum(obitos)) %>%
  ungroup()

ggplot(esc_CID_2023, aes(x = ESC, y = prop, fill = causa_categoria)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_grey(
    start = 0.2,  # mais escuro
    end = 0.9,    # mais claro
    name = "CID-10"
  ) +
  labs(
    x = "Escolaridade",
    y = "Proporção de óbitos (%)",
    fill = "CID-10"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.box = "vertical",
    legend.box.just = "center"
  ) +
  guides(fill = guide_legend(ncol = 1, byrow = TRUE))

ggsave("Analise_files/esc_cid.pdf", width = 158, height = 93, units = "mm")

ggplot(esc_CID_2023, aes(x = ESC, y = obitos, fill = causa_categoria)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(x = "Escolaridade", y = "Óbitos", fill = "causa_categoria") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("Analise_files/esc2_cid.pdf", width = 158, height = 93, units = "mm")
# ---------------------------------------------------------------------------- #
# Local de Ocorrencia 2023 e CID-10
# ---------------------------------------------------------------------------- #
loco_CID_2023 <- sim2023 %>%
  count(LOCOCOR, causa_categoria, name = "obitos") %>%
  group_by(LOCOCOR) %>%
  mutate(prop = obitos / sum(obitos)) %>%
  ungroup()

ggplot(loco_CID_2023, aes(x = LOCOCOR, y = prop, fill = causa_categoria)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_grey(
    start = 0.2,  # mais escuro
    end = 0.9,    # mais claro
    name = "CID-10"
  ) +
  labs(
    x = "Local de Ocorrência",
    y = "Proporção de óbitos (%)",
    fill = "CID-10"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.box = "vertical",
    legend.box.just = "center"
  ) +
  guides(fill = guide_legend(ncol = 1, byrow = TRUE))

ggsave("Analise_files/loc_cid.pdf", width = 158, height = 93, units = "mm")

ggplot(loco_CID_2023, aes(x = LOCOCOR, y = obitos, fill = causa_categoria)) +
  geom_col(position = "dodge") +
  labs(x = "Local", y = "Óbitos", fill = "causa_categoria") +
  theme_minimal(base_size = 12)

ggsave("Analise_files/loc2_cid.pdf", width = 158, height = 93, units = "mm")

# GRÁFICOS ----

regioes_sf <- geobr::read_region(year = 2020) %>%
  dplyr::select(code_region, name_region, geom) %>%
  dplyr::mutate(
    Regiao_geobr = dplyr::recode(
      name_region,
      "Centro Oeste" = "Centro-Oeste",
      .default = name_region
    )
  )

dados_mapa <- regioes_sf %>%
  dplyr::left_join(taxa_fem_regiao, by = c("Regiao_geobr" = "Regiao"))

## REGIAO ----
ggplot(dados_mapa) +
  geom_sf(aes(fill = taxa_100k), color = "white", linewidth = 0.3) +
  geom_sf_text(aes(label = round(taxa_100k, 2)), size = 2.8, color = "white") +
  scale_fill_gradient(
    name = "Taxa por 100 mil",
    low = "white", high = "black"
  ) +
  facet_wrap(~ ano, nrow = 2) +
  # labs(title = "Taxa de homicídios femininos por Região - 2020 a 2023",
  #      subtitle = "Taxa por 100 mil habitantes (pop. feminina)",
  #      caption = "Fonte: SIM/MS e projeções IBGE (14-64 anos)") +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    strip.text = element_text(face = "bold"))


## UF ----
# --------------------------- #
# Dados
# --------------------------- #
ufs_sf <- geobr::read_state(year = 2020) %>%
  dplyr::select(code_state, abbrev_state, name_state, geom) %>% 
  mutate(
    name_state = dplyr::recode(
      name_state,
      "Amazônas"            = "Amazonas",
      "Mato Grosso Do Sul"  = "Mato Grosso do Sul",
      "Rio De Janeiro"      = "Rio de Janeiro",
      "Rio Grande Do Norte" = "Rio Grande do Norte",
      "Rio Grande Do Sul"   = "Rio Grande do Sul"
    )
  )

# --------------------------- #
# Unindo os dados
# --------------------------- #
dados_mapa_uf <- ufs_sf %>%
  left_join(taxa_fem_uf, by = c("name_state" = "munResUf"))

# --------------------------- #
# Mapa 2020 a 2023
# --------------------------- #
ggplot(dados_mapa_uf) +
  geom_sf(aes(fill = taxa_100k), color = "white", linewidth = 0.2) +
  scale_fill_gradient(
    name = "Taxa por 100 mil",
    low = "white", high = "black"
  ) +
  facet_wrap(~ ano, nrow = 2) +
  # labs(
  #   title = "Taxa de homicídios femininos por UF - 2020 a 2023",
  #   subtitle = "Taxa por 100 mil habitantes (pop. feminina, 14-64 anos)",
  #   caption = "Fonte: SIM/MS e projeções IBGE"
  # ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    strip.text = element_text(face = "bold")
  )

