# ============================================================================ #
#                     Descritiva
# ============================================================================ #

source("Codigos/1-Tratamento.R")
source("Codigos/2-CalculoTaxas.R")

# ---- Total de obitos por ano e Taxas -----------------------------------------
tab_geral <- taxa_fem_br %>%
  rename(Ano             = Ano,
         `Óbitos`        = obitos_fem,
         `Pop. feminina` = pop_fem,
         `Taxa (100 mil)`= taxa_100k
  ) %>%
  mutate(`Taxa (100 mil)` = round(`Taxa (100 mil)`, 2))

xtable(tab_geral)

# ---- Distribuição Geografica -------------------------------------------------
# Região
tab_regiao <- sim %>%
  count(Regiao, Ano, name = "obitos") %>%
  mutate(Ano = as.character(Ano)) %>%
  pivot_wider(names_from = Ano, values_from = obitos,
              names_sort = TRUE, names_prefix = "ob_") %>%
  left_join(
    taxa_regiao %>%
      pivot_longer(-Regiao, names_to = "Ano", values_to = "taxa") %>%
      mutate(taxa = round(taxa, 2)) %>%
      pivot_wider(names_from = Ano, values_from = taxa,
                  names_sort = TRUE, names_prefix = "tx_"),
    by = "Regiao"
  ) %>%
  arrange(Regiao) %>%
  select(Regiao,
         ob_2020, tx_2020,
         ob_2021, tx_2021,
         ob_2022, tx_2022,
         ob_2023, tx_2023)

xtable(tab_regiao)

regiao_sf <- geobr::read_region(year = 2020) %>%
  dplyr::select(code_region, name_region, geom) %>%
  mutate(name_region = recode(name_region,
                              "Norte"         = "Norte",
                              "Nordeste"      = "Nordeste",
                              "Sudeste"       = "Sudeste",
                              "Sul"           = "Sul",
                              "Centro Oeste"  = "Centro-Oeste"
  ))

taxa_regiao_long <- taxa_regiao %>%
  pivot_longer(-Regiao, names_to = "ano", values_to = "taxa_100k") %>%
  mutate(ano = as.integer(ano))

dados_mapa_regiao <- regiao_sf %>%
  left_join(taxa_regiao_long, by = c("name_region" = "Regiao"))

ggplot(dados_mapa_regiao) +
  geom_sf(aes(fill = taxa_100k), color = "white", linewidth = 0.15) +
  scale_fill_gradient(
    name     = "Taxa por 100 mil",
    low      = "#f0f0f0",
    high     = "#1a1a1a",
    na.value = "grey80",
    guide    = guide_colorbar(
      title.position = "top",
      barwidth       = unit(8, "cm"),
      barheight      = unit(0.3, "cm")
    )
  ) +
  facet_wrap(~ano, nrow = 2) +
  theme_void(base_family = "Times New Roman") +
  theme(
    legend.position   = "bottom",
    legend.title      = element_text(size = 7, hjust = 0.5),
    legend.text       = element_text(size = 7),
    strip.text        = element_text(size = 8, margin = margin(b = 3)),
    plot.caption      = element_text(size = 6, hjust = 0, margin = margin(t = 6)),
    panel.spacing     = unit(0.5, "lines"))

ggsave("Imagens/1a_mapa_regiao.pdf", width = 190, height = 150, units = "mm")


# UF
tab_uf <- sim %>%
  count(munResUf, Ano, name = "obitos") %>%
  mutate(Ano = as.character(Ano)) %>%
  pivot_wider(names_from = Ano, values_from = obitos,
              names_sort = TRUE, names_prefix = "ob_") %>%
  left_join(
    taxa_uf %>%
      select(-codUF) %>%
      pivot_longer(-munResUf, names_to = "Ano", values_to = "taxa") %>%
      mutate(taxa = round(taxa, 2)) %>%
      pivot_wider(names_from = Ano, values_from = taxa,
                  names_sort = TRUE, names_prefix = "tx_"),
    by = "munResUf"
  ) %>%
  left_join(
    taxa_uf %>%
      distinct(munResUf, codUF),
    by = "munResUf"
  ) %>%
  arrange(codUF) %>%
  select(munResUf,
         ob_2020, tx_2020,
         ob_2021, tx_2021,
         ob_2022, tx_2022,
         ob_2023, tx_2023)

xtable(tab_uf)

taxa_uf_long <- taxa_uf %>%
  pivot_longer(-c(codUF, munResUf), names_to = "ano", values_to = "taxa_100k") %>%
  mutate(ano = as.integer(ano))

ufs_sf <- geobr::read_state(year = 2020) %>%
  dplyr::select(code_state, name_state, geom) %>%
  mutate(name_state = recode(name_state,
                             "Amazônas"            = "Amazonas",
                             "Mato Grosso Do Sul"  = "Mato Grosso do Sul",
                             "Rio De Janeiro"      = "Rio de Janeiro",
                             "Rio Grande Do Norte" = "Rio Grande do Norte",
                             "Rio Grande Do Sul"   = "Rio Grande do Sul"
  ))

dados_mapa_painel <- ufs_sf %>%
  left_join(taxa_uf_long, by = c("name_state" = "munResUf"))

ggplot(dados_mapa_painel) +
  geom_sf(aes(fill = taxa_100k), color = "white", linewidth = 0.15) +
  scale_fill_gradient(
    name     = "Taxa por 100 mil",
    low      = "#f0f0f0",
    high     = "#1a1a1a",
    na.value = "grey80",
    guide    = guide_colorbar(
      title.position = "top",
      barwidth       = unit(8, "cm"),
      barheight      = unit(0.3, "cm")
    )
  ) +
  facet_wrap(~ano, nrow = 2) +
  theme_void(base_family = "Times New Roman") +
  theme(
    legend.position   = "bottom",
    legend.title      = element_text(size = 7, hjust = 0.5),
    legend.text       = element_text(size = 7),
    strip.text        = element_text(size = 8, margin = margin(b = 3)),
    plot.caption      = element_text(size = 6, hjust = 0, margin = margin(t = 6)),
    panel.spacing     = unit(0.5, "lines")
  )

ggsave("Imagens/1b_mapa_uf.pdf", width = 190, height = 150, units = "mm")

# ---- Perfil da Vítima --------------------------------------------------------
# ---- Raça/Cor ----------------------------------------------------------------
tab_raca <- sim %>%
  filter(!is.na(raca_cor)) %>%
  count(Ano, raca_cor, name = "obitos") %>%
  group_by(Ano) %>%
  mutate(prop = round(obitos / sum(obitos) * 100, 1)) %>%
  ungroup() %>%
  mutate(Ano = as.character(Ano)) %>%
  pivot_wider(
    names_from  = Ano,
    values_from = c(obitos, prop),
    names_glue  = "{Ano}_{.value}"
  ) %>%
  arrange(raca_cor)

xtable(tab_raca)

sim %>%
  filter(!is.na(raca_cor)) %>%
  count(Ano, raca_cor, name = "obitos") %>%
  group_by(Ano) %>%
  mutate(prop = obitos / sum(obitos)) %>%
  ungroup() %>%
  ggplot(aes(x = factor(Ano), y = prop, fill = raca_cor)) +
  geom_col(color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = cinzas_3, name = "Raça/cor") +
  labs(x = "Ano", y = "Proporção de óbitos (%)") +
  theme_light() +
  theme(axis.text.x     = element_text(angle = 0, hjust = 0.5),
        legend.position = "bottom",
        legend.text     = element_text(size = 8),
        legend.key.size = unit(0.4, "cm"))

ggsave("Imagens/2a_raca_ano.pdf", width = 158, height = 93, units = "mm")

# ---- Faixa etaria ------------------------------------------------------------
# Quinquenal
tab_fe <- sim %>%
  filter(!is.na(fe_quinque)) %>%
  count(Ano, fe_quinque, name = "obitos") %>%
  left_join(
    taxa_idade_quinque %>%
      pivot_longer(-fe_quinque, names_to = "Ano", values_to = "taxa") %>%
      mutate(Ano = as.integer(Ano)),
    by = c("Ano", "fe_quinque")
  ) %>%
  mutate(
    taxa = round(taxa, 2),
    Ano  = as.character(Ano)
  ) %>%
  pivot_wider(
    names_from  = Ano,
    values_from = c(obitos, taxa),
    names_glue  = "{Ano}_{.value}"
  ) %>%
  arrange(fe_quinque)

xtable(tab_fe)

taxa_idade_quinque %>%
  pivot_longer(-fe_quinque, names_to = "ano", values_to = "taxa_100k") %>%
  mutate(
    ano = as.integer(ano),
    fe_quinque = str_replace(fe_quinque, " anos", "\nanos")
  ) %>%
  ggplot(aes(x = fe_quinque, y = taxa_100k, group = factor(ano))) +
  geom_line(aes(linetype = factor(ano)), linewidth = 0.7) +
  geom_point(aes(shape = factor(ano)), size = 2) +
  labs(x = "Faixa etária", y = "Taxa por 100 mil",
       linetype = "Ano", shape = "Ano") +
  theme_light() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

ggsave("Imagens/2c_taxa_faixa_etaria.pdf", width = 190, height = 100, units = "mm")


# Resumida
tab_fe <- sim %>%
  filter(!is.na(fe_resumida)) %>%
  count(Ano, fe_resumida, name = "obitos") %>%
  group_by(Ano) %>%
  mutate(prop = round(obitos / sum(obitos) * 100, 1)) %>%
  ungroup() %>%
  mutate(Ano = as.character(Ano)) %>%
  pivot_wider(
    names_from  = Ano,
    values_from = c(obitos, prop),
    names_glue  = "{Ano}_{.value}"
  ) %>%
  arrange(fe_resumida)

xtable(tab_fe)

taxa_idade_resumida %>%
  pivot_longer(-fe_resumida, names_to = "ano", values_to = "taxa_100k") %>%
  mutate(
    ano = as.integer(ano),
    fe_resumida = str_replace(fe_resumida, " anos", "\nanos")
  ) %>%
  ggplot(aes(x = fe_resumida, y = taxa_100k, group = factor(ano))) +
  geom_line(aes(linetype = factor(ano)), linewidth = 0.7) +
  geom_point(aes(shape = factor(ano)), size = 2) +
  labs(x = "Faixa etária", y = "Taxa por 100 mil",
       linetype = "Ano", shape = "Ano") +
  theme_light() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

ggsave("Imagens/2c2_taxa_faixa_etaria.pdf", width = 190, height = 100, units = "mm")

# ---- estado civil e escolaridade ---------------------------------------------
# Estado civil
tab_estciv <- sim %>%
  filter(!is.na(estciv_grupo)) %>%
  count(Ano, estciv_grupo, name = "obitos") %>%
  group_by(Ano) %>%
  mutate(prop = round(obitos / sum(obitos) * 100, 1)) %>%
  ungroup() %>%
  mutate(Ano = as.character(Ano)) %>%
  pivot_wider(
    names_from  = Ano,
    values_from = c(obitos, prop),
    names_glue  = "{Ano}_{.value}"
  ) %>%
  arrange(estciv_grupo)

xtable(tab_estciv)

sim %>%
  filter(!is.na(estciv_grupo)) %>%
  count(Ano, estciv_grupo, name = "obitos") %>%
  group_by(Ano) %>%
  mutate(prop = obitos / sum(obitos)) %>%
  ungroup() %>%
  ggplot(aes(x = factor(Ano), y = prop, fill = estciv_grupo)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = cinzas_5, name = "Estado civil") +
  labs(x = "Ano", y = "Proporção de óbitos (%)") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "plain")
  ) +
  guides(fill = guide_legend(nrow = 1))

ggsave("Imagens/2d_estciv_ano.pdf", width = 158, height = 100, units = "mm")

# Escolaridade
tab_esc <- sim %>%
  filter(!is.na(ESC_GRUPO)) %>%
  count(Ano, ESC_GRUPO, name = "obitos") %>%
  group_by(Ano) %>%
  mutate(prop = round(obitos / sum(obitos) * 100, 1)) %>%
  ungroup() %>%
  mutate(Ano = as.character(Ano)) %>%
  pivot_wider(
    names_from  = Ano,
    values_from = c(obitos, prop),
    names_glue  = "{Ano}_{.value}"
  ) %>%
  arrange(ESC_GRUPO)

xtable(tab_esc)

sim %>%
  filter(!is.na(ESC_GRUPO)) %>%
  count(Ano, ESC_GRUPO, name = "obitos") %>%
  group_by(Ano) %>%
  mutate(prop = obitos / sum(obitos)) %>%
  ungroup() %>%
  ggplot(aes(x = factor(Ano), y = prop, fill = ESC_GRUPO)) +
  geom_col(position = "dodge", color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = cinzas_5, name = "Escolaridade") +
  labs(x = "Ano", y = "Proporção de óbitos (%)") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "plain")
  ) +
    guides(fill = guide_legend(nrow = 1))

ggsave("Imagens/2e_escolaridade_ano.pdf", width = 158, height = 93, units = "mm")

# ---- Causa da morte ----------------------------------------------------------
tab_causa <- sim %>%
  count(Ano, causa_categoria, name = "obitos") %>%
  group_by(Ano) %>%
  mutate(prop = round(obitos / sum(obitos) * 100, 1)) %>%
  ungroup() %>%
  mutate(Ano = as.character(Ano)) %>%
  pivot_wider(
    names_from  = Ano,
    values_from = c(obitos, prop),
    names_glue  = "{Ano}_{.value}"
  ) %>%
  arrange(causa_categoria)

sim %>%
  count(Ano, causa_categoria, name = "obitos") %>%
  group_by(Ano) %>%
  mutate(prop = obitos / sum(obitos)) %>%
  ungroup() %>%
  ggplot(aes(x = factor(Ano), y = prop, fill = causa_categoria)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(
    values = cinzas_5,
    name   = "Causa Base",
    labels = c(
      "Lesão por arma de fogo",
      "Lesão por enforcamento",
      "Lesão por instrumento perfurante,\ncortante ou contundente",
      "Lesão por maus tratos",
      "Outros"
    )
  ) +
  labs(x = "Ano", y = "Proporção de óbitos (%)") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "plain")
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave("Imagens/3a_causa_proporcao_ano.pdf", width = 200, height = 93, units = "mm")

# ---- Local de ocorrência -----------------------------------------------------
tab_loco <- sim %>%
  filter(!is.na(lococor_grupo)) %>%
  count(Ano, lococor_grupo, name = "obitos") %>%
  group_by(Ano) %>%
  mutate(prop = round(obitos / sum(obitos) * 100, 1)) %>%
  ungroup() %>%
  mutate(Ano = as.character(Ano)) %>%
  pivot_wider(
    names_from  = Ano,
    values_from = c(obitos, prop),
    names_glue  = "{Ano}_{.value}"
  ) %>%
  arrange(lococor_grupo)

xtable(tab_loco)

sim %>%
  filter(!is.na(lococor_grupo)) %>%
  count(Ano, lococor_grupo, name = "obitos") %>%
  group_by(Ano) %>%
  mutate(prop = obitos / sum(obitos)) %>%
  ungroup() %>%
  ggplot(aes(x = factor(Ano), y = prop, fill = lococor_grupo)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = cinzas_5, name = "Local de Ocorrência") +
  labs(x = "Ano", y = "Proporção de óbitos (%)") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  guides(fill = guide_legend(ncol = 1)) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "plain")
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave("Imagens/3b_local_ocorrencia_ano.pdf", width = 158, height = 100, units = "mm")

# ---- Cruzamentos -------------------------------------------------------------
# Todos os cruzamentos são referentes a 2023 para facilitar a interpretação

sim2023 <- sim %>% filter(Ano == 2023)
 
# Causa × Raça/cor
cruz_causa_raca <- sim2023 %>%
  count(raca_cor, causa_categoria, name = "obitos") %>%
  group_by(causa_categoria) %>%
  mutate(prop = round(obitos / sum(obitos) * 100, 1)) %>%
  ungroup()

xtable(cruz_causa_raca)

ggplot(cruz_causa_raca, aes(x = raca_cor, y = prop, fill = causa_categoria)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = cinzas_5, name = "Causa") +
  labs(x = "Raça/cor", y = "Proporção de óbitos (%)") +
  scale_fill_manual(
    values = cinzas_5,
    name   = "Causa Base",
    labels = c(
      "Lesão por arma de fogo",
      "Lesão por enforcamento",
      "Lesão por instrumento perfurante,\ncortante ou contundente",
      "Lesão por maus tratos",
      "Outros"
    )
  ) +
  labs(x = "Raça/Cor", y = "Proporção de óbitos (%)") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "plain")
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave("Imagens/4a_causa_raca_2023.pdf", width = 158, height = 100, units = "mm")

# Causa × Região
cruz_causa_regiao <- sim2023 %>%
  count(Regiao, causa_categoria, name = "obitos") %>%
  group_by(Regiao) %>%
  mutate(prop = round(obitos / sum(obitos) * 100, 1)) %>%
  ungroup()

xtable(cruz_causa_regiao)

ggplot(cruz_causa_regiao, aes(x = Regiao, y = prop, fill = causa_categoria)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = cinzas_5, name = "Causa") +
  labs(x = "Região", y = "Proporção de óbitos (%)") +
  scale_fill_manual(
    values = cinzas_5,
    name   = "Causa Base",
    labels = c(
      "Lesão por arma de fogo",
      "Lesão por enforcamento",
      "Lesão por instrumento perfurante,\ncortante ou contundente",
      "Lesão por maus tratos",
      "Outros"
    )
  ) +
  labs(x = "Região", y = "Proporção de óbitos (%)") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "plain")
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave("Imagens/4b_causa_regiao_2023.pdf", width = 158, height = 100, units = "mm")

# Causa × Faixa etária
cruz_causa_fe <- sim2023 %>%
  filter(!is.na(fe_resumida)) %>%
  count(fe_resumida, causa_categoria, name = "obitos") %>%
  group_by(fe_resumida) %>%
  mutate(prop = round(obitos / sum(obitos) * 100, 1)) %>%
  ungroup() %>% 
  mutate(
    fe_resumida = str_replace(fe_resumida, " anos", "\nanos")
  )

ggplot(cruz_causa_fe, aes(x = fe_resumida, y = prop, fill = causa_categoria)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = cinzas_5, name = "Causa") +
  labs(x = "Faixa etária", y = "Proporção de óbitos (%)") +
  scale_fill_manual(
    values = cinzas_5,
    name   = "Causa Base",
    labels = c(
      "Lesão por arma de fogo",
      "Lesão por enforcamento",
      "Lesão por instrumento perfurante,\ncortante ou contundente",
      "Lesão por maus tratos",
      "Outros"
    )
  ) +
  labs(x = "Faixa Etária", y = "Proporção de óbitos (%)") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "plain")
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave("Imagens/4c_causa_faixa_2023.pdf", width = 158, height = 100, units = "mm")

# Causa × Local de ocorrência
cruz_causa_loco <- sim2023 %>%
  filter(!is.na(lococor_grupo)) %>%
  count(lococor_grupo, causa_categoria, name = "obitos") %>%
  group_by(lococor_grupo) %>%
  mutate(prop = round(obitos / sum(obitos) * 100, 1)) %>%
  ungroup()

xtable(cruz_causa_loco)

ggplot(cruz_causa_loco, aes(x = lococor_grupo, y = prop, fill = causa_categoria)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = cinzas_5, name = "Causa") +
  labs(x = "Local de ocorrência", y = "Proporção de óbitos (%)") +
  theme_light() +
  guides(fill = guide_legend(ncol = 1)) +
  scale_fill_manual(
    values = cinzas_5,
    name   = "Causa Base",
    labels = c(
      "Lesão por arma de fogo",
      "Lesão por enforcamento",
      "Lesão por instrumento perfurante,\ncortante ou contundente",
      "Lesão por maus tratos",
      "Outros"
    )
  ) +
  labs(x = "Local de Ocorrência", y = "Proporção de óbitos (%)") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "plain")
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave("Imagens/4d_causa_local_2023.pdf", width = 158, height = 100, units = "mm")


# Faixa etária e Escolaridade
fe_esc <- sim2023 %>%
  filter(!is.na(fe_resumida), !is.na(ESC_GRUPO)) %>%
  count(fe_resumida, ESC_GRUPO, name = "obitos") %>%
  group_by(fe_resumida) %>%
  mutate(prop = round(obitos / sum(obitos) * 100, 1)) %>%
  ungroup() %>%
  mutate(
    fe_resumida = str_replace(fe_resumida, " anos", "\nanos")
  )

xtable(fe_esc)

ggplot(fe_esc, aes(x = fe_resumida, y = prop, fill = ESC_GRUPO)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = cinzas_5, name = "Escolaridade") +
  labs(x = "Faixa Etária", y = "Proporção de óbitos (%)") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "plain")
  ) +
  guides(fill = guide_legend(nrow = 1))

ggsave("Imagens/4e_faixa_escolaridade_2023.pdf", width = 158, height = 100, units = "mm")

# Faixa etária e Raça/cor
fe_raca <- sim2023 %>%
  filter(!is.na(fe_resumida), !is.na(raca_cor)) %>%
  count(fe_resumida, raca_cor, name = "obitos") %>%
  group_by(fe_resumida) %>%
  mutate(prop = round(obitos / sum(obitos) * 100, 1)) %>%
  ungroup() %>%
  mutate(
    fe_resumida = str_replace(fe_resumida, " anos", "\nanos")
  )

xtable(fe_raca)

ggplot(fe_raca, aes(x = fe_resumida, y = prop, fill = raca_cor)) +
  geom_col(position = "fill", color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = cinzas_3, name = "Raça/Cor") +
  labs(x = "Faixa Etária", y = "Proporção de óbitos (%)") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "plain")
  ) +
  guides(fill = guide_legend(nrow = 1))

ggsave("Imagens/4f_faixa_raca_2023.pdf", width = 158, height = 100, units = "mm")