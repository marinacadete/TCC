source("Codigos/Tratamento.R")

# ---------------------------------------------------------------------------- #
# Projeção Brasil                                                               ----
# ---------------------------------------------------------------------------- #
pop_fem_br <- dados_projecao %>%
  filter(SIGLA == "BR",
         IDADE >= 14, 
         IDADE <= 64) %>% 
  mutate(ano = as.integer(ano),
         pop = as.numeric(pop)) %>%
  group_by(ano) %>%
  summarise(pop_fem = sum(pop, na.rm = TRUE), .groups = "drop")

obitos_fem_br <- sim %>%
  mutate(ano = year(DTOBITO)) %>%
  count(ano, name = "obitos_fem")

taxa_fem_br <- obitos_fem_br %>%
  left_join(pop_fem_br, by = "ano") %>%
  mutate(taxa_100k = (obitos_fem / pop_fem) * 1e5) %>%
  arrange(ano); taxa_fem_br


# ---------------------------------------------------------------------------- #
# Projeção Região                                                               ----
# ---------------------------------------------------------------------------- #
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
# Projeção UF                                                                   ----
# ---------------------------------------------------------------------------- #
pop_fem_uf <- dados_projecao %>%
  filter(!LOCAL %in% c("Centro-Oeste", "Nordeste", "Norte", 
                       "Sudeste", "Sul", "Brasil"),
         IDADE >= 14, 
         IDADE <= 64) %>%
  mutate(ano = as.integer(ano),
         pop = as.numeric(pop)) %>%
  group_by(munResUf = LOCAL, ano) %>%
  summarise(pop_fem = sum(pop, na.rm = TRUE), .groups = "drop")

obitos_fem_uf <- sim %>%
  select(munResUf, codUF, DTOBITO) %>% 
  mutate(ano = year(DTOBITO)) %>%
  count(codUF, munResUf, ano, name = "obitos_fem")

taxa_fem_uf <- obitos_fem_uf %>%
  left_join(pop_fem_uf, by = c("munResUf", "ano")) %>%
  mutate(taxa_100k = (obitos_fem / pop_fem) * 1e5) %>%
  arrange(munResUf, ano); taxa_fem_uf

taxa_uf_wide <- taxa_fem_uf %>%
  dplyr::select(codUF, munResUf, ano, taxa_100k) %>%
  tidyr::pivot_wider(
    names_from = ano,
    values_from = taxa_100k
  ) %>%
  dplyr::arrange(codUF)

xtable(taxa_uf_wide)


# ---------------------------------------------------------------------------- #
# Projeção Faixa Etária                                                                   ----
# ---------------------------------------------------------------------------- #
pop_fem_idade <- dados_projecao %>%
  filter(!LOCAL %in% c("BR"),
         IDADE >= 14, 
         IDADE <= 64) %>%
  mutate(ano = as.integer(ano),
         pop = as.numeric(pop)) %>%
  group_by(IDADEanos = IDADE, ano) %>%
  summarise(pop_fem = sum(pop, na.rm = TRUE), .groups = "drop")

obitos_fem_idade <- sim %>%
  select(IDADEanos, DTOBITO) %>% 
  mutate(ano = year(DTOBITO)) %>%
  count(IDADEanos, ano, name = "obitos_fem")

obitos_fem_idade_agru <- sim %>%
  select(IDADEanos, fe_quinque, DTOBITO) %>% 
  mutate(ano = year(DTOBITO)) %>%
  count(IDADEanos, fe_quinque, ano, name = "obitos_fem")

# ---------------------------------------------------------------------------- #
taxa_fem_idade_simples <- obitos_fem_idade %>%
  left_join(pop_fem_idade, by = c("IDADEanos", "ano")) %>%
  mutate(taxa_100k = (obitos_fem / pop_fem) * 1e5) %>%
  arrange(IDADEanos, ano); taxa_fem_idade_simples

taxa_idade_simples_wide <- taxa_fem_idade_simples %>%
  dplyr::select(IDADEanos, ano, taxa_100k) %>%
  tidyr::pivot_wider(
    names_from = ano,
    values_from = taxa_100k
  ) %>%
  dplyr::arrange(IDADEanos)

xtable(taxa_idade_simples_wide)
# ---------------------------------------------------------------------------- #

taxa_fem_idade_agrupado <- obitos_fem_idade_agru %>%
  left_join(pop_fem_idade, by = c("IDADEanos", "ano")) %>%
  select(fe_quinque, ano, obitos_fem, pop_fem) %>%
  group_by(fe_quinque, ano) %>% 
  summarise(
    obitos_fem = sum(obitos_fem, na.rm = TRUE),
    pop_fem = sum(pop_fem, na.rm = TRUE)
  ) %>%
  mutate(taxa_100k = (obitos_fem / pop_fem) * 1e5) %>%
  arrange(fe_quinque, ano); taxa_fem_idade_agrupado

taxa_idade_wide <- taxa_fem_idade_agrupado %>%
  dplyr::select(fe_quinque, ano, taxa_100k) %>%
  tidyr::pivot_wider(
    names_from = ano,
    values_from = taxa_100k
  ) %>%
  dplyr::arrange(fe_quinque)


# ---------------------------------------------------------------------------- #
# Proporção Raça/Cor                                                           # ----
# ---------------------------------------------------------------------------- #
pop_fem_raca <- prop_RacaCor %>%
  filter(!Local %in% c("Centro-Oeste", "Nordeste", "Norte", 
                       "Sudeste", "Sul", "Brasil")) %>%
  mutate(pop = as.numeric(População)) %>%
  group_by(munResUf = Local, 
           RACACOR = `Raça/cor`) %>% 
  dplyr::select(munResUf, RACACOR, pop) %>% 
  summarise(pop_fem = sum(pop, na.rm = TRUE), .groups = "drop")

obitos_fem_raca <- sim %>%
  dplyr::select(munResUf, RACACOR) %>% 
  count(munResUf, RACACOR, name = "obitos_fem")

taxa_fem_raca <- obitos_fem_raca %>%
  left_join(pop_fem_raca, by = c("munResUf", "RACACOR")) %>%
  mutate(taxa_100k = (obitos_fem / pop_fem) * 1e5) %>%
  arrange(munResUf, RACACOR); taxa_fem_raca

taxa_uf_raca_tab <- taxa_fem_raca %>%
  dplyr::select(codUF, munResUf, ano, taxa_100k) %>%
  tidyr::pivot_wider(
    names_from = ano,
    values_from = taxa_100k
  ) %>%
  dplyr::arrange(codUF)

xtable(taxa_uf_wide)

