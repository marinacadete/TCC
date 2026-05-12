# ============================================================================ #
#               Taxas de Mortalidade
# ============================================================================ #

source("Codigos/1-Tratamento.R")

# ---- Função auxiliar ---------------------------------------------------------

calcular_taxa <- function(obitos, pop, by) {
  left_join(obitos, pop, by = by) %>%
    mutate(taxa_100k = obitos_fem / pop_fem * 1e5)
}

# ---- Brasil ------------------------------------------------------------------

taxa_fem_br <- sim %>%
  count(Ano, name = "obitos_fem") %>%
  calcular_taxa(
    dados_projecao %>%
      filter(SIGLA == "BR") %>%
      group_by(ano = ano) %>%
      summarise(pop_fem = sum(pop)),
    by = c("Ano" = "ano")
  ) %>%
  arrange(Ano)

# ---- Região ------------------------------------------------------------------

taxa_regiao <- sim %>%
  count(Regiao, Ano, name = "obitos_fem") %>%
  calcular_taxa(
    dados_projecao %>%
      filter(LOCAL %in% c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul")) %>%
      group_by(Regiao = LOCAL, ano = as.integer(ano)) %>%
      summarise(pop_fem = sum(pop), .groups = "drop"),
    by = c("Regiao", "Ano" = "ano")
  ) %>%
  select(Regiao, Ano, taxa_100k) %>%
  pivot_wider(names_from = Ano, values_from = taxa_100k) %>%
  arrange(Regiao)

# ---- UF ----------------------------------------------------------------------

taxa_uf <- sim %>%
  count(codUF, munResUf, Ano, name = "obitos_fem") %>%
  calcular_taxa(
    dados_projecao %>%
      filter(!LOCAL %in% c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul", "Brasil")) %>%
      group_by(munResUf = LOCAL, ano = as.integer(ano)) %>%
      summarise(pop_fem = sum(pop), .groups = "drop"),
    by = c("munResUf", "Ano" = "ano")
  ) %>%
  select(codUF, munResUf, Ano, taxa_100k) %>%
  pivot_wider(names_from = Ano, values_from = taxa_100k) %>%
  arrange(codUF)

# ---- Faixa etária ------------------------------------------------------------

taxa_idade_simples <- sim %>%
  count(IDADEanos, Ano, name = "obitos_fem") %>%
  calcular_taxa(
    dados_projecao %>%
      filter(LOCAL == "Brasil") %>%
      group_by(IDADEanos = IDADE, ano = as.integer(ano)) %>%
      summarise(pop_fem = sum(pop), .groups = "drop"),
    by = c("IDADEanos", "Ano" = "ano")
  ) %>%
  select(IDADEanos, Ano, taxa_100k) %>%
  pivot_wider(names_from = Ano, values_from = taxa_100k) %>%
  arrange(IDADEanos)

taxa_idade_quinque <- sim %>%
  count(IDADEanos, fe_quinque, Ano, name = "obitos_fem") %>%
  calcular_taxa(
    dados_projecao %>%
      filter(LOCAL == "Brasil") %>%
      group_by(IDADEanos = IDADE, ano = as.integer(ano)) %>%
      summarise(pop_fem = sum(pop), .groups = "drop"),
    by = c("IDADEanos", "Ano" = "ano")
  ) %>%
  group_by(fe_quinque, Ano) %>%
  summarise(across(c(obitos_fem, pop_fem), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(taxa_100k = obitos_fem / pop_fem * 1e5) %>%
  select(fe_quinque, Ano, taxa_100k) %>%
  pivot_wider(names_from = Ano, values_from = taxa_100k) %>%
  arrange(fe_quinque)

taxa_idade_resumida <- sim %>%
  count(IDADEanos, fe_resumida, Ano, name = "obitos_fem") %>%
  calcular_taxa(
    dados_projecao %>%
      filter(LOCAL == "Brasil") %>%
      group_by(IDADEanos = IDADE, ano = as.integer(ano)) %>%
      summarise(pop_fem = sum(pop), .groups = "drop"),
    by = c("IDADEanos", "Ano" = "ano")
  ) %>%
  group_by(fe_resumida, Ano) %>%
  summarise(across(c(obitos_fem, pop_fem), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(taxa_100k = obitos_fem / pop_fem * 1e5) %>%
  select(fe_resumida, Ano, taxa_100k) %>%
  pivot_wider(names_from = Ano, values_from = taxa_100k) %>%
  arrange(fe_resumida)

# ---- Raça/Cor ----------------------------------------------------------------

taxa_racaCor <- sim %>%
  count(raca_cor, fe_quinque, Ano, name = "obitos_fem") %>%
  left_join(
    dados_projecao %>%
      filter(LOCAL == "Brasil") %>%
      group_by(faixa_etaria, ano = as.integer(ano)) %>%
      summarise(pop_fem = sum(pop), .groups = "drop"),
    by = c("fe_quinque" = "faixa_etaria", "Ano" = "ano")
  ) %>%
  left_join(
    prop_RacaCor %>% filter(Local == "Brasil"),
    by = c("raca_cor" = "Raça/cor", "fe_quinque" = "Faixa_etaria")
  ) %>%
  mutate(
    pop_raca = pop_fem * Proporcao,
    taxa_100k = obitos_fem / pop_raca * 1e5
  )
