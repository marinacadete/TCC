# ============================================================================ #
#                 Modelagem 
# ============================================================================ #

source("Codigos/1-Tratamento.R")

# ---- Base para a Regressão ---------------------------------------------------

base_regressao <- sim %>%
  filter(!is.na(ESC), !is.na(ESTCIV)) %>%
  group_by(Ano, munResUf, Regiao, raca_cor, fe_quinque, fe_resumida, ESC_GRUPO, 
           estciv_grupo, causa_categoria) %>%
  summarise(obitos = n(), .groups = "drop") %>%
  left_join(
    dados_projecao %>%
      filter(!LOCAL %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) %>%
      group_by(munResUf = LOCAL,
               fe_quinque = faixa_etaria,
               Ano = as.numeric(ano)) %>%
      summarise(pop = sum(pop), .groups = "drop"), by = c("munResUf", "fe_quinque", "Ano")
    ) %>%
  left_join(
    prop_RacaCor %>% filter(Local == "Brasil"),
    by = c("raca_cor" = "Raça/cor", "fe_quinque" = "Faixa_etaria")) %>%
  mutate(pop_raca = pop * Proporcao)

# ---- Categorias de Referência ------------------------------------------------

base_regressao$raca_cor     <- relevel(factor(base_regressao$raca_cor), ref = "Branca")
base_regressao$fe_resumida  <- relevel(factor(base_regressao$fe_resumida, ordered = FALSE), ref = "45 a 64 anos")
base_regressao$estciv_grupo <- relevel(factor(base_regressao$estciv_grupo), ref = "Com companheiro")
base_regressao$ESC_GRUPO    <- relevel(factor(base_regressao$ESC_GRUPO), ref = "8 anos ou mais de Estudo")
base_regressao$Regiao       <- relevel(factor(base_regressao$Regiao), ref = "Sudeste")

# ---- Modelo Poisson (2020-2023) ----------------------------------------------

modelo_1 <- glm(obitos ~ factor(Ano) + raca_cor + fe_resumida + Regiao + 
                  ESC_GRUPO + estciv_grupo + causa_categoria + 
                offset(log(pop_raca)),
                family = poisson, 
                data = base_regressao)

summary(modelo_1)
dispersiontest(modelo_1)

fit.model <- modelo_1
source("Codigos/source/Envel_pois.R")
source("Codigos/source/Diag_pois.R")

# ---- Modelo BN ---------------------------------------------------------------

modelo_2 <- glm.nb(obitos ~ Ano + raca_cor + fe_resumida + Regiao + ESC_GRUPO + estciv_grupo +
    offset(log(pop_raca)), data = base_regressao)

summary(modelo_2)

fit.model <- modelo_2
source("Codigos/source/Envel_bn.R")
source("Codigos/source/Diag_bn.R")


# ============================================================================ #
#                 Modelagem 2023 APENAS
# ============================================================================ #

base_2023 <- base_regressao %>%
  filter(Ano == 2023)

library(DHARMa)

# ---- Modelo Poisson ----------------------------------------------------------

m0_20023 <- glm(obitos ~ raca_cor + fe_resumida + Regiao + 
                  ESC_GRUPO + estciv_grupo + causa_categoria  + 
                offset(log(pop_raca)), 
                family = poisson, data = base_2023)

summary(m0_20023)

dispersiontest(m0_20023)

fit.model <- m0_20023
source("Codigos/source/Envel_pois.R")
source("Codigos/source/Diag_pois.R")

# ---- Modelo BN ---------------------------------------------------------------

m1_2023 <- glm.nb(obitos ~ raca_cor + fe_quinque + Regiao + ESC_GRUPO + estciv_grupo +
    offset(log(pop_raca)), data = base_2023)

summary(m1_2023)

fit.model <- m1_2023
source("Codigos/source/Envel_bn.R")
source("Codigos/source/Diag_bn.R")
