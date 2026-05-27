# ============================================================================ #
#                 MODELAGEM QUASI e GAMLSS                                      ----
# ============================================================================ #

source("Codigos/1-Tratamento.R")

# ---- Base para a Regressão ---------------------------------------------------

base_regressao <- sim %>%
  filter(fe_resumida == "15 a 29 anos",
         Ano == 2023) %>% 
  group_by(Ano, munResUf, Regiao, raca_cor) %>%
  summarise(obitos = n(), .groups = "drop") %>%
  left_join(
    dados_projecao %>%
      filter(!LOCAL %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) %>%
      group_by(munResUf = LOCAL,
               Ano = as.numeric(ano)) %>%
      summarise(pop = sum(pop), .groups = "drop"),
    by = c("munResUf", "Ano"))

# ---- Categorias de Referência ------------------------------------------------

base_regressao$raca_cor <- relevel(factor(base_regressao$raca_cor), ref = "Branca")
base_regressao$Regiao   <- relevel(factor(base_regressao$Regiao), ref = "Norte")

# ============================================================================ #
#                 Modelagem 2023
# ============================================================================ #

# ---- Modelo Quasi-Vero -------------------------------------------------------

modelo_quasi_vero_simples <- glm(obitos ~ raca_cor + Regiao + log(pop),
                                 family = quasi(link = log, variance = "mu"),
                                 data = base_regressao)

summary(modelo_quasi_vero_simples)
summary(modelo_quasi_vero_simples)$dispersion
plot(modelo_quasi_vero_simples)

# ---- GAMLSS Poisson ----------------------------------------------------------

# PO
modelo_gamlss_pois_PO <- gamlss(obitos ~ raca_cor + Regiao + log(pop),
                             family = PO, data = base_regressao)

summary(modelo_gamlss_pois_PO)
plot(modelo_gamlss_pois_PO)
wp(modelo_gamlss_pois_PO)


# PIG (Poisson-Inversa Gaussiana)
modelo_gamlss_pois_PIG <- gamlss(obitos ~ raca_cor + Regiao + log(pop),
                             family = PIG, data = base_regressao)

summary(modelo_gamlss_pois_PIG)
plot(modelo_gamlss_pois_PIG)
wp(modelo_gamlss_pois_PIG)


# ZIG (Poisson inflacionada de zeros)
modelo_gamlss_pois_ZIP <- gamlss(obitos ~ raca_cor + Regiao + log(pop),
                                 family = ZIP, data = base_regressao)

summary(modelo_gamlss_pois_ZIP)
plot(modelo_gamlss_pois_ZIP)
wp(modelo_gamlss_pois_ZIP)

# ---- GAMLSS Binomial Negativa ------------------------------------------------

# NBI
modelo_gamlss_bn_NBI <- gamlss(obitos ~ raca_cor + Regiao + log(pop),
                              family = NBI, data = base_regressao)

summary(modelo_gamlss_bn_NBI)
plot(modelo_gamlss_bn_NBI)
wp(modelo_gamlss_bn_NBI)

# NBII
modelo_gamlss_bn_NBII <- gamlss(obitos ~ raca_cor + Regiao + log(pop),
                                family = NBII, data = base_regressao)

summary(modelo_gamlss_bn_NBII)
plot(modelo_gamlss_bn_NBII)
wp(modelo_gamlss_bn_NBII)

# ZINBI
modelo_gamlss_bn_ZINBI <- gamlss(obitos ~ raca_cor + Regiao + log(pop),
                                 family = ZINBI, data = base_regressao)

summary(modelo_gamlss_bn_ZINBI)
plot(modelo_gamlss_bn_ZINBI)
wp(modelo_gamlss_bn_ZINBI)








