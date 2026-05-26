# ============================================================================ #
#                 MODELAGEM: QUASI-POISSON e GAMLSS                             ----
# ============================================================================ #

source("Codigos/1-Tratamento.R")

# ---- Base para a Regressão ---------------------------------------------------

base_regressao <- sim %>%
  group_by(Ano, munResUf, Regiao, raca_cor, causa_categoria) %>%
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
base_regressao$Regiao   <- relevel(factor(base_regressao$Regiao), ref = "Centro-Oeste")

# ============================================================================ #
#                 Modelagem 2020-2023
# ============================================================================ #

base_regressao_arma         <- base_regressao %>% filter(causa_categoria == "Lesão por arma de fogo")
base_regressao_cortante     <- base_regressao %>% filter(causa_categoria == "Lesão por instrumento perfurante, cortante ou contundente")
base_regressao_enforcamento <- base_regressao %>% filter(causa_categoria == "Lesão por enforcamento")
base_regressao_mausTratos   <- base_regressao %>% filter(causa_categoria == "Lesão por maus tratos")
base_regressao_outros       <- base_regressao %>% filter(causa_categoria == "Outros")

# ---- Modelo Quasi-Poisson ----------------------------------------------------

modelo_quasi_simples_arma         <- glm(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop), family = quasipoisson, data = base_regressao_arma)
modelo_quasi_simples_cortante     <- glm(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop), family = quasipoisson, data = base_regressao_cortante)
modelo_quasi_simples_enforcamento <- glm(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop), family = quasipoisson, data = base_regressao_enforcamento)
modelo_quasi_simples_mausTratos   <- glm(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop), family = quasipoisson, data = base_regressao_mausTratos)
modelo_quasi_simples_outros       <- glm(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop), family = quasipoisson, data = base_regressao_outros)

summary(modelo_quasi_simples_arma)
summary(modelo_quasi_simples_cortante)
summary(modelo_quasi_simples_enforcamento)
summary(modelo_quasi_simples_mausTratos)
summary(modelo_quasi_simples_outros)

summary(modelo_quasi_simples_arma)$dispersion
summary(modelo_quasi_simples_cortante)$dispersion
summary(modelo_quasi_simples_enforcamento)$dispersion
summary(modelo_quasi_simples_mausTratos)$dispersion
summary(modelo_quasi_simples_outros)$dispersion


# ---- Modelo GAMLSS: Poisson-Normal (PO com efeito aleatório / PIG) -----------
# Família PIG (Poisson-Inversa Gaussiana) é a "Poisson-Normal" mais usada em gamlss
# Alternativa: NBI (Negative Binomial type I) -> equivalente à BN clássica

modelo_gamlss_pn_arma <- gamlss(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop),
                                family = PIG, data = base_regressao_arma)
modelo_gamlss_pn_cortante <- gamlss(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop),
                                    family = PIG, data = base_regressao_cortante)
modelo_gamlss_pn_enforcamento <- gamlss(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop),
                                        family = PIG, data = base_regressao_enforcamento)
modelo_gamlss_pn_mausTratos <- gamlss(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop),
                                      family = PIG, data = base_regressao_mausTratos)
modelo_gamlss_pn_outros <- gamlss(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop),
                                  family = PIG, data = base_regressao_outros)

summary(modelo_gamlss_pn_arma)
summary(modelo_gamlss_pn_cortante)
summary(modelo_gamlss_pn_enforcamento)
summary(modelo_gamlss_pn_mausTratos)
summary(modelo_gamlss_pn_outros)

# Diagnóstico gamlss: worm plot e resíduos quantílicos
plot(modelo_gamlss_pn_arma);         wp(modelo_gamlss_pn_arma)
plot(modelo_gamlss_pn_cortante);     wp(modelo_gamlss_pn_cortante)
plot(modelo_gamlss_pn_enforcamento); wp(modelo_gamlss_pn_enforcamento)
plot(modelo_gamlss_pn_mausTratos);   wp(modelo_gamlss_pn_mausTratos)
plot(modelo_gamlss_pn_outros);       wp(modelo_gamlss_pn_outros)

# ---- Modelo GAMLSS: Binomial Negativa (NBI) ----------------------------------

modelo_gamlss_bn_arma <- gamlss(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop),
                                family = NBI, data = base_regressao_arma)
modelo_gamlss_bn_cortante <- gamlss(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop),
                                    family = NBI, data = base_regressao_cortante)
modelo_gamlss_bn_enforcamento <- gamlss(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop),
                                        family = NBI, data = base_regressao_enforcamento)
modelo_gamlss_bn_mausTratos <- gamlss(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop),
                                      family = NBI, data = base_regressao_mausTratos)
modelo_gamlss_bn_outros <- gamlss(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop),
                                  family = NBI, data = base_regressao_outros)

summary(modelo_gamlss_bn_arma)
summary(modelo_gamlss_bn_cortante)
summary(modelo_gamlss_bn_enforcamento)
summary(modelo_gamlss_bn_mausTratos)
summary(modelo_gamlss_bn_outros)

plot(modelo_gamlss_bn_arma);         wp(modelo_gamlss_bn_arma)
plot(modelo_gamlss_bn_cortante);     wp(modelo_gamlss_bn_cortante)
plot(modelo_gamlss_bn_enforcamento); wp(modelo_gamlss_bn_enforcamento)
plot(modelo_gamlss_bn_mausTratos);   wp(modelo_gamlss_bn_mausTratos)
plot(modelo_gamlss_bn_outros);       wp(modelo_gamlss_bn_outros)

# ---- Comparação entre modelos por categoria ----------------------------------

GAIC(modelo_gamlss_pn_arma, modelo_gamlss_bn_arma)
GAIC(modelo_gamlss_pn_cortante, modelo_gamlss_bn_cortante)
GAIC(modelo_gamlss_pn_enforcamento, modelo_gamlss_bn_enforcamento)
GAIC(modelo_gamlss_pn_mausTratos, modelo_gamlss_bn_mausTratos)
GAIC(modelo_gamlss_pn_outros, modelo_gamlss_bn_outros)

# ============================================================================ #
#                 Modelagem 2023 APENAS
# ============================================================================ #

base_2023 <- base_regressao %>%
  filter(Ano == 2023)

# ---- Modelo Quasi-Poisson ----------------------------------------------------

modelo_quasi_simples_2023 <- glm(obitos ~ raca_cor + Regiao + log(pop),
                                 family = quasipoisson, data = base_2023)
modelo_quasi_simples_2023 <- glm(obitos ~ raca_cor + Regiao + offset(log(pop)),
                                 family = quasipoisson, data = base_2023)

summary(modelo_quasi_simples_2023)
summary(modelo_quasi_simples_2023)$dispersion
plot(modelo_quasi_simples_2023)

fit.model <- modelo_quasi_simples_2023
source("Codigos/source/Envel_pois.R")
source("Codigos/source/Diag_pois.R")

# ---- Modelo GAMLSS: Poisson-Normal (PIG) -------------------------------------

modelo_gamlss_pn_2023 <- gamlss(obitos ~ raca_cor + Regiao + log(pop),
                                family = PIG, data = base_2023)
modelo_gamlss_pn_2023 <- gamlss(obitos ~ raca_cor + Regiao + offset(log(pop)),
                                family = PIG, data = base_2023)

summary(modelo_gamlss_pn_2023)
plot(modelo_gamlss_pn_2023)
wp(modelo_gamlss_pn_2023)

# ---- Modelo GAMLSS: Binomial Negativa (NBI) ----------------------------------

modelo_gamlss_bn_2023 <- gamlss(obitos ~ raca_cor + Regiao + log(pop),
                                family = NBI, data = base_2023)
modelo_gamlss_bn_2023 <- gamlss(obitos ~ raca_cor + Regiao + offset(log(pop)),
                                family = NBI, data = base_2023)

summary(modelo_gamlss_bn_2023)
plot(modelo_gamlss_bn_2023)
wp(modelo_gamlss_bn_2023)

# ---- Comparação 2023 ---------------------------------------------------------

GAIC(modelo_gamlss_pn_2023, modelo_gamlss_bn_2023)