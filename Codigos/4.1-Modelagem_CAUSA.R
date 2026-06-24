# ============================================================================ #
#                 MODELAGEM SUPER SIMPLES por CAUSA 2023                        ----
# ============================================================================ #

source("Codigos/1-Tratamento.R")

# ---- Base para a Regressão ---------------------------------------------------

base_regressao <- sim %>%
  filter(Ano == 2023) %>%
  group_by(Ano, munResUf, Regiao, raca_cor, fe_resumida, causa_categoria) %>%
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

base_regressao_arma         <- base_regressao %>% filter(causa_categoria == "Lesão por arma de fogo")
base_regressao_cortante     <- base_regressao %>% filter(causa_categoria == "Lesão por instrumento perfurante, cortante ou contundente")
base_regressao_enforcamento <- base_regressao %>% filter(causa_categoria == "Lesão por enforcamento")
base_regressao_mausTratos   <- base_regressao %>% filter(causa_categoria == "Lesão por maus tratos")
base_regressao_outros       <- base_regressao %>% filter(causa_categoria == "Outros")

# ---- Modelo Poisson ----------------------------------------------------------

modelo_pois_simples_arma         <- glm(obitos ~ raca_cor + Regiao + log(pop), family = poisson, data = base_regressao_arma)
modelo_pois_simples_cortante     <- glm(obitos ~ raca_cor + Regiao + log(pop), family = poisson, data = base_regressao_cortante)
modelo_pois_simples_enforcamento <- glm(obitos ~ raca_cor + Regiao + log(pop), family = poisson, data = base_regressao_enforcamento)
modelo_pois_simples_mausTratos   <- glm(obitos ~ raca_cor + Regiao + log(pop), family = poisson, data = base_regressao_mausTratos)
modelo_pois_simples_outros       <- glm(obitos ~ raca_cor + Regiao + log(pop), family = poisson, data = base_regressao_outros)

summary(modelo_pois_simples_arma)
summary(modelo_pois_simples_cortante)
summary(modelo_pois_simples_enforcamento)
summary(modelo_pois_simples_mausTratos)
summary(modelo_pois_simples_outros) 

dispersiontest(modelo_pois_simples_arma)
dispersiontest(modelo_pois_simples_cortante)
dispersiontest(modelo_pois_simples_enforcamento)
dispersiontest(modelo_pois_simples_mausTratos)
dispersiontest(modelo_pois_simples_outros) 

modelos_pois <- list(
  "Arma de fogo" = modelo_pois_simples_arma,
  "Cortante"     = modelo_pois_simples_cortante,
  "Enforcamento" = modelo_pois_simples_enforcamento,
  "Maus tratos"  = modelo_pois_simples_mausTratos,
  "Outros"       = modelo_pois_simples_outros
)

par(mfrow = c(2, 3))
for (causa in names(modelos_pois)) {
  fit.model <- modelos_pois[[causa]]
  source("Codigos/source/Envel_pois.R")
  title(main = paste("Poisson -", causa))
}

par(mfrow = c(1, 1))

# ---- Modelo BN ---------------------------------------------------------------

modelo_bn_simples_arma         <- glm.nb(obitos ~ raca_cor + Regiao + log(pop), data = base_regressao_arma)
modelo_bn_simples_cortante     <- glm.nb(obitos ~ raca_cor + Regiao + log(pop), data = base_regressao_cortante)
modelo_bn_simples_enforcamento <- glm.nb(obitos ~ raca_cor + Regiao + log(pop), data = base_regressao_enforcamento)
modelo_bn_simples_mausTratos   <- glm.nb(obitos ~ raca_cor + Regiao + log(pop), data = base_regressao_mausTratos)
modelo_bn_simples_outros       <- glm.nb(obitos ~ raca_cor + Regiao + log(pop), data = base_regressao_outros)

summary(modelo_bn_simples_arma)
summary(modelo_bn_simples_cortante)
summary(modelo_bn_simples_enforcamento)
summary(modelo_bn_simples_mausTratos)
summary(modelo_bn_simples_outros) 

modelos_bn <- list(
  "Arma de fogo" = modelo_bn_simples_arma,
  "Cortante"     = modelo_bn_simples_cortante,
  "Enforcamento" = modelo_bn_simples_enforcamento,
  "Maus tratos"  = modelo_bn_simples_mausTratos,
  "Outros"       = modelo_bn_simples_outros
)

par(mfrow = c(2, 3))

for (causa in names(modelos_bn)) {
  fit.model <- modelos_bn[[causa]]
  source("Codigos/source/Envel_bn.R")
  title(main = paste("BN -", causa))
}

