# ============================================================================ #
#                 MODELAGEM SUPER SIMPLES por CAUSA                             ----
# ============================================================================ #

source("Codigos/1-Tratamento.R")

# ---- Base para a Regressão ---------------------------------------------------

base_regressao <- sim %>%
  group_by(Ano, munResUf, Regiao, raca_cor,causa_categoria) %>%
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

# ---- Modelo Poisson ----------------------------------------------------------

modelo_pois_simples_arma         <- glm(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop), family = poisson, data = base_regressao_arma)
modelo_pois_simples_cortante     <- glm(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop), family = poisson, data = base_regressao_cortante)
modelo_pois_simples_enforcamento <- glm(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop), family = poisson, data = base_regressao_enforcamento)
modelo_pois_simples_mausTratos   <- glm(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop), family = poisson, data = base_regressao_mausTratos)
modelo_pois_simples_outros       <- glm(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop), family = poisson, data = base_regressao_outros)

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


fit.model <- modelo_pois_simples_arma
source("Codigos/source/Envel_pois.R")
source("Codigos/source/Diag_pois.R")

fit.model <- modelo_pois_simples_cortante
source("Codigos/source/Envel_pois.R")
source("Codigos/source/Diag_pois.R")

fit.model <- modelo_pois_simples_enforcamento
source("Codigos/source/Envel_pois.R")
source("Codigos/source/Diag_pois.R")

fit.model <- modelo_pois_simples_mausTratos
source("Codigos/source/Envel_pois.R")
source("Codigos/source/Diag_pois.R")

fit.model <- modelo_pois_simples_outros
source("Codigos/source/Envel_pois.R")
source("Codigos/source/Diag_pois.R")

# ---- Modelo BN ---------------------------------------------------------------

modelo_bn_simples_arma         <- glm.nb(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop), data = base_regressao_arma)
modelo_bn_simples_cortante     <- glm.nb(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop), data = base_regressao_cortante)
modelo_bn_simples_enforcamento <- glm.nb(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop), data = base_regressao_enforcamento)
modelo_bn_simples_mausTratos   <- glm.nb(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop), data = base_regressao_mausTratos)
modelo_bn_simples_outros       <- glm.nb(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop), data = base_regressao_outros)

summary(modelo_bn_simples_arma)
summary(modelo_bn_simples_cortante)
summary(modelo_bn_simples_enforcamento)
summary(modelo_bn_simples_mausTratos)
summary(modelo_bn_simples_outros) 

fit.model <- modelo_bn_simples_arma
source("Codigos/source/Envel_bn.R")
source("Codigos/source/Diag_bn.R")

fit.model <- modelo_bn_simples_cortante
source("Codigos/source/Envel_bn.R")
source("Codigos/source/Diag_bn.R")

fit.model <- modelo_bn_simples_enforcamento
source("Codigos/source/Envel_bn.R")
source("Codigos/source/Diag_bn.R")

fit.model <- modelo_bn_simples_mausTratos
source("Codigos/source/Envel_bn.R")
source("Codigos/source/Diag_bn.R")

fit.model <- modelo_bn_simples_outros
source("Codigos/source/Envel_bn.R")
source("Codigos/source/Diag_bn.R")

# ============================================================================ #
#                 Modelagem 2023 APENAS
# ============================================================================ #

base_2023 <- base_regressao %>%
  filter(Ano == 2023)

# ---- Modelo Poisson ----------------------------------------------------------

modelo_pois_simples_2023 <- glm(obitos ~ raca_cor + Regiao + log(pop),
                                family = poisson, data = base_2023)
modelo_pois_simples_2023 <- glm(obitos ~ raca_cor + Regiao + offset(log(pop)),
                                family = poisson, data = base_2023)

summary(modelo_pois_simples_2023)
dispersiontest(modelo_pois_simples_2023)
plot(modelo_pois_simples_2023)
fit.model <- modelo_pois_simples_2023
source("Codigos/source/Envel_pois.R")
source("Codigos/source/Diag_pois.R")

# ---- Modelo BN ---------------------------------------------------------------

modelo_bn_simples_2023 <- glm.nb(obitos ~ raca_cor + Regiao + log(pop),
                                 data = base_2023)
modelo_bn_simples_2023 <- glm.nb(obitos ~ raca_cor + Regiao + offset(log(pop)),
                                 data = base_2023)

summary(modelo_bn_simples_2023)
plot(modelo_bn_simples_2023)
fit.model <- modelo_bn_simples_2023
source("Codigos/source/Envel_bn.R")
source("Codigos/source/Diag_bn.R")

