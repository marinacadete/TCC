# ============================================================================ #
#                 MODELAGEM SUPER SIMPLES                                       ----
# ============================================================================ #

source("Codigos/1-Tratamento.R")

# ---- Base para a Regressão ---------------------------------------------------

base_regressao <- sim %>%
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
#                 Modelagem 2020-2023
# ============================================================================ #

modelo_pois_log <- glm(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop),
                       family = poisson(link = "log"), data = base_regressao)

modelo_pois_raiz <- glm(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop),
                        family = poisson(link = "sqrt"), data = base_regressao,
                        start = coef(modelo_pois_log))

modelo_pois_identidade <- glm(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop),
                              family = poisson(link = "identity"), data = base_regressao,
                              start = coef(modelo_pois_log))

modelo_pois_inversa <- glm(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop),
                           family = poisson(link = "inverse"), data = base_regressao)

AIC(modelo_pois_log, modelo_pois_raiz, modelo_pois_identidade, modelo_pois_inversa)
BIC(modelo_pois_log, modelo_pois_raiz, modelo_pois_identidade, modelo_pois_inversa)

# ---- Modelo Poisson ----------------------------------------------------------

modelo_pois_simples <- glm(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop),
                           family = poisson(link = "log"), data = base_regressao)
# modelo_pois_simples <- glm(obitos ~ raca_cor + Regiao + offset(log(pop)),
#                            family = poisson(link = "log"), data = base_regressao)

summary(modelo_pois_simples)
dispersiontest(modelo_pois_simples)
plot(modelo_pois_simples)
fit.model <- modelo_pois_simples
source("Codigos/source/Envel_pois.R")
source("Codigos/source/Diag_pois.R")

# ---- Modelo BN ---------------------------------------------------------------

modelo_bn_simples <- glm.nb(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop),
                            data = base_regressao)
# modelo_bn_simples <- glm.nb(obitos ~ raca_cor + Regiao + offset(log(pop)),
#                             data = base_regressao)

summary(modelo_bn_simples)
plot(modelo_bn_simples)
fit.model <- modelo_bn_simples
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
# modelo_pois_simples_2023 <- glm(obitos ~ raca_cor + Regiao + offset(log(pop)),
#                                 family = poisson, data = base_2023)

summary(modelo_pois_simples_2023)
dispersiontest(modelo_pois_simples_2023)
plot(modelo_pois_simples_2023)
fit.model <- modelo_pois_simples_2023
source("Codigos/source/Envel_pois.R")
source("Codigos/source/Diag_pois.R")

# ---- Modelo BN ---------------------------------------------------------------

modelo_bn_simples_2023 <- glm.nb(obitos ~ raca_cor + Regiao + log(pop),
                                 data = base_2023)
# modelo_bn_simples_2023 <- glm.nb(obitos ~ raca_cor + Regiao + offset(log(pop)),
#                                  data = base_2023)

summary(modelo_bn_simples_2023)
plot(modelo_bn_simples_2023)
fit.model <- modelo_bn_simples_2023
source("Codigos/source/Envel_bn.R")
source("Codigos/source/Diag_bn.R")

# ============================================================================ #
#                 MODELAGEM ACRESCENTANDO "15 a 29 anos"                        ----
# ============================================================================ #

source("Codigos/1-Tratamento.R")

# ---- Base para a Regressão ---------------------------------------------------

base_regressao <- sim %>%
  filter(fe_resumida == "15 a 29 anos") %>% 
  group_by(Ano, munResUf, Regiao, raca_cor) %>%
  summarise(obitos = n(), .groups = "drop") %>%
  left_join(
    dados_projecao %>%
      filter(!LOCAL %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"),
             IDADE <= 29) %>%
      group_by(munResUf = LOCAL,
               Ano = as.numeric(ano)) %>%
      summarise(pop = sum(pop), .groups = "drop"),
    by = c("munResUf", "Ano"))


# ---- Categorias de Referência ------------------------------------------------

base_regressao$raca_cor <- relevel(factor(base_regressao$raca_cor), ref = "Branca")
base_regressao$Regiao   <- relevel(factor(base_regressao$Regiao), ref = "Norte")

# ============================================================================ #
#                 Modelagem 2020-2023
# ============================================================================ #

# ---- Modelo Poisson ----------------------------------------------------------

modelo_pois_simples <- glm(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop),
                           family = poisson, data = base_regressao)
# modelo_pois_simples <- glm(obitos ~ raca_cor + Regiao + offset(log(pop)),
#                            family = poisson, data = base_regressao)

summary(modelo_pois_simples)
dispersiontest(modelo_pois_simples)
plot(modelo_pois_simples)
fit.model <- modelo_pois_simples
source("Codigos/source/Envel_pois.R")
source("Codigos/source/Diag_pois.R")

# ---- Modelo BN ---------------------------------------------------------------

modelo_bn_simples <- glm.nb(obitos ~ factor(Ano) + raca_cor + Regiao + log(pop),
                            data = base_regressao)
# modelo_bn_simples <- glm.nb(obitos ~ raca_cor + Regiao + offset(log(pop)),
#                             data = base_regressao)

summary(modelo_bn_simples)
plot(modelo_bn_simples)
fit.model <- modelo_bn_simples
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
# modelo_pois_simples_2023 <- glm(obitos ~ raca_cor + Regiao + offset(log(pop)),
#                                 family = poisson, data = base_2023)

summary(modelo_pois_simples_2023)
dispersiontest(modelo_pois_simples_2023)
plot(modelo_pois_simples_2023)
fit.model <- modelo_pois_simples_2023
source("Codigos/source/Envel_pois.R")
source("Codigos/source/Diag_pois.R")

# ---- Modelo BN ---------------------------------------------------------------

modelo_bn_simples_2023 <- glm.nb(obitos ~ raca_cor + Regiao + log(pop),
                                 data = base_2023)
# modelo_bn_simples_2023 <- glm.nb(obitos ~ raca_cor + Regiao + offset(log(pop)),
#                                  data = base_2023)

summary(modelo_bn_simples_2023)
plot(modelo_bn_simples_2023)
fit.model <- modelo_bn_simples_2023
source("Codigos/source/Envel_bn.R")
source("Codigos/source/Diag_bn.R")


# ============================================================================ #
#                 MODELAGEM ACRESCENTANDO "ESTADO CIVIL" e "ESCOLARIDADE"       ----
# ============================================================================ #

source("Codigos/1-Tratamento.R")

# ---- Base para a Regressão ---------------------------------------------------

base_regressao <- sim %>%
  filter(fe_resumida == "15 a 29 anos",
         !is.na(ESTCIV),
         #!is.na(ESC),
         Ano == 2023) %>% 
  group_by(Ano, munResUf, Regiao, raca_cor, 
           estciv_grupo 
           #ESC_GRUPO
           ) %>%
  summarise(obitos = n(), .groups = "drop") %>%
  left_join(
    dados_projecao %>%
      filter(!LOCAL %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"),
             IDADE <= 29,
             ano == 2023) %>%
      group_by(munResUf = LOCAL,
               Ano = as.numeric(ano)) %>%
      summarise(pop = sum(pop), .groups = "drop"),
    by = c("munResUf", "Ano"))


# ---- Categorias de Referência ------------------------------------------------

base_regressao$raca_cor      <- relevel(factor(base_regressao$raca_cor), ref = "Branca")
base_regressao$Regiao        <- relevel(factor(base_regressao$Regiao), ref = "Norte")
base_regressao$estciv_grupo  <- relevel(factor(base_regressao$estciv_grupo), ref = "Com companheiro")
#base_regressao$ESC_GRUPO     <- relevel(factor(base_regressao$ESC_GRUPO), ref = "Menos de 8 anos de Estudo")

# ============================================================================ #
#                 Modelagem 2023
# ============================================================================ #

# ---- Modelo Poisson ----------------------------------------------------------

modelo_pois_simples_2023 <- glm(obitos ~ raca_cor + Regiao + #ESC_GRUPO + 
                                  estciv_grupo + 
                                  log(pop),
                                family = poisson, data = base_regressao)
# modelo_pois_simples_2023 <- glm(obitos ~ raca_cor + Regiao + offset(log(pop)),
#                                 family = poisson, data = base_regressao)

summary(modelo_pois_simples_2023)
dispersiontest(modelo_pois_simples_2023)
plot(modelo_pois_simples_2023)
fit.model <- modelo_pois_simples_2023
source("Codigos/source/Envel_pois.R")
source("Codigos/source/Diag_pois.R")

# ---- Modelo BN ---------------------------------------------------------------

modelo_bn_simples_2023 <- glm.nb(obitos ~ raca_cor + Regiao + #ESC_GRUPO + 
                                   estciv_grupo + 
                                   log(pop),
                                 data = base_regressao)
modelo_bn_simples_2023 <- glm.nb(obitos ~ raca_cor + Regiao + #ESC_GRUPO + 
                                   estciv_grupo + offset(log(pop)),
                                data = base_regressao)

summary(modelo_bn_simples_2023)
plot(modelo_bn_simples_2023)
fit.model <- modelo_bn_simples_2023
source("Codigos/source/Envel_bn.R")
source("Codigos/source/Diag_bn.R")

