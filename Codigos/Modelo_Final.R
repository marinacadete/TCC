source("rdocs/Trat_Dados.R")

# ============================================================================ #
#                 MODELANDO O LOG DA OCORRÊNCIA DE HOMICÍDIO                   #
# ============================================================================ # ----
contagem_2023 <- sim %>%
  filter(Ano==2023) %>% 
  group_by(munResUf, Regiao, raca_cor, ESTCIV, ESC_GRUPO, 
           LOCOCOR, fe_quinque, causa_categoria) %>%
  summarise(contagem = n(), .groups = "drop") %>% 
  mutate(
    Local = as.factor(munResUf),
    Regiao = as.factor(Regiao),
    `Raça/cor` = as.factor(raca_cor),
    ESTCIV = as.factor(ESTCIV),
    ESC_GRUPO = as.factor(ESC_GRUPO),
    LOCOCOR = as.factor(LOCOCOR),
    Faixa_etaria = as.factor(fe_quinque),
    causa_categoria = as.factor(causa_categoria))

pop_prop_2023 <- dados_projecao %>% 
  dplyr::select(LOCAL, ano, pop, faixa_etaria) %>% 
  filter(ano == 2023) %>% 
  group_by(LOCAL, faixa_etaria) %>% 
  summarise(pop = sum(pop)) %>% 
  rename(Local = LOCAL, Faixa_etaria = faixa_etaria) %>% 
  left_join(prop_RacaCor, by = c("Local", "Faixa_etaria")) %>% 
  mutate(populacao_prop = pop * Proporcao) %>%
  dplyr::select(Local, Faixa_etaria, `Raça/cor`, populacao_prop)

modelo_2023 <- contagem_2023 %>% 
  left_join(pop_prop_2023, by = c("Local", "Faixa_etaria", "Raça/cor")) %>% 
  dplyr::select(munResUf, Regiao, raca_cor, ESTCIV, ESC_GRUPO, LOCOCOR, Faixa_etaria,
         causa_categoria, contagem, populacao_prop) %>% 
  dplyr::mutate(
    Regiao = as.factor(Regiao),
    raca_cor = as.factor(raca_cor),
    Faixa_etaria = as.factor(Faixa_etaria),
    ESTCIV = as.factor(ESTCIV),
    ESC_GRUPO = as.factor(ESC_GRUPO),
    LOCOCOR = as.factor(LOCOCOR),
    causa_categoria = as.factor(causa_categoria)
  ) %>%
  droplevels()

#write_csv2(modelo_2023, "dados_modelo2023.csv")

# ---------------------------------------------------------------------------- #
# POISSON
modelo_pois_2023 <- glm(
  contagem ~ Regiao + raca_cor + ESTCIV + ESC_GRUPO + LOCOCOR + 
    Faixa_etaria +  causa_categoria + offset(log(populacao_prop)),
  family = poisson(link = "log"),
  data = modelo_2023); summary(modelo_pois_2023)

fit.model <- modelo_pois_2023
source("rdocs/Envel_pois.R")
source("rdocs/Diag_pois.R")

library(AER)
AER::dispersiontest(modelo_pois_2023)

library(DHARMa)
sim_res <- simulateResiduals(modelo_pois_2023)
plot(sim_res)


# ---------------------------------------------------------------------------- # 
# BINOMIAL NEGATIVO
modelo_bn_2023 <- MASS::glm.nb(
  contagem ~ munResUf + 
    raca_cor +
    ESTCIV + ESC_GRUPO + LOCOCOR + 
    Faixa_etaria +  causa_categoria +
    offset(log(populacao_prop)),
  data = modelo_2023); summary(modelo_bn_2023)
# 
# library(DHARMa)
# 
# sim <- simulateResiduals(fittedModel = modelo_bn_2023, n = 500)
# plot(sim)


fit.model <- modelo_bn_2023
source("rdocs/Envel_Bn.R")
source("rdocs/Diag_Bn.R")
