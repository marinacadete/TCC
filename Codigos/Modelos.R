source("rdocs/Trat_Dados.R")

# ============================================================================ #
#                   MODELANDO A OCORRENCIA DE HOMICÍDIO                        # 
# ============================================================================ # ----
contagem_uf_2022 <- sim %>%
  filter(Ano==2022) %>% 
  group_by(munResUf, RACACOR, ESTCIV, ESC_GRUPO, LOCOCOR, fe_quinque) %>%
  summarise(contagem = n()) %>% 
    mutate(
      munResUf = as.factor(munResUf),
      RACACOR = as.factor(RACACOR),
      ESTCIV = as.factor(ESTCIV),
      ESC_GRUPO = as.factor(ESC_GRUPO),
      LOCOCOR = as.factor(LOCOCOR),
      faixa_etaria = as.factor(fe_quinque)
    )

contagem_Regiao_2022 <- sim %>%
  filter(Ano==2022) %>% 
  group_by(Regiao, RACACOR, ESTCIV, ESC_GRUPO, LOCOCOR, faixa_etaria) %>%
  summarise(contagem = n()) %>% 
  mutate(
    Regiao = as.factor(Regiao),
    RACACOR = as.factor(RACACOR),
    ESTCIV = as.factor(ESTCIV),
    ESC_GRUPO = as.factor(ESC_GRUPO),
    LOCOCOR = as.factor(LOCOCOR),
    faixa_etaria = as.factor(faixa_etaria)
  )

contagem_uf_2023 <- sim %>%
  filter(Ano==2023) %>% 
  group_by(munResUf, RACACOR, ESTCIV, ESC_GRUPO, LOCOCOR, faixa_etaria) %>%
  summarise(contagem = n()) %>% 
  mutate(
    munResUf = as.factor(munResUf),
    RACACOR = as.factor(RACACOR),
    ESTCIV = as.factor(ESTCIV),
    ESC_GRUPO = as.factor(ESC_GRUPO),
    LOCOCOR = as.factor(LOCOCOR),
    faixa_etaria = as.factor(faixa_etaria)
  )

contagem_Regiao_2023 <- sim %>%
  filter(Ano==2023) %>% 
  group_by(Regiao, RACACOR, ESTCIV, ESC_GRUPO, LOCOCOR, faixa_etaria) %>%
  summarise(contagem = n()) %>% 
  mutate(
    Regiao = as.factor(Regiao),
    RACACOR = as.factor(RACACOR),
    ESTCIV = as.factor(ESTCIV),
    ESC_GRUPO = as.factor(ESC_GRUPO),
    LOCOCOR = as.factor(LOCOCOR),
    faixa_etaria = factor(faixa_etaria, ordered = FALSE)
  )

# ---------------------------------------------------------------------------- #
# POISSON
modelo_pois_uf_22 <- glm(
  contagem ~ munResUf + RACACOR + ESTCIV + ESC_GRUPO + LOCOCOR + faixa_etaria,
  family = poisson(link = "log"),
  data = contagem_uf_2022
); summary(modelo_pois_uf_22)

modelo_pois_reg_22 <- glm(
  contagem ~ Regiao + RACACOR + ESTCIV + ESC_GRUPO + LOCOCOR + faixa_etaria,
  family = poisson(link = "log"),
  data = contagem_Regiao_2022
); summary(modelo_pois_reg_22)

fit.model <- modelo_pois_reg_22; attach(sim)
source("rdocs/Envel_pois.R")
source("rdocs/Diag_pois.R")


modelo_pois_uf_23 <- glm(
  contagem ~ munResUf + RACACOR + ESTCIV + ESC_GRUPO + LOCOCOR + faixa_etaria,
  family = poisson(link = "log"),
  data = contagem_uf_2023
); summary(modelo_pois_uf_23)

modelo_pois_reg_23 <- glm(
  contagem ~ Regiao + RACACOR + ESTCIV + ESC_GRUPO + LOCOCOR + faixa_etaria,
  family = poisson(link = "log"),
  data = contagem_Regiao_2023
); summary(modelo_pois_reg_23)

fit.model <- modelo_pois_uf_23
source("rdocs/Envel_pois.R")
source("rdocs/Diag_pois.R")


#disp_uf_2022 <- sum(residuals(modelo_pois_uf_22, type = "pearson")^2) / modelo_pois_uf_22$df.residual
disp_reg_2022 <- sum(residuals(modelo_pois_reg_22, type = "pearson")^2) / modelo_pois_reg_22$df.residual 
#disp_uf_2023 <- sum(residuals(modelo_pois_uf_23, type = "pearson")^2) / modelo_pois_uf_23$df.residual
disp_reg_2023 <- sum(residuals(modelo_pois_reg_23, type = "pearson")^2) / modelo_pois_reg_23$df.residual

disp_reg_2022
disp_reg_2023

# ---------------------------------------------------------------------------- # 
# BINOMIAL NEGATIVO
modelo_bn_reg2022 <- glm.nb(
  contagem ~ Regiao + RACACOR + ESTCIV + ESC_GRUPO + LOCOCOR + faixa_etaria,
  data = contagem_Regiao_2022
); summary(modelo_bn_reg2022)

fit.model <- modelo_bn_reg2022
source("rdocs/Envel_pois.R")
source("rdocs/Diag_Bn.R")

modelo_bn_reg2023 <- glm.nb(
  contagem ~ Regiao + RACACOR + ESTCIV + ESC_GRUPO + LOCOCOR + faixa_etaria,
  data = contagem_Regiao_2023
); summary(modelo_bn_reg2023)

fit.model <- modelo_bn_reg2023
source("rdocs/Envel_pois.R")
source("rdocs/Diag_Bn.R")


# ---------------------------------------------------------------------------- # ----
# ============================================================================ #
#                 MODELANDO O LOG DA OCORRÊNCIA DE HOMICÍDIO                   #
# ============================================================================ # ----
proj_uf_2023 <- dados_projecao %>%
  filter(!LOCAL %in% c("Centro-Oeste", "Nordeste", "Norte", 
                       "Sudeste", "Sul", "Brasil"),
         ano == 2023) %>%
  mutate(ano = as.integer(ano),
         pop = as.numeric(pop)) %>%
  group_by(munResUf = LOCAL, ano, faixa_etaria) %>%
  summarise(pop_fem = sum(pop, na.rm = TRUE), .groups = "drop")

prop_uf_2022 <- prop_RacaCor %>%
  filter(!Local %in% c("Centro-Oeste", "Nordeste", "Norte", 
                       "Sudeste", "Sul", "Brasil"))

log_uf_2023 <- sim %>%
  filter(Ano == 2023) %>% 
  left_join(proj_uf_2023, by = "munResUf") %>% 
  group_by(munResUf, RACACOR, ESTCIV, ESC_GRUPO, LOCOCOR, faixa_etaria, pop_fem) %>%
  summarise(contagem = n()) %>% 
  mutate(
    munResUf = as.factor(munResUf),
    RACACOR = as.factor(RACACOR),
    ESTCIV = as.factor(ESTCIV),
    ESC_GRUPO = as.factor(ESC_GRUPO),
    LOCOCOR = as.factor(LOCOCOR),
    faixa_etaria = as.factor(faixa_etaria)
  ) %>% 
  mutate(
    munResUf = relevel(munResUf, ref = "São Paulo"),
    RACACOR = relevel(RACACOR, ref = "Branca"),
    ESTCIV = relevel(ESTCIV, ref = "Casado"),
    LOCOCOR = relevel(LOCOCOR, ref = "Domicílio"),
    faixa_etaria = relevel(faixa_etaria, ref = "<15")
  )

# ---------------------------------------------------------------------------- #
# POISSON
modelo_pois_taxa_uf_2023 <- glm(
  contagem ~ munResUf + RACACOR + ESTCIV + ESC_GRUPO + LOCOCOR + faixa_etaria +
    offset(log(pop_fem)),
  family = poisson(link = "log"),
  data = log_uf_2023); summary(modelo_pois_taxa_uf_2023)

fit.model <- modelo_pois_taxa_uf_2023; attach(sim)
source("rdocs/Envel_pois.R")
source("rdocs/Diag_pois.R")


disp_reg_2023 <- sum(residuals(
  modelo_pois_taxa_uf_2023, type = "pearson")^2) / 
  modelo_pois_taxa_uf_2023$df.residual; disp_reg_2023

# ---------------------------------------------------------------------------- # 
# BINOMIAL NEGATIVO
modelo_bn_taxa_uf_2023 <- glm.nb(
  contagem ~ munResUf + RACACOR + ESTCIV + ESC_GRUPO + LOCOCOR + faixa_etaria +
    offset(log(pop_fem)),
  data = log_uf_2023
); summary(modelo_bn_taxa_uf_2023)

fit.model <- modelo_bn_taxa_uf_2023
source("rdocs/Envel_Bn.R")
source("rdocs/Diag_Bn.R")

# ---------------------------------------------------------------------------- # ----
# ============================================================================ #
#                     MODELANDO A TAXA DE MORTALIDADE                          #
# ============================================================================ # ----