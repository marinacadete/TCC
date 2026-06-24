# ============================================================================ #
#                       Tratamento dos Dados
# ============================================================================ #

source("Codigos/source/packages.R")

# ---- Leitura dos Dados -------------------------------------------------------

dados_sim_completos <- read_delim(
  "Dados/DadosSIM_Homicidios_Femininos.csv",
  delim = ";", escape_double = FALSE, 
  col_types = cols(...1 = col_skip(),
                   DTOBITO = col_date(format = "%d/%m/%Y")), 
  # HORAOBITO = col_time(format = "%H%M")),
  trim_ws = TRUE)

dados_projecao <- read_excel("Dados/projecoes_2024_tab1_idade_simple.xlsx") %>%
  pivot_longer(cols = matches("^20\\d{2}$"), names_to = "ano", values_to = "pop") %>%
  filter(dplyr::between(IDADE, 15, 64)) %>%
  mutate(
    ano = as.integer(ano), 
    pop = as.numeric(pop),
    
    faixa_etaria = case_when(
      IDADE >= 15 & IDADE <= 19 ~ "15 a 19 anos",
      IDADE >= 20 & IDADE <= 24 ~ "20 a 24 anos",
      IDADE >= 25 & IDADE <= 29 ~ "25 a 29 anos",
      IDADE >= 30 & IDADE <= 34 ~ "30 a 34 anos",
      IDADE >= 35 & IDADE <= 39 ~ "35 a 39 anos",
      IDADE >= 40 & IDADE <= 44 ~ "40 a 44 anos",
      IDADE >= 45 & IDADE <= 49 ~ "45 a 49 anos",
      IDADE >= 50 & IDADE <= 54 ~ "50 a 54 anos",
      IDADE >= 55 & IDADE <= 59 ~ "55 a 59 anos",
      IDADE >= 60 & IDADE <= 64 ~ "60 a 64 anos"
    ),
    faixa_etaria = factor(faixa_etaria, levels = c(
      "15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos",
      "35 a 39 anos", "40 a 44 anos", "45 a 49 anos", "50 a 54 anos",
      "55 a 59 anos", "60 a 64 anos"
    ), ordered = TRUE)
  )

prop_RacaCor <- read_excel(
  "Dados/populacao_residente_por_RacaCor_2022.xlsx",
  skip = 6,
  col_names = c("Local", "Faixa_etaria", "Total", "Branca", 
                "Preta", "Amarela", "Parda", "Indigena")
  ) %>%
  fill(Local) %>%
  filter(Faixa_etaria != "14 anos") %>%
  transmute(
    Local,
    Faixa_etaria,
    prop_Branca = (Branca + Amarela) / Total,
    prop_Negra  = (Preta  + Parda)   / Total
  ) %>%
  pivot_longer(prop_Branca:prop_Negra, names_to = "Raça/cor", values_to = "Proporcao") %>%
  mutate(`Raça/cor` = sub("^prop_", "", `Raça/cor`))

# ---- Tabelas auxiliares ------------------------------------------------------

niveis_fe <- c(
  "15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos",
  "35 a 39 anos", "40 a 44 anos", "45 a 49 anos", "50 a 54 anos",
  "55 a 59 anos", "60 a 64 anos"
)

colunas_excluir <- c(
  "CODMUNNATU", "CODMUNOCOR", "DTNASC", "SEXO", "ASSISTMED", "NECROPSIA",
  "LINHAA", "LINHAB", "LINHAC", "LINHAD", "LINHAII","DTINVESTIG", "NATURAL", 
  "OCUP", "CAUSABAS_O"
)

# ---- Tratamento --------------------------------------------------------------

sim <- dados_sim_completos %>%
  select(-all_of(colunas_excluir)) %>%
  filter(!is.na(RACACOR), RACACOR != "Indígena", IDADEanos != 14) %>%
  mutate(
    
    Ano = year(DTOBITO),
    
    hora_decimal = if_else(
      !is.na(HORAOBITO),
      as.integer(hms::as_hms(HORAOBITO)) %/% 3600L,
      NA_integer_
    ),
    
    Periodo_Obito = factor(case_when(
      hora_decimal <  6                          ~ "Madrugada",
      hora_decimal < 12                          ~ "Manhã",
      hora_decimal < 18                          ~ "Tarde",
      hora_decimal >= 18 & hora_decimal <= 23    ~ "Noite"
    ), levels = c("Madrugada", "Manhã", "Tarde", "Noite"), ordered = TRUE
    ),
    
    Regiao = recode(
      str_sub(as.character(CODMUNRES), 1, 1),
      "1" = "Norte",
      "2" = "Nordeste",
      "3" = "Sudeste",
      "4" = "Sul",
      "5" = "Centro-Oeste",
      .default = "Não identificado"
    ),
    
    codUF = str_sub(as.character(CODMUNRES), 1, 2
    ),
    
    fe_quinque = factor(case_when(
      IDADEanos >= 15 & IDADEanos <= 19 ~ "15 a 19 anos",
      IDADEanos >= 20 & IDADEanos <= 24 ~ "20 a 24 anos",
      IDADEanos >= 25 & IDADEanos <= 29 ~ "25 a 29 anos",
      IDADEanos >= 30 & IDADEanos <= 34 ~ "30 a 34 anos",
      IDADEanos >= 35 & IDADEanos <= 39 ~ "35 a 39 anos",
      IDADEanos >= 40 & IDADEanos <= 44 ~ "40 a 44 anos",
      IDADEanos >= 45 & IDADEanos <= 49 ~ "45 a 49 anos",
      IDADEanos >= 50 & IDADEanos <= 54 ~ "50 a 54 anos",
      IDADEanos >= 55 & IDADEanos <= 59 ~ "55 a 59 anos",
      IDADEanos >= 60 & IDADEanos <= 64 ~ "60 a 64 anos"
    ), levels = niveis_fe, ordered = TRUE
    ),
    
    fe_resumida = factor(case_when(
      IDADEanos >= 15 & IDADEanos <= 29 ~ "15 a 29 anos",
      IDADEanos >= 30 & IDADEanos <= 44 ~ "30 a 44 anos",
      IDADEanos >= 45 & IDADEanos <= 64 ~ "45 a 64 anos"
    ), levels = c("15 a 29 anos", "30 a 44 anos", "45 a 64 anos"), ordered = TRUE
    ),
    
    raca_cor = case_when(
      RACACOR %in% c("Preta", "Parda")    ~ "Negra",
      RACACOR %in% c("Branca", "Amarela") ~ "Branca"
    ),
    
    estciv_grupo = case_when(
      ESTCIV %in% c("Casado", "União consensual")                  ~ "Com companheiro",
      ESTCIV %in% c("Solteiro", "Viúvo", "Separado judicialmente") ~ "Sem companheiro",
      .default = "Não informado"
    ),
    
    ESC_GRUPO = factor(case_when(
      ESC %in% c("Nenhuma", "1 a 3 anos", "4 a 7 anos") ~ "Menos de 8 anos de Estudo",
      ESC %in% c("8 a 11 anos", "12 anos ou mais")      ~ "8 anos ou mais de Estudo",
      .default = "Não informado"
    ), levels = c("Menos de 8 anos de Estudo", "8 anos ou mais de Estudo", "Não informado")),
    
    codigo = str_extract(str_to_upper(str_squish(CAUSABAS)), "[A-Z]\\d{2}"),
    
    causa_categoria = case_when(
      codigo %in% c("X93", "X94", "X95") ~ "Lesão por arma de fogo",
      codigo %in% c("X99", "Y00")        ~ "Lesão por instrumento perfurante, cortante ou contundente",
      codigo == "X91"                    ~ "Lesão por enforcamento",
      str_detect(codigo, "^Y0[4-7]$")    ~ "Lesão por maus tratos",
      TRUE                               ~ "Outros"
    ),
    lococor_grupo = case_when(
      LOCOCOR == "Domicílio"                                        ~ "Domicílio",
      LOCOCOR == "Via pública"                                      ~ "Via pública",
      LOCOCOR %in% c("Hospital", "Outro estabelecimento de saúde")  ~ "Estabelecimento de saúde",
      LOCOCOR == "Outros"                                           ~ "Outros",
      .default = "Não Informado"
    ))

