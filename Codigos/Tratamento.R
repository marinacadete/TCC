source("Codigos/source/packages.R")

# ============================================================================ #
#                       Tratamento dos Dados
# ============================================================================ #

dados_completos <- read_delim(
  "Dados/DadosSIM_Homicidios_Femininos.csv",
  delim = ";", escape_double = FALSE, 
  col_types = cols(...1 = col_skip(),
                   HORAOBITO = col_time(format = "%H%M")),
  trim_ws = TRUE) %>% 
  filter(RACACOR != "Indígena") %>% 
  mutate(
    # Extrai o padrão de LETRA + 2 DÍGITOS
    codigo = str_extract(str_to_upper(str_squish(CAUSABAS_O)), "[A-Z]\\d{2}"),
    causa_categoria = case_when(
      codigo %in% c("X93", "X94", "X95") ~ "Lesão por arma de fogo",
      codigo %in% c("X99", "Y00") ~ "Lesão por instrumento perfurante, cortante ou contundente",
      codigo == "X91" ~ "Lesão por enforcamento",
      str_detect(codigo, "^Y0[4-7]$") ~ "Lesão por maus tratos",
      TRUE ~ "Outros"
    )
  )

dados_projecao <- read_excel("Dados/projecoes_2024_tab1_idade_simple.xlsx") %>% 
  pivot_longer(
    cols = matches("^20\\d{2}$"),
    names_to = "ano",
    values_to = "pop"
  ) %>% 
  filter(IDADE >= 15, IDADE <= 64) %>%
  mutate(
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
      IDADE >= 60 & IDADE <= 64 ~ "60 a 64 anos",
      TRUE ~ NA_character_
    ),
    Faixa_etaria = factor(
      faixa_etaria,
      levels = c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos",
                 "35 a 39 anos", "40 a 44 anos", "45 a 49 anos", "50 a 54 anos", 
                 "55 a 59 anos", "60 a 64 anos"),
      ordered = TRUE
    )
  )

prop_RacaCor <- read_excel(
  "Dados/Total 9606.xlsx",skip = 6,
  col_names = c("Local", "Faixa_etaria", "Total", "Branca", 
                "Preta","Amarela", "Parda", "Indigena")) %>% 
  fill(Local) %>%
  mutate(
    Branca = Branca + Amarela,
    Negra = Preta + Parda) %>%
  dplyr::select(Local, Faixa_etaria, Total, Branca, Negra) %>% 
  mutate(
    prop_Branca   = Branca   / Total,
    prop_Negra    = Negra    / Total
  ) %>%
  mutate(across(starts_with("prop_"), ~ round(.x * 100, 2), .names = "pct_{.col}")) %>% 
  dplyr::select(Local, Faixa_etaria, prop_Branca, prop_Negra) %>% 
  pivot_longer(prop_Branca:prop_Negra, names_to = "Raça/cor", values_to = "Proporcao") %>% 
  filter(!Faixa_etaria == "14 anos") %>% 
  mutate(`Raça/cor` = sub("^prop_", "", `Raça/cor`))


regioes <- c(
  "Acre" = "Norte", "Amapá" = "Norte", "Amazonas" = "Norte","Pará" = "Norte", 
  "Rondônia" = "Norte", "Roraima" = "Norte", "Tocantins" = "Norte", "Alagoas" = "Nordeste", 
  "Bahia" = "Nordeste", "Ceará" = "Nordeste","Maranhão" = "Nordeste", "Paraíba" = "Nordeste", 
  "Pernambuco" = "Nordeste", "Piauí" = "Nordeste", "Rio Grande do Norte" = "Nordeste", 
  "Sergipe" = "Nordeste", "Distrito Federal" = "Centro-Oeste", "Goiás" = "Centro-Oeste",
  "Mato Grosso" = "Centro-Oeste", "Mato Grosso do Sul" = "Centro-Oeste", "Espírito Santo" = "Sudeste", 
  "Minas Gerais" = "Sudeste", "Rio de Janeiro" = "Sudeste", "São Paulo" = "Sudeste",
  "Paraná" = "Sul", "Rio Grande do Sul" = "Sul", "Santa Catarina" = "Sul"
)

ufs_codigos <- data.frame(
  munResUf = c("Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará",
               "Distrito Federal", "Espírito Santo", "Goiás", "Maranhão",
               "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Pará",
               "Paraíba", "Paraná", "Pernambuco", "Piauí", "Rio Grande do Norte",
               "Rio Grande do Sul", "Rio de Janeiro", "Rondônia", "Roraima",
               "Santa Catarina", "São Paulo", "Sergipe", "Tocantins"),
  codUF = c(12, 27, 16, 13, 29, 23, 53, 32, 52, 21, 51, 50, 31, 15,
            25, 41, 26, 22, 24, 43, 33, 11, 14, 42, 35, 28, 17)
)

sim <- dados_completos %>%
  dplyr::select(-c(SEXO, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII)) %>% 
  filter(IDADEanos != 14) %>% 
  mutate(
    Ano = year(DTOBITO),
    Regiao = recode(munResUf, !!!regioes, .default = "Outros"),
    fe_quinque = case_when(
      IDADEanos >= 15 & IDADEanos <= 19 ~ "15 a 19 anos",
      IDADEanos >= 20 & IDADEanos <= 24 ~ "20 a 24 anos",
      IDADEanos >= 25 & IDADEanos <= 29 ~ "25 a 29 anos",
      IDADEanos >= 30 & IDADEanos <= 34 ~ "30 a 34 anos",
      IDADEanos >= 35 & IDADEanos <= 39 ~ "35 a 39 anos",
      IDADEanos >= 40 & IDADEanos <= 44 ~ "40 a 44 anos",
      IDADEanos >= 45 & IDADEanos <= 49 ~ "45 a 49 anos",
      IDADEanos >= 50 & IDADEanos <= 54 ~ "50 a 54 anos",
      IDADEanos >= 55 & IDADEanos <= 59 ~ "55 a 59 anos",
      IDADEanos >= 60 & IDADEanos <= 64 ~ "60 a 64 anos",
      TRUE ~ NA_character_),
    fe_quinque = factor(
      fe_quinque,
      levels = c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos",
                 "35 a 39 anos", "40 a 44 anos", "45 a 49 anos", "50 a 54 anos", 
                 "55 a 59 anos", "60 a 64 anos"),
      ordered = TRUE
    ),
    ESC_GRUPO = case_when(
      ESC %in% c("Nenhuma", "1 a 3 anos", "4 a 7 anos") ~ "Menos de 8 anos de Estudo",
      ESC %in% c("8 a 11 anos", "12 anos ou mais") ~ "Mais de 8 anos de estudo",
      TRUE ~ NA_character_),
    ESC_GRUPO = factor(ESC_GRUPO, levels = c("Menos de 8 anos de Estudo", 
                                             "Mais de 8 anos de estudo")
    ),
    hora_decimal = if_else(!is.na(HORAOBITO),
                           (as.integer(hms::as_hms(HORAOBITO)) %/% 3600),
                           NA_integer_),
    Periodo_Obito = case_when(
      hora_decimal >= 0  & hora_decimal < 6  ~ "Madrugada", # 00–05
      hora_decimal >= 6  & hora_decimal < 12 ~ "Manhã",     # 06–11
      hora_decimal >= 12 & hora_decimal < 18 ~ "Tarde",     # 12–17
      hora_decimal >= 18 & hora_decimal <= 23 ~ "Noite",    # 18–23
      TRUE ~ NA_character_
    ),
    Periodo_Obito = factor(Periodo_Obito,
                           levels = c("Madrugada","Manhã","Tarde","Noite"),
                           ordered = TRUE),
    raca_cor = case_when(
      RACACOR %in% c("Preta", "Parda") ~ "Negra",
      RACACOR %in% c("Branca", "Amarela") ~ "Branca",
      TRUE ~ NA_character_
    )) %>% 
  filter(!is.na(raca_cor)) %>%
  left_join(ufs_codigos, by = "munResUf")

