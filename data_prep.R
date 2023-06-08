# Pacotes -------------------------------------------------------------------------------------
library(tidyverse)

# Lendo a base para ajustes -------------------------------------------------------------------
df_renomeado <- readr::read_rds("database_08062023.rds")

# Excluindo os duplicados para analise transversal --------------------------------------------
# Ajustando o nome das variaveis

df_renomeado <- janitor::clean_names(df_renomeado)

# Pegando somente quem tem racimed e excluindo que tem um segundo registro (reavaliação)
com_racimed <-
  df_renomeado |>
  filter(!is.na(racimed)) |>
  distinct(racimed,.keep_all = TRUE)

# Pegando quem não tem racimed
racimed_na <-
  df_renomeado |>
  filter(is.na(racimed))

# Juntando as bases
df <-
  bind_rows(com_racimed,
            racimed_na)

# Selecionando as variaveis que iremos usar ---------------------------------------------------
df_paper <-
  df |>
  select(
    nome,
    data_nasc,
    estado_civil,
    genero,
    raca,
    renda,
    peso,
    percentual_gordura,
    percentual_mm,
    estatura,
    comorbidades,
    medicamentos,
    handgrip_direita,
    tug,
    ts,
    starts_with("ansiedade"),
    starts_with("depressao"),
    starts_with("who_qol")
  )

# Variavel idade > 18 anos --------------------------------------------------------------------
df_paper <-
  df_paper |>
  mutate(data_nasc =  unlist(data_nasc),
         data_nasc = as.Date(data_nasc, format="%d/%m/%Y"),
         idade = 2023 - lubridate::year(data_nasc)) |>
  filter(idade > 18)

# Ajustando as variaveis demograficas e antropométricas ---------------------------------------
# peso, percentual de gordura, massa magra e estatura
df_paper <-
  df_paper |>
  mutate(peso = as.numeric(stringr::str_sub(peso, start = 1,end = -1)),
         percentual_gordura = stringr::str_sub(percentual_gordura, start = 1,end = -1),
         percentual_mm = stringr::str_sub(percentual_mm,start = 1,end = -1),
         estatura = stringr::str_sub(estatura,start = 1,end = -1)) |>
  # convertendo percentual de gordura, massa magra e estatura para numerico
  mutate(percentual_gordura = as.numeric(percentual_gordura),
         percentual_mm = as.numeric(percentual_mm),
         estatura = as.numeric(estatura)) |>
  # separando as tentativas do HGS
  tidyr::separate(col = handgrip_direita,
                  c("hgs_tent_1", "hgs_tent_2", "hgs_tent_3"),
                  sep = "/",
                  convert = TRUE) |>
  # convertendo o HGS para numerico
  mutate(hgs_tent_1 = as.numeric(hgs_tent_1),
         hgs_tent_2 = as.numeric(hgs_tent_2),
         hgs_tent_3 = as.numeric(hgs_tent_3)) |>
  # separando as tentativas do TUG
  tidyr::separate(col = tug,
                  c("tug_tent_1", "tug_tent_2", "tug_tent_3"),
                  sep = "/",
                  convert = TRUE) |>
  # convertendo o TUG para numerico
  mutate(tug_tent_1 = as.numeric(tug_tent_1),
         tug_tent_2 = as.numeric(tug_tent_2),
         tug_tent_3 = as.numeric(tug_tent_3)) |>
  # separando as tentativas do TS
  tidyr::separate(col = ts,
                  c("ts_tent_1", "ts_tent_2", "ts_tent_3"),
                  sep = "/",
                  convert = TRUE) |>
  # convertendo o TS para numerico
  mutate(ts_tent_1 = as.numeric(ts_tent_1),
         ts_tent_2 = as.numeric(ts_tent_2),
         ts_tent_3 = as.numeric(ts_tent_3)) |>
  # convertendo as colunas que vieram como lista para numericas
  mutate(who_qol1 = as.numeric(unlist(who_qol1)),
         who_qol3 = as.numeric(unlist(who_qol3)),
         who_qol8 = as.numeric(unlist(who_qol8)),
         who_qol13 = as.numeric(unlist(who_qol13))
  )

# verificando a base
glimpse(df_paper)

