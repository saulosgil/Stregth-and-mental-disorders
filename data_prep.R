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

# Ajustando a base para análise ---------------------------------------------------------------
# Pegando valores máximo - HGS, TUG, TS -------------------------------------------------------
df_paper_ajustada <-
  df_paper |>
  mutate(hgs_max = apply(df_paper[,13:15], MARGIN = 1,FUN = max),
         tug_max = apply(df_paper[,16:18], MARGIN = 1,FUN = max),
         ts_max = apply(df_paper[,19:21], MARGIN = 1,FUN = max),
# escore de ansiedade
         ansiedade_score = apply(df_paper[,22:42],MARGIN = 1,FUN = sum),
# escore de depressão
         depressao_score = apply(df_paper[,43:63],MARGIN = 1,FUN = sum),
         ) |>
# ajuste devido o escore estar vindo de 1 a 4 e não 0 a 3 como deveria!
  mutate(ansiedade_score = ansiedade_score - 21) |>
# retirando as colunas que não iremos usar
  select(-hgs_tent_1,
         -hgs_tent_2,
         -hgs_tent_3,
         -tug_tent_1,
         -tug_tent_2,
         -tug_tent_3,
         -ts_tent_1,
         -ts_tent_2,
         -ts_tent_3,
         -starts_with("ansiedade_beck"),
         -starts_with("depressao_beck"),
         -who_qol1,
         -medicamentos) |>
# ajustando as questões do WHOQoL
  mutate(who_qol4 = case_when(who_qol4 == 1 ~ 5,
                              who_qol4 == 2 ~ 4,
                              who_qol4 == 3 ~ 3,
                              who_qol4 == 4 ~ 2,
                              who_qol4 == 5 ~ 1),
         who_qol5 = case_when(who_qol5 == 1 ~ 5,
                              who_qol5 == 2 ~ 4,
                              who_qol5 == 3 ~ 3,
                              who_qol5 == 4 ~ 2,
                              who_qol5 == 5 ~ 1),
         who_qol27 = case_when(who_qol27 == 1 ~ 5,
                               who_qol27 == 2 ~ 4,
                               who_qol27 == 3 ~ 3,
                               who_qol27 == 4 ~ 2,
                               who_qol27 == 5 ~ 1))

# Calculando o WHOQoL -------------------------------------------------------------------------
# físico
whoqol_fisico <-
  df_paper_ajustada |>
  select(who_qol4,
         who_qol5,
         who_qol11,
         who_qol16,
         who_qol7,
         who_qol18,
         who_qol19)

whoqol_fisico <-
  whoqol_fisico |>
  mutate(whoqol_fisico = apply(whoqol_fisico[,1:6],MARGIN = 1,FUN = sum)/7) |>
  select(whoqol_fisico)

# psicologico
whoqol_psicol <-
  df_paper_ajustada |>
  select(who_qol6,
         who_qol7,
         who_qol8,
         who_qol12,
         who_qol20,
         who_qol27)

whoqol_psicol <-
  whoqol_psicol |>
  mutate(whoqol_psicol = apply(whoqol_psicol[,1:5],MARGIN = 1,FUN = sum)/6) |>
  select(whoqol_psicol)

# Relações sociais
whoqol_social <-
  df_paper_ajustada |>
  select(who_qol21,
         who_qol22,
         who_qol23)

whoqol_social <-
  whoqol_social |>
  mutate(whoqol_social = apply(whoqol_social[,1:3],MARGIN = 1,FUN = sum)/3) |>
  select(whoqol_social)

# Meio ambiente
whoqol_ambiente <-
  df_paper_ajustada |>
  select(who_qol9,
         who_qol10,
         who_qol13,
         who_qol14,
         who_qol15,
         who_qol24,
         who_qol25,
         who_qol26)

whoqol_ambiente <-
  whoqol_ambiente |>
  mutate(whoqol_ambiente = apply(whoqol_ambiente[,1:3],MARGIN = 1,FUN = sum)/8) |>
  select(whoqol_ambiente)

# Juntando os dominios do WHOQol com a base ---------------------------------------------------
#juntado os dominios
fis_psi <- bind_cols(whoqol_fisico, whoqol_psicol)
fis_psi_soc <- bind_cols(fis_psi,whoqol_social)
todos_dominios <- bind_cols(fis_psi_soc,whoqol_ambiente)

# Juntando os dominios do WHOQoL com a base e removendo as colunas isoladas
df_paper_ajustada_final <- bind_cols(df_paper_ajustada, todos_dominios)

df_paper_ajustada_final <-
  df_paper_ajustada_final |>
  rename(who_percep_saude = who_qol2,
         who_satisf_saude = who_qol3) |>
  select(-starts_with("who_qol"),
         -data_nasc) |>
    relocate(idade, .after = nome) |>
    relocate(estatura, .after = peso) |>
    relocate(hgs_max, .after = comorbidades) |>
    relocate(tug_max, .after = hgs_max) |>
    relocate(ts_max, .after = tug_max) |>
    relocate(ansiedade_score, .after = ts_max) |>
    relocate(depressao_score, .after = ansiedade_score) |>
    mutate(percentual_gordura = case_when(percentual_gordura > 1000 ~ 26.3,
                                          TRUE ~ percentual_gordura)
)

# Tratando os missing -------------------------------------------------------------------------
# Verifricando os missing
mice::md.pattern(df_paper_ajustada_final)

# Imputação using pmm
tempData <- mice::mice(df_paper_ajustada_final,m=5,maxit=50,meth='pmm',seed=500)

# Base imputada
df <- complete(tempData,1)
mice::md.pattern(df)
glimpse(df)

# tabela para analise -------------------------------------------------------------------------
write_rds(x = df,file =  "df_para_analise.rds")
