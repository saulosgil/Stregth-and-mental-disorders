# pacotes -------------------------------------------------------------------------------------
library(tidyverse)

# ler a base ----------------------------------------------------------------------------------
df <- readRDS("df_para_analise.rds")
glimpse(df)

# Criando os grupos pela mediana de acordo com o HGS -------------------------------------------------------
df <- df |>
  arrange(hgs_max)

low_strength <-
  df |>
  slice(1:108) |>
  mutate(hgs_class = "B_low_strength")

hig_strength <-
  df |>
  slice(109:216) |>
  mutate(hgs_class = "A_hig_strength")

df <- bind_rows(low_strength,hig_strength)

# Criando os grupos pela mediana de acordo com o TUG -------------------------------------------------------
df <- df |>
  arrange(tug_max)

low_tug <-
  df |>
  slice(1:108) |>
  mutate(tug_class = "A_hig_tug")

hig_tug <-
  df |>
  slice(109:216) |>
  mutate(tug_class = "B_low_tug")


df <- bind_rows(low_tug,hig_tug)

# Criando os tercis de acordo com o TS --------------------------------------------------------
df <- df |>
  arrange(ts_max)

low_ts <-
  df |>
  slice(1:108) |>
  mutate(ts_class = "B_low_ts")

hig_ts <-
  df |>
  slice(109:216) |>
  mutate(ts_class = "A_hig_ts")

df <- bind_rows(low_ts,hig_ts)

# Criando as classes para ansiedade, depressão, idade, bmi -----------------------------------
df <-
  df |>
  mutate(ansiedade_class = if_else(ansiedade_score > 25, 1,0),
         depressao_class = if_else(depressao_score > 28, 1,0),
         idade_class = if_else(idade < 65, "adulto", "idoso"),
         bmi = peso/((estatura/100)^2),
         bmi_class = if_else(bmi < 30, "non-obese", "obese",missing = "non-obese")
         )

# realocando as colunas -----------------------------------------------------------------------
df <-
  df|>
  relocate(hgs_class,.after = hgs_max) |>
  relocate(tug_class,.after = tug_max) |>
  relocate(ts_class,.after = ts_max) |>
  relocate(ansiedade_class,.after = ansiedade_score) |>
  relocate(depressao_class,.after = depressao_score) |>
  relocate(idade_class,.after = idade) |>
  relocate(bmi,.after = estatura) |>
  relocate(bmi_class,.after = bmi) |>
  mutate(raca = as.factor(raca))

glimpse(df)

# Graficos exploratórios ----------------------------------------------------------------------
df |>
  ggplot(mapping = aes(x = hgs_max,y = ansiedade_score)) +
  geom_point()

df |>
  ggplot(mapping = aes(x = tug_max,y = ansiedade_score)) +
  geom_point()

df |>
  ggplot(mapping = aes(x = ts_max,y = ansiedade_score)) +
  geom_point()

df |>
  ggplot(mapping = aes(x = hgs_max,y = depressao_score)) +
  geom_point()

df |>
  ggplot(mapping = aes(x = tug_max,y = depressao_score)) +
  geom_point()

df |>
  ggplot(mapping = aes(x = ts_max,y = depressao_score)) +
  geom_point()

df |>
  group_by(hgs_class) |>
  summarize(media = mean(depressao_score),
            DP = sd(depressao_score)) |>
  ggplot(mapping = aes(x = hgs_class,
                       y = media)) +
  geom_bar(position = position_dodge(),
           stat = "identity",
           colour='black') +
  geom_errorbar(aes(ymin = media,
                    ymax = media + DP),
                width=.2)

# analises exploratórias - test t e regressão -------------------------------------------------
# tirar notação cientifica
options(scipen=999)

# Graficos + testes t para amostras independentes ---------------------------------------------
# Ansiedade
## HGS
t.test(depressao_score ~ ts_class, df)

## TS

## TUG

# Depressao
## HGS
t.test(depressao_score ~ ts_class, df)

## TS

## TUG

# Radar plot + teste t independente de todos os dominios --------------------------------------





# Regressões lineares -------------------------------------------------------------------------

model <- lm(whoqol_fisico ~ hgs_max +
               genero + idade_class + raca + bmi_class, df)

model <- glm(depressao_class ~ hgs_class +
              genero + idade_class + raca + bmi_class, df, family = binomial)

summary(model)


