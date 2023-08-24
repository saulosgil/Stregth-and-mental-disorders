# pacotes -------------------------------------------------------------------------------------
library(tidyverse)
library(patchwork)

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
  mutate(
    ansiedade_class = case_when(
      ansiedade_score <  08 ~ "Minimal",
      ansiedade_score >= 08 & ansiedade_score < 16 ~ "Mild",
      ansiedade_score >= 16 & ansiedade_score < 26 ~ "Moderate",
      ansiedade_score >= 26 ~ "Severe"
    ),
    depressao_class = case_when(
      depressao_score <  14 ~ "Minimal",
      depressao_score >= 14 & depressao_score < 20 ~ "Mild",
      depressao_score >= 20 & depressao_score < 29 ~ "Moderate",
      depressao_score >= 29 ~ "Severe"
    ),
    ansiedade_class_2 = if_else(ansiedade_score < 16, "Minimal/Mild","Moderate/Severe"),
    depressao_class_2 = if_else(depressao_score < 20, "Minimal/Mild","Moderate/Severe"),
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
  relocate(ansiedade_class_2,.after = ansiedade_class) |>
  relocate(depressao_class,.after = ansiedade_class_2) |>
  relocate(depressao_class_2,.after = depressao_class) |>
  relocate(idade_class,.after = idade) |>
  relocate(bmi,.after = estatura) |>
  relocate(bmi_class,.after = bmi) |>
  mutate(raca = as.factor(raca)) |>
  mutate(ansiedade_class = as.factor(ansiedade_class),
         ansiedade_class_2 = as.factor(ansiedade_class_2),
         depressao_class = as.factor(depressao_class),
         depressao_class_2 = as.factor(depressao_class_2))

glimpse(df)
skimr::skim(df)

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

# Grafico de barras ---------------------------------------------------------------------------
df |>
  group_by(hgs_class) |>
  summarize(media = mean(depressao_score),
            DP = sd(depressao_score)) |>
  ggplot(mapping = aes(x = hgs_class,
                       y = media)) +
  geom_bar(position = position_dodge(),
           stat = "identity",
           colour = "black",
           fill = c("black", "white"),
           show.legend = FALSE) +
  geom_errorbar(aes(ymin = media,
                    ymax = media + DP),
                width=.2) +
  theme_classic() +
  ylab(label = "Beck depression inventory score (a.u.)") +
  scale_x_discrete(labels = c("High\nTimed Stands", "Low\nTimed Stands"))+
  theme(
    axis.title.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 25, 5))

# Grafico de barras para mostrar frequencia relativa de sintomas severos de ansiedade  --------
# handgrip ------------------------------------------------------------------------------------
# Ansiedade
hgs_ansiedade_severa <-
  df |>
  group_by(hgs_class, ansiedade_class_2) |>
  summarise(abs_ansiedade_2 = n(),
            perc_ansiedade_2 = n()/(nrow(df)/2)*100)

hgs_ansiedade_severa

# vetor de ordenação

level_order <- c("Minimal/Mild", "Moderate/Severe")
# plot
hgs_ans <-
  df |>
  group_by(hgs_class, ansiedade_class_2) |>
  summarise(abs_ansiedade_2 = n(),
            perc_ansiedade_2 = n()/(nrow(df)/2)*100) |>
  ggplot(mapping = aes(x = ansiedade_class_2,
                       y = perc_ansiedade_2,
                       colour = "black",
                       fill = hgs_class)) +
  geom_col(position = "dodge",
           colour = "black") +
  geom_text(aes(
    label = c("95%", "13%", "86%", "22%")),
    color = "black",
    position = position_dodge(0.9),
    size = 8,
    vjust = -0.5,
    show.legend = FALSE) +
  scale_x_discrete(limits = level_order) +
  scale_y_continuous(limits = c(0, 100, 25)) +
  scale_fill_manual(labels = c("High Strength", "Low Strength"),
                    values = c("black", "white")) +
  xlab(label = "") +
  ylab("relative frequency (%)") +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

hgs_ans

# Depressao
ts_depressao_severa <-
  df |>
  group_by(ts_class, depressao_class_2) |>
  summarise(abs_depressao_2 = n(),
            perc_depressao_2 = n()/(nrow(df)/2)*100)

ts_depressao_severa

# Plot
hgs_dep <-
  df |>
  group_by(hgs_class, depressao_class_2) |>
  summarise(abs_depressao_severo_2 = n(),
            perc_depressao_severo_2 = n()/(nrow(df)/2)*100) |>
  ggplot(mapping = aes(x = depressao_class_2,
                       y = perc_depressao_severo_2,
                       colour = "black",
                       fill = hgs_class)) +
  geom_col(position = "dodge",
           colour = "black") +
  geom_text(aes(
    label = c("95%","13%","92%","16%")),
    color = "black",
    position = position_dodge(0.9),
    size = 8,
    vjust = -0.5,
    show.legend = FALSE) +
  scale_x_discrete(limits = level_order) +
  scale_y_continuous(limits = c(0, 100, 25)) +
  scale_fill_manual(labels = c("High Strength", "Low Strength"),
                    values = c("black", "white")) +
  xlab(label = "") +
  ylab("relative frequency (%)") +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

hgs_dep

# Timed stands --------------------------------------------------------------------------------
# Ansiedade
ts_ansiedade_severa <-
  df |>
  group_by(ts_class, ansiedade_class_2) |>
  summarise(abs_ansiedade_2 = n(),
            perc_ansiedade_2 = n()/(nrow(df)/2)*100)

ts_ansiedade_severa

# Qui quadrado
ts_ans_chisquare <- table(df$ts_class, df$ansiedade_class)
chisq.test(ts_ans_chisquare)

# plot
ts_ans <-
  df |>
  group_by(ts_class, ansiedade_class) |>
  summarise(abs_ansiedade_severo = n(),
            perc_ansiedade_severo = n()/(nrow(df)/2)*100) |>
  ggplot(mapping = aes(x = ansiedade_class,
                       y = perc_ansiedade_severo,
                       colour = "black",
                       fill = ts_class)) +
  geom_col(position = "dodge",
           colour = "black") +
  geom_text(aes(
    label = c("13.9%","76.9%","7.4%","1.8%","24.1%","52.8%","17.6%","5.5%")),
    color = "black",
    position = position_dodge(0.9),
    size = 5,
    vjust = -0.5,
    show.legend = FALSE) +
  scale_x_discrete(limits = level_order) +
  scale_y_continuous(limits = c(0, 100, 25)) +
  scale_fill_manual(labels = c("High Timed Stands", "Low Timed Stands"),
                    values = c("black", "white")) +
  xlab(label = "") +
  ylab("relative frequency (%)") +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

ts_ans

# Depressao
ts_depressao_severa <-
  df |>
  group_by(ts_class, depressao_class) |>
  summarise(abs_depressao_severo = n(),
            perc_depressao_severo = n()/(nrow(df)/2)*100)

ts_depressao_severa

# Qui quadrado
ts_dep_chisquare <- table(df$ts_class, df$depressao_class)
chisq.test(ts_dep_chisquare)

# Plot
ts_dep <-
  df |>
  group_by(ts_class, depressao_class) |>
  summarise(abs_depressao_severo = n(),
            perc_depressao_severo = n()/(nrow(df)/2)*100) |>
  ggplot(mapping = aes(x = depressao_class,
                       y = perc_depressao_severo,
                       colour = "black",
                       fill = ts_class)) +
  geom_col(position = "dodge",
           colour = "black") +
  geom_text(aes(
    label = c("7.4%","81.5%","8.3%","2.7%","18.5%","65.7%","9.2%","6.4%")),
    color = "black",
    position = position_dodge(0.9),
    size = 5,
    vjust = -0.5,
    show.legend = FALSE) +
  scale_x_discrete(limits = level_order) +
  scale_y_continuous(limits = c(0, 100, 25)) +
  scale_fill_manual(labels = c("High Strength", "Low Strength"),
                    values = c("black", "white")) +
  xlab(label = "") +
  ylab("relative frequency (%)") +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

ts_dep

# Layout
(hgs_ans | ts_ans)/(hgs_dep | ts_dep)

# analises exploratórias - test t e regressão -------------------------------------------------
# tirar notação cientifica
options(scipen=999)

# Graficos + testes t para amostras independentes ---------------------------------------------
# Ansiedade
## HGS
t.test(ansiedade_score ~ hgs_class, df)

## TS
t.test(ansiedade_score ~ ts_class, df)

# Depressao
## HGS
t.test(depressao_score ~ hgs_class, df)

## TS
t.test(depressao_score ~ ts_class, df)

# Regressões lineares -------------------------------------------------------------------------
## HGS
### Ansiedade
#### Crude
model <- lm(ansiedade_score ~ hgs_max, df)
sjPlot::tab_model(model)

#### Ajustado 1
model <- lm(ansiedade_score ~ hgs_max + genero + idade_class + raca + renda + bmi_class, df)
sjPlot::tab_model(model)

#### Ajustado 2
model <- lm(ansiedade_score ~ hgs_max + genero + idade_class + raca + renda, df)
sjPlot::tab_model(model)

### Depressao
#### Crude
model <- lm(depressao_score ~ hgs_max, df)
sjPlot::tab_model(model)

#### Ajustado 1
model <- lm(depressao_score ~ hgs_max + genero + idade_class + raca + renda + bmi_class, df)
sjPlot::tab_model(model)

#### Ajustado 2
model <- lm(depressao_score ~ hgs_max + genero + idade_class + raca + renda, df)
sjPlot::tab_model(model)

## TS
### Ansiedade
#### Crude
model <- lm(ansiedade_score ~ ts_max, df)
sjPlot::tab_model(model)

#### Ajustado 1
model <- lm(ansiedade_score ~ ts_max + genero + idade_class + raca + renda + bmi_class, df)
sjPlot::tab_model(model)

#### Ajustado 2
model <- lm(ansiedade_score ~ ts_max + genero + idade_class + raca + renda, df)
sjPlot::tab_model(model)

### Depressao
#### Crude
model <- lm(depressao_score ~ ts_max, df)
sjPlot::tab_model(model)

#### Ajustado 1
model <- lm(depressao_score ~ ts_max + genero + idade_class + raca + renda + bmi_class, df)
sjPlot::tab_model(model)

#### Ajustado 2
model <- lm(depressao_score ~ ts_max + genero + idade_class + raca + renda, df)
sjPlot::tab_model(model)

