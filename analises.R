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
         depressao_class = if_else(depressao_score >= 29, 1,0),
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
  mutate(raca = as.factor(raca)) |>
  mutate(ansiedade_class = as.factor(ansiedade_class)) |>
  mutate(depressao_class = as.factor(depressao_class))

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
  scale_x_discrete(labels = c("High\nStrength", "Low\nStrength"))+
  theme(
    axis.title.x = element_blank()
  )

# Grafico de barras para mostrar frequencia relativa de sintomas severos de ansiedade  --------
# handgrip ------------------------------------------------------------------------------------
# Ansiedade
hgs_ansiedade_severa <-
  df |>
  group_by(hgs_class, ansiedade_class) |>
  summarise(abs_ansiedade_severo = n(),
            perc_ansiedade_severo = n()/(nrow(df)/2)*100)

hgs_ansiedade_severa

# plot
hgs_ans <-
hgs_ansiedade_severa |>
  filter(ansiedade_class == 1) |>
  ggplot(mapping = aes(x = hgs_class,
                       y = perc_ansiedade_severo)) +
  geom_col(mapping = aes(),
           color = "black",
           fill = c("black", "white")) +
  geom_text(aes(label = c("42.6%", "67.6%")),
            size = 8,
            vjust = 0,
            hjust = -0.2) +
  ylab(label = "Relative frequency (%)") +
  xlab(label = "") +
  scale_x_discrete(labels = c("High\nStrength", "Low\nStrength"))+
  theme(panel.grid = element_line(colour = "black",
                                  linetype = 2))  +
  theme_classic() +
  scale_y_continuous(limits = c(0, 80)) +
  coord_flip()

# Depressao
hgs_depressao_severa <-
  df |>
  group_by(hgs_class, depressao_class) |>
  summarise(abs_depressao_severo = n(),
            perc_depressao_severo = n()/(nrow(df)/2)*100)

hgs_depressao_severa

# Plot
hgs_dep <-
  hgs_depressao_severa |>
  filter(depressao_class == 1) |>
  ggplot(mapping = aes(x = hgs_class,
                       y = perc_depressao_severo)) +
  geom_col(mapping = aes(),
           color = "black",
           fill = c("black", "white")) +
  geom_text(aes(label = c("2.7%", "7.4%")),
            # position = position_dodge(0.9),
            size = 8,
            vjust = 0,
            hjust = -0.2) +
  ylab(label = "Relative frequency (%)") +
  xlab(label = "") +
  scale_x_discrete(labels = c("High\nStrength", "Low\nStrength"))+
  theme(panel.grid = element_line(colour = "black",
                                  linetype = 2))  +
  theme_classic() +
  scale_y_continuous(limits = c(0, 10)) +
  coord_flip()

# Timed stands --------------------------------------------------------------------------------
# Ansiedade
ts_ansiedade_severa <-
  df |>
  group_by(ts_class, ansiedade_class) |>
  summarise(abs_ansiedade_severo = n(),
            perc_ansiedade_severo = n()/(nrow(df)/2)*100)

ts_ansiedade_severa

# plot
ts_ans <-
  ts_ansiedade_severa |>
  filter(ansiedade_class == 1) |>
  ggplot(mapping = aes(x = ts_class,
                       y = perc_ansiedade_severo)) +
  geom_col(mapping = aes(),
           color = "black",
           fill = c("black", "white")) +
  geom_text(aes(label = c("42.6%", "67.6%")),
            size = 8,
            vjust = 0,
            hjust = -0.2) +
  ylab(label = "Relative frequency (%)") +
  xlab(label = "") +
  scale_x_discrete(labels = c("High\nStrength", "Low\nStrength"))+
  theme(panel.grid = element_line(colour = "black",
                                  linetype = 2))  +
  theme_classic() +
  scale_y_continuous(limits = c(0, 80)) +
  coord_flip()

# Depressao
ts_depressao_severa <-
  df |>
  group_by(ts_class, depressao_class) |>
  summarise(abs_depressao_severo = n(),
            perc_depressao_severo = n()/(nrow(df)/2)*100)

ts_depressao_severa

# Plot
ts_dep <-
  ts_depressao_severa |>
  filter(depressao_class == 1) |>
  ggplot(mapping = aes(x = ts_class,
                       y = perc_depressao_severo)) +
  geom_col(mapping = aes(),
           color = "black",
           fill = c("black", "white")) +
  geom_text(aes(label = c("1.8%", "8.3%")),
            # position = position_dodge(0.9),
            size = 8,
            vjust = 0,
            hjust = -0.2) +
  ylab(label = "Relative frequency (%)") +
  xlab(label = "") +
  scale_x_discrete(labels = c("High\nStrength", "Low\nStrength"))+
  theme(panel.grid = element_line(colour = "black",
                                  linetype = 2))  +
  theme_classic() +
  scale_y_continuous(limits = c(0, 10)) +
  coord_flip()

(hgs_ans | hgs_dep)/(ts_ans | ts_dep)
# analises exploratórias - test t e regressão -------------------------------------------------
# tirar notação cientifica
options(scipen=999)

# Graficos + testes t para amostras independentes ---------------------------------------------
# Ansiedade
## HGS
t.test(depressao_score ~ tug_class, df)

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

model <- glm(ansiedade_class ~ hgs_class +
              genero + idade_class + raca + renda + bmi_class, df, family = binomial)

summary(model)






