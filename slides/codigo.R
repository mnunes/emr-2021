########################
### regressao linear ###
########################

# pacotes necessarios

library(tidyverse)
theme_set(theme_bw())
library(tidymodels)

# analise exploratoria

ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point() +
  labs(x = "Velocidade (mph)", y = "Distância (pés)")

# determinacao do software

cars_lm <- 
  linear_reg() %>% 
  set_engine("lm")

# ajuste do modelo

cars_lm_fit <- 
  cars_lm %>% 
  fit(dist ~ speed, 
      data = cars)

# resultados

cars_lm_fit

# resultados

tidy(cars_lm_fit)

# semente aleatoria

set.seed(555)

# 75% dos dados como treino

cars_split <- initial_split(cars, prop = .75)
cars_split

# criar os conjuntos de dados de treino e teste

cars_treino <- training(cars_split)
nrow(cars_treino)/nrow(cars)

cars_teste  <- testing(cars_split)
nrow(cars_teste)/nrow(cars)

# receita

cars_rec <- 
  recipe(dist ~ speed, 
         data = cars_treino)

cars_rec

# modelo

cars_lm <- 
  linear_reg() %>% 
  set_engine("lm")

cars_lm

# criar workflow

cars_wflow <- 
  workflow() %>% 
  add_recipe(cars_rec) %>%
  add_model(cars_lm)

# ajuste do modelo

cars_lm_fit_treino <- fit(cars_wflow,
                          cars_treino)

tidy(cars_lm_fit_treino)
tidy(cars_lm_fit)

# semente aleatoria

set.seed(321)

# divisao dos dados

cars_treino_cv <- vfold_cv(cars_treino, 
                           v = 5)

cars_treino_cv

# modelo ajustado com validacao cruzada

cars_lm_fit_cv <- fit_resamples(cars_wflow,
                                cars_treino_cv)

cars_lm_fit_cv

# resultados

collect_metrics(cars_lm_fit_cv)

# resultados no conjunto de teste

resultado <- 
  cars_teste %>%
  bind_cols(predict(cars_lm_fit_treino, cars_teste) %>%
              rename(predicao_lm = .pred))

# resultado final

metrics(resultado, 
        truth = dist, 
        estimate = predicao_lm)

# grafico final

ggplot(resultado, aes(x = dist, y = predicao_lm)) +
  geom_point() +
  labs(x = "Valores Observados", y = "Valores Preditos") +
  geom_abline(intercept = 0, slope = 1) +
  coord_fixed()



#####################
### random forest ###
#####################

library(tidymodels)
library(onehot)
library(palmerpenguins)
library(GGally)
library(ggfortify)
library(vip)

# checagem dos dados

glimpse(penguins)

# criacao de variaveis dummy

pp <- 
  penguins %>%
  select(!where(is.numeric)) %>%
  select(-species) %>%
  onehot() %>%
  predict(penguins) %>%
  as.data.frame() %>%
  select(i_Biscoe    = `island=Biscoe`, 
         i_Dream     = `island=Dream`,
         i_Torgersen = `island=Torgersen`,
         s_fem       = `sex=female`,
         s_male      = `sex=male`)


pp <- 
  penguins %>%
  select(where(is.numeric), species, -year) %>%
  bind_cols(pp) %>%
  relocate(species) %>%
  na.omit()

# treino/teste

penguins %>%
  group_by(species) %>%
  count()

# 75% dos dados como treino

set.seed(1232)

pp_split <- initial_split(pp, prop = .75, strata = species)

# criar os conjuntos de dados de treino e teste

pp_treino <- training(pp_split)
pp_teste  <- testing(pp_split)

# eda

autoplot(prcomp(pp_treino %>% select(-species),
                center = TRUE, scale. = TRUE), 
         data = pp_treino,
         colour = "species") +
  scale_colour_viridis_d()

ggpairs(pp_treino %>% select(species, 
                             bill_length_mm,
                             bill_depth_mm,
                             flipper_length_mm,
                             body_mass_g), 
        aes(colour = species)) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d()


# pre-processamento

pp_rec <- 
  recipe(species ~ ., 
         data = pp_treino) %>%
  # remover observacoes de modo que todos os niveis de species
  # fiquem com o mesmo numero de observacoes
  themis::step_downsample(species) %>% 
  # center/scale
  step_center(-species) %>% 
  step_scale(-species) %>% 
  # funcao para aplicar a transformacao aos dados
  prep()


# aplicar a transformacao aos dados

pp_treino_t <- juice(pp_rec)

# preparar o conjunto de teste

pp_teste_t <- bake(pp_rec,
                   new_data = pp_teste)

# definicao do tuning

pp_rf_tune <-
  rand_forest(
    mtry = tune(),
    trees = 1000,
    min_n = tune()
  ) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "impurity")


# grid de procura

pp_rf_grid <- grid_regular(mtry(range(1, 9)),
                           min_n(range(10, 50)),
                           levels = c(9, 5))

# workflow

pp_rf_tune_wflow <- 
  workflow() %>%
  add_model(pp_rf_tune) %>%
  add_formula(species ~ .)

# definicao da validacao cruzada

set.seed(2389)

pp_treino_cv <- vfold_cv(pp_treino_t, v = 7)

# avaliacao do modelo

pp_rf_fit_tune <- 
  pp_rf_tune_wflow %>% 
  tune_grid(
    resamples = pp_treino_cv,
    grid = pp_rf_grid
  )

# resultados

collect_metrics(pp_rf_fit_tune)

pp_rf_fit_tune %>%
  collect_metrics() %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(., aes(x = mtry, y = mean, colour = min_n, group = min_n)) +
  geom_line() +
  geom_point() +
  facet_grid(~ .metric) +
  scale_x_continuous(breaks = seq(1, 9, 2)) +
  scale_colour_viridis_d()

# melhores modelos

pp_rf_fit_tune %>%
  show_best("roc_auc")

pp_rf_fit_tune %>%
  show_best("accuracy")

# melhor modelo

pp_rf_best <- 
  pp_rf_fit_tune %>%
  select_best("accuracy")

pp_rf_final <-
  pp_rf_tune_wflow %>%
  finalize_workflow(pp_rf_best)

pp_rf_final <- fit(pp_rf_final, 
                   pp_treino_t)

# resultados no conjunto de teste

resultado_rf <- 
  pp_teste_t %>%
  bind_cols(predict(pp_rf_final, pp_teste_t) %>%
              rename(predicao_rf = .pred_class))


metrics(resultado_rf, 
        truth = species, 
        estimate = predicao_rf,
        options = "roc")

conf_mat(resultado_rf, 
         truth = species, 
         estimate = predicao_rf) %>%
  autoplot(type = "heatmap")

# sensitividade

sens(resultado_rf, 
     truth = species, 
     estimate = predicao_rf)

# especificidade

spec(resultado_rf, 
     truth = species, 
     estimate = predicao_rf)

# importancia das variaveis

pp_rf_final %>% 
  extract_fit_parsnip() %>% 
  vip(scale = TRUE)




