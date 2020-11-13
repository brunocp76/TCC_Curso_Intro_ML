# 0 - Macro e Bibliotecas -------------------------------------------------
cls <- function() cat("\f")

library(vip)
library(tidyverse)
library(tidymodels)

Allocated_Memory <- paste(memory.size(), "Mb")


# 1 - Importar as Bases ---------------------------------------------------
# httr::GET("https://github.com/curso-r/main-intro-ml/raw/master/dados/adult.rds", httr::write_disk("adult.rds", overwrite = TRUE))
# adult <- read_rds("adult.rds")
# help(adult)

adult_val <- readRDS(file = "data-raw/adult_val.rds")

adult <- readRDS(file = "data-raw/adult.rds")

adult %>% glimpse()

adult %>% count(resposta)


# 2 - Convertendo Variaveis Categoricas em Fatores ------------------------

# Para ordenar os valores, olharei a WOE, ou Weight of Evidence das variaveis nominais...

IV0 <- adult %>%
  select(-id) %>%
  mutate(
    resp_num = ifelse(test = resposta == "<=50K",
                      yes = 0L,
                      no = 1L)
  ) %>%
  select(-resposta) %>%
  Information::create_infotables(
    y = "resp_num",
    bins = 20,
    parallel = TRUE
  )
cls()
IV0

IV0$Tables$relationship %>% arrange(WOE)
IV0$Tables$marital_status %>% arrange(WOE)
IV0$Tables$occupation %>% arrange(WOE)
IV0$Tables$education %>% arrange(WOE)
IV0$Tables$sex %>% arrange(WOE)
IV0$Tables$workclass %>% arrange(WOE)
IV0$Tables$native_country %>% arrange(WOE)
IV0$Tables$race %>% arrange(WOE)


adult2 <- adult %>%
  select(-id) %>%
  mutate(
    relationship = factor(
      relationship,
      levels = c(
        'Own-child',
        'Other-relative',
        'Unmarried',
        'Not-in-family',
        'Husband',
        'Wife'
      )
    ),
    marital_status = factor(
      marital_status,
      levels = c(
        'Never-married',
        'Separated',
        'Married-spouse-absent',
        'Widowed',
        'Divorced',
        'Married-AF-spouse',
        'Married-civ-spouse'
      )
    ),
    occupation = factor(
      occupation,
      levels = c(
        'Priv-house-serv',
        'Other-service',
        'Handlers-cleaners',
        NA,
        'Armed-Forces',
        'Farming-fishing',
        'Machine-op-inspct',
        'Adm-clerical',
        'Transport-moving',
        'Craft-repair',
        'Sales',
        'Tech-support',
        'Protective-serv',
        'Prof-specialty',
        'Exec-managerial'
      )
    ),
    education = factor(
      education,
      levels = c(
        'Preschool',
        '1st-4th',
        '5th-6th',
        '7th-8th',
        '9th',
        '10th',
        '11th',
        '12th',
        'HS-grad',
        'Some-college',
        'Assoc-acdm',
        'Assoc-voc',
        'Bachelors',
        'Masters',
        'Prof-school',
        'Doctorate'
      )
    ),
    sex = factor(
      sex,
      levels = c(
        'Female',
        'Male'
      )
    ),
    workclass = factor(
      workclass,
      levels = c(
        NA,
        'Private',
        'Never-worked',
        'Without-pay',
        'State-gov',
        'Self-emp-not-inc',
        'Local-gov',
        'Federal-gov',
        'Self-emp-inc'
      )
    ),
    native_country = factor(
      native_country,
      levels = c(
        'Dominican-Republic',
        'Columbia',
        'Guatemala',
        'Mexico',
        'Nicaragua',
        'Peru',
        'Vietnam',
        'Honduras',
        'El-Salvador',
        'Haiti',
        'Puerto-Rico',
        'Trinadad&Tobago',
        'Portugal',
        'Laos',
        'Jamaica',
        'Ecuador',
        'Thailand',
        'Poland',
        'South',
        'Ireland',
        'Hungary',
        'Holand-Netherlands',
        'Outlying-US(Guam-USVI-etc)',
        'United-States',
        'Scotland',
        NA,
        'Cuba',
        'China',
        'Greece',
        'Hong',
        'Philippines',
        'Germany',
        'Canada',
        'England',
        'Italy',
        'Cambodia',
        'Yugoslavia',
        'Japan',
        'Taiwan',
        'India',
        'France',
        'Iran'
      )
    ),
    race = factor(
      race,
      levels = c(
        'Other',
        'Amer-Indian-Eskimo',
        'Black',
        'White',
        'Asian-Pac-Islander'
      )
    )
  )

Allocated_Memory <- paste(memory.size(), "Mb")


# 3 - Rapido EDA ----------------------------------------------------------
cls()
adult %>% glimpse()
adult2 %>% glimpse()

SmartEDA::ExpNumViz(
  data = adult2,
  target = "resposta",
  type = 1,
  # nlim = NULL,
  fname = NULL,
  col = c("blue", "red"),
  Page = c(3, 2),
  sample = NULL
)

IV <- adult2 %>%
  mutate(
    resp_num = ifelse(test = resposta == "<=50K",
                      yes = 0L,
                      no = 1L)
  ) %>%
  select(-resposta) %>%
  Information::create_infotables(
    y = "resp_num",
    bins = 20,
    parallel = TRUE
  )
cls()
IV

IV$Tables$relationship %>% arrange(WOE)
IV$Tables$marital_status %>% arrange(WOE)
IV$Tables$occupation %>% arrange(WOE)
IV$Tables$education %>% arrange(WOE)
IV$Tables$sex %>% arrange(WOE)
IV$Tables$workclass %>% arrange(WOE)
IV$Tables$native_country %>% arrange(WOE)
IV$Tables$race %>% arrange(WOE)

SmartEDA::ExpCatStat(
  data = adult2,
  Target = "resposta",
  result = "Stat",
  clim = 20,
  # nlim = NULL,
  bins = 20,
  plot = FALSE,
  top = 30,
  Round = 5
) %>%
  filter(Variable != "id") %>%
  arrange(desc(`Cramers V`), desc(`Chi-squared`))

# Agora que ordenei as variaveis categoricas de acordo com a resposta...

table(adult2$relationship, adult2$resposta)
table(adult2$marital_status, adult2$resposta)
table(adult2$occupation, adult2$resposta)
table(adult2$education, adult2$resposta)
table(adult2$sex, adult2$resposta)
table(adult2$workclass, adult2$resposta)
table(adult2$race, adult2$resposta)


# 4 - Bases de Treino e Validacao -----------------------------------------
split <- initial_split(
  data = adult2,
  strata = resposta,
  prop = 3/4
)
split

train_adult <- training(split)
tests_adult <- testing(split)


# 5 - Duas Receitas no Capricho... ----------------------------------------
recipe_adult <- recipe(
  resposta ~ .,
  data = train_adult
) %>%
  step_zv(all_predictors()) %>%
  step_modeimpute(all_nominal(), -all_outcomes()) %>%
  step_medianimpute(all_numeric(), -all_outcomes()) %>%
  step_novel(all_nominal(), -all_outcomes()) %>%
  # step_pca(all_numeric()) %>%
  step_dummy(all_nominal(), -all_outcomes())

recipe_adult

recipe_adult_xb <- recipe(
  resposta ~ .,
  data = train_adult
) %>%
  step_zv(all_predictors()) %>%
  step_modeimpute(all_nominal(), -all_outcomes()) %>%
  step_medianimpute(all_numeric(), -all_outcomes()) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_novel(all_nominal(), -all_outcomes()) %>%
  # step_pca(all_numeric()) %>%
  step_dummy(all_nominal(), -all_outcomes())

recipe_adult_xb


# 6 - Definindo os Modelos ------------------------------------------------


# 6.1 - Regressao Logistica -----------------------------------------------
adult_rl_model <- logistic_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_mode("classification") %>%
  set_engine("glmnet")


# 6.2 - Arvore de Decisao -------------------------------------------------
adult_ad_model <- decision_tree(
  cost_complexity = tune(),
  min_n = tune(),
  tree_depth = tune()
) %>%
  set_mode("classification") %>%
  set_engine("rpart")


# 6.3 - XGBoost -----------------------------------------------------------
adult_xb_model <- boost_tree(
  trees = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune()
) %>%
  set_mode("classification") %>%
  set_engine("xgboost")


# 7 - Workflows -----------------------------------------------------------


# 7.1 - Workflow da Regressao Logistica -----------------------------------
adult_rl_wf <- workflow() %>%
  add_model(adult_rl_model) %>%
  add_recipe(recipe_adult)


# 7.2 - Workflow da Arvore de Decisao -------------------------------------
adult_ad_wf <- workflow() %>%
  add_model(adult_ad_model) %>%
  add_recipe(recipe_adult)


# 7.2 - Workflow da Arvore de Decisao -------------------------------------
adult_xb_wf <- workflow() %>%
  add_model(adult_xb_model) %>%
  add_recipe(recipe_adult_xb)


# 8 - Tunagem de Hiperparametros ------------------------------------------


# 8.1 - Cross-Validation --------------------------------------------------
adult_resamples <- vfold_cv(
  train_adult,
  v = 10,
  strata = resposta
)
adult_resamples


# 8.2 - Tune Grid da Regressao Logistica ----------------------------------
adult_rl_tune_grid <- tune_grid(
  object = adult_rl_wf,
  resamples = adult_resamples,
<<<<<<< HEAD
  grid = 20,
=======
  grid = 50,
>>>>>>> 7707e0e0cbd43ee907eb938379ab498686a7c9e9
  metrics = metric_set(roc_auc),
  control = control_grid(verbose = TRUE, allow_par = TRUE)
)


# 8.3 - Tune Grid da Arvore de Decisao ------------------------------------
adult_ad_tune_grid <- tune_grid(
  object = adult_ad_wf,
  resamples = adult_resamples,
<<<<<<< HEAD
  grid = 20,
=======
  grid = 50,
>>>>>>> 7707e0e0cbd43ee907eb938379ab498686a7c9e9
  metrics = metric_set(roc_auc),
  control = control_grid(verbose = TRUE, allow_par = TRUE)
)


# 8.4 - Tune Grid do XGBoost ----------------------------------------------
adult_xb_tune_grid <- tune_grid(
  object = adult_xb_wf,
  resamples = adult_resamples,
<<<<<<< HEAD
  grid = 20,
=======
  grid = 50,
>>>>>>> 7707e0e0cbd43ee907eb938379ab498686a7c9e9
  metrics = metric_set(roc_auc),
  control = control_grid(verbose = TRUE, allow_par = TRUE)
)


# 8.5 - Olhando as Metricas de Desempenho ---------------------------------
collect_metrics(adult_rl_tune_grid) %>% arrange(desc(mean)) %>% print.data.frame()
collect_metrics(adult_ad_tune_grid) %>% arrange(desc(mean)) %>% print.data.frame()
collect_metrics(adult_xb_tune_grid) %>% arrange(desc(mean)) %>% print.data.frame()

collect_metrics(adult_rl_tune_grid) %>%
  filter(penalty < 0.02) %>%
  ggplot(aes(x = penalty, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err)) +
  facet_wrap(~.metric, scales = "free") +
  scale_x_log10()

collect_metrics(adult_ad_tune_grid) %>%
  ggplot(aes(x = cost_complexity, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err)) +
  facet_wrap(~.metric, scales = "free") +
  scale_x_log10()

# collect_metrics(adult_xb_tune_grid) %>%
#   ggplot(aes(x = cost_complexity, y = mean)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err)) +
#   facet_wrap(~.metric, scales = "free") +
#   scale_x_log10()

Allocated_Memory <- paste(memory.size(), "Mb")


# 9 - Modelos Finais e Submissao ------------------------------------------
# graphics.off()
cls()


# 9.1 - Melhor Modelo de Regressao Logistica ------------------------------
adult_best_rl_params <- select_best(
  adult_rl_tune_grid,
  "roc_auc"
)
adult_best_rl_params


# 9.2 - Melhor Modelo de Arvore de Decisao --------------------------------
adult_best_ad_params <- select_best(
  adult_ad_tune_grid,
  "roc_auc"
)
adult_best_ad_params


# 9.3 - Melhor Modelo de Arvore de Decisao --------------------------------
adult_best_xb_params <- select_best(
  adult_xb_tune_grid,
  "roc_auc"
)
adult_best_xb_params


# 9.4 - Workflows de Finalizacao dos Modelos  -----------------------------
adult_rl_wf <- adult_rl_wf %>%
  finalize_workflow(adult_best_rl_params)
adult_rl_wf

adult_ad_wf <- adult_ad_wf %>%
  finalize_workflow(adult_best_ad_params)
adult_ad_wf

adult_xb_wf <- adult_xb_wf %>%
  finalize_workflow(adult_best_xb_params)
adult_xb_wf


# 9.5 - Aplicando os Melhores Modelos -------------------------------------
adult_rl_last_fit <- last_fit(
  adult_rl_wf,
  split
)
adult_rl_last_fit

adult_ad_last_fit <- last_fit(
  adult_ad_wf,
  split
)
adult_ad_last_fit

adult_xb_last_fit <- last_fit(
  adult_xb_wf,
  split
)
adult_xb_last_fit


# 9.6 - Metricas de Desempenho --------------------------------------------
collect_metrics(adult_rl_last_fit)
collect_metrics(adult_ad_last_fit)
collect_metrics(adult_xb_last_fit)

adult_test_rl_preds <- collect_predictions(adult_rl_last_fit)
adult_test_rl_preds

adult_test_ad_preds <- collect_predictions(adult_ad_last_fit)
adult_test_ad_preds

adult_test_xb_preds <- collect_predictions(adult_xb_last_fit)
adult_test_xb_preds

adult_roc_rl_curve <- adult_test_rl_preds %>% roc_curve(resposta, `.pred_<=50K`)
autoplot(adult_roc_rl_curve)

adult_roc_ad_curve <- adult_test_ad_preds %>% roc_curve(resposta, `.pred_<=50K`)
autoplot(adult_roc_ad_curve)

adult_roc_xb_curve <- adult_test_xb_preds %>% roc_curve(resposta, `.pred_<=50K`)
autoplot(adult_roc_xb_curve)


# 9.7 - Importancia de Variaveis nos Modelos ------------------------------
adult_rl_last_fit_model <- adult_rl_last_fit$.workflow[[1]]$fit$fit
adult_rl_last_fit_model
vip(adult_rl_last_fit_model)

adult_ad_last_fit_model <- adult_ad_last_fit$.workflow[[1]]$fit$fit
adult_ad_last_fit_model
vip(adult_ad_last_fit_model)

adult_xb_last_fit_model <- adult_xb_last_fit$.workflow[[1]]$fit$fit
adult_xb_last_fit_model
vip(adult_xb_last_fit_model)


# 9.8 - Matrizes de Confusao ----------------------------------------------
adult_test_rl_preds %>%
  mutate(
    resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
  ) %>%
  conf_mat(resposta, resposta_class)

adult_test_ad_preds %>%
  mutate(
    resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
  ) %>%
  conf_mat(resposta, resposta_class)

adult_test_xb_preds %>%
  mutate(
    resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
  ) %>%
  conf_mat(resposta, resposta_class)


# 9.9 - Finalmente Modelos Finais -----------------------------------------
adult_modelo_rl_final <- adult_rl_wf %>% fit(adult)
adult_modelo_ad_final <- adult_ad_wf %>% fit(adult)
adult_modelo_xb_final <- adult_xb_wf %>% fit(adult)

adult_val_submissao <- adult_val %>%
  mutate(
    more_than_50k = predict(
      adult_modelo_xb_final,
      new_data = adult_val,
      type = "prob"
    )$`.pred_>50K`
  ) %>%
  select(id, more_than_50k)
adult_val_submissao

write_csv(
  adult_val_submissao,
  "adult_val_submissao_Bruno_Pasquini.csv"
)

dim(adult_val_submissao)


# Limpando os Arquivos Temporarios ----------------------------------------
graphics.off()
cls()
Allocated_Memory <- paste(memory.size(), "Mb")
rm(list = ls())
