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

Allocated_Memory <- paste(memory.size(), "Mb")


# 2 - Convertendo Variaveis Categoricas em Fatores ------------------------

# Para ordenar os valores, olharei a WOE, ou Weight of Evidence das variaveis nominais...

# IV0 <- adult %>%
#   select(-id) %>%
#   mutate(
#     resp_num = ifelse(test = resposta == "<=50K",
#                       yes = 0L,
#                       no = 1L)
#   ) %>%
#   select(-resposta) %>%
#   Information::create_infotables(
#     y = "resp_num",
#     bins = 20,
#     parallel = FALSE
#   )
# IV0
#
# IV0$Tables$relationship %>% arrange(WOE)
# IV0$Tables$marital_status %>% arrange(WOE)
# IV0$Tables$occupation %>% arrange(WOE)
# IV0$Tables$education %>% arrange(WOE)
# IV0$Tables$sex %>% arrange(WOE)
# IV0$Tables$workclass %>% arrange(WOE)
# IV0$Tables$native_country %>% arrange(WOE)
# IV0$Tables$race %>% arrange(WOE)
#
#
# adult2 <- adult %>%
#   select(-id) %>%
#   mutate(
#     relationship = factor(
#       relationship,
#       levels = c(
#         'Own-child',
#         'Other-relative',
#         'Unmarried',
#         'Not-in-family',
#         'Husband',
#         'Wife'
#       )
#     ),
#     marital_status = factor(
#       marital_status,
#       levels = c(
#         'Never-married',
#         'Separated',
#         'Married-spouse-absent',
#         'Widowed',
#         'Divorced',
#         'Married-AF-spouse',
#         'Married-civ-spouse'
#       )
#     ),
#     occupation = factor(
#       occupation,
#       levels = c(
#         'Priv-house-serv',
#         'Other-service',
#         'Handlers-cleaners',
#         NA,
#         'Armed-Forces',
#         'Farming-fishing',
#         'Machine-op-inspct',
#         'Adm-clerical',
#         'Transport-moving',
#         'Craft-repair',
#         'Sales',
#         'Tech-support',
#         'Protective-serv',
#         'Prof-specialty',
#         'Exec-managerial'
#       )
#     ),
#     education = factor(
#       education,
#       levels = c(
#         'Preschool',
#         '1st-4th',
#         '5th-6th',
#         '7th-8th',
#         '9th',
#         '10th',
#         '11th',
#         '12th',
#         'HS-grad',
#         'Some-college',
#         'Assoc-acdm',
#         'Assoc-voc',
#         'Bachelors',
#         'Masters',
#         'Prof-school',
#         'Doctorate'
#       )
#     ),
#     sex = factor(
#       sex,
#       levels = c(
#         'Female',
#         'Male'
#       )
#     ),
#     workclass = factor(
#       workclass,
#       levels = c(
#         NA,
#         'Private',
#         'Never-worked',
#         'Without-pay',
#         'State-gov',
#         'Self-emp-not-inc',
#         'Local-gov',
#         'Federal-gov',
#         'Self-emp-inc'
#       )
#     ),
#     native_country = factor(
#       native_country,
#       levels = c(
#         'Dominican-Republic',
#         'Columbia',
#         'Guatemala',
#         'Mexico',
#         'Nicaragua',
#         'Peru',
#         'Vietnam',
#         'Honduras',
#         'El-Salvador',
#         'Haiti',
#         'Puerto-Rico',
#         'Trinadad&Tobago',
#         'Portugal',
#         'Laos',
#         'Jamaica',
#         'Ecuador',
#         'Thailand',
#         'Poland',
#         'South',
#         'Ireland',
#         'Hungary',
#         'Holand-Netherlands',
#         'Outlying-US(Guam-USVI-etc)',
#         'United-States',
#         'Scotland',
#         NA,
#         'Cuba',
#         'China',
#         'Greece',
#         'Hong',
#         'Philippines',
#         'Germany',
#         'Canada',
#         'England',
#         'Italy',
#         'Cambodia',
#         'Yugoslavia',
#         'Japan',
#         'Taiwan',
#         'India',
#         'France',
#         'Iran'
#       )
#     ),
#     race = factor(
#       race,
#       levels = c(
#         'Other',
#         'Amer-Indian-Eskimo',
#         'Black',
#         'White',
#         'Asian-Pac-Islander'
#       )
#     )
#   )
#
# Allocated_Memory <- paste(memory.size(), "Mb")
#
#
# cls()
# adult %>% glimpse()
# adult2 %>% glimpse()
#
# SmartEDA::ExpNumViz(
#   data = adult2,
#   target = "resposta",
#   type = 1,
#   # nlim = NULL,
#   fname = NULL,
#   col = c("blue", "red"),
#   Page = c(3, 2),
#   sample = NULL
# )
#
# IV <- adult2 %>%
#   mutate(
#     resp_num = ifelse(test = resposta == "<=50K",
#                       yes = 0L,
#                       no = 1L)
#   ) %>%
#   select(-resposta) %>%
#   Information::create_infotables(
#     y = "resp_num",
#     bins = 20,
#     parallel = TRUE
#   )
# IV
#
# IV$Tables$relationship %>% arrange(WOE)
# IV$Tables$marital_status %>% arrange(WOE)
# IV$Tables$occupation %>% arrange(WOE)
# IV$Tables$education %>% arrange(WOE)
# IV$Tables$sex %>% arrange(WOE)
# IV$Tables$workclass %>% arrange(WOE)
# IV$Tables$native_country %>% arrange(WOE)
# IV$Tables$race %>% arrange(WOE)
#
# SmartEDA::ExpCatStat(
#   data = adult2,
#   Target = "resposta",
#   result = "Stat",
#   clim = 20,
#   # nlim = NULL,
#   bins = 20,
#   plot = FALSE,
#   top = 30,
#   Round = 5
# ) %>%
#   filter(Variable != "id") %>%
#   arrange(desc(`Cramers V`), desc(`Chi-squared`))
#
# # Agora que ordenei as variaveis categoricas de acordo com a resposta...
#
# table(adult2$relationship, adult2$resposta)
# table(adult2$marital_status, adult2$resposta)
# table(adult2$occupation, adult2$resposta)
# table(adult2$education, adult2$resposta)
# table(adult2$sex, adult2$resposta)
# table(adult2$workclass, adult2$resposta)
# table(adult2$race, adult2$resposta)


# 3 - Bases de Treino e Validacao -----------------------------------------
split <- initial_split(
   data = adult,
   strata = resposta,
   prop = 3/4
)
split

train_adult <- training(split)
tests_adult <- testing(split)


# 4 - Uma Receita no Capricho... ------------------------------------------
recipe_adult <- recipe(
   formula = resposta ~ .,
   data = train_adult
) %>%
   step_zv(all_predictors()) %>%
   step_modeimpute(all_nominal(), -all_outcomes()) %>%
   step_medianimpute(all_numeric(), -all_outcomes()) %>%
   step_novel(all_nominal(), -all_outcomes()) %>%
   step_dummy(all_nominal(), -all_outcomes())

recipe_adult

# bake(prep(recipe_adult))
# 5 - Definindo os Modelos ------------------------------------------------


# 5.1 - Regressao Logistica -----------------------------------------------
adult_rl_model <- logistic_reg(
   mode = "classification",
   penalty = tune(),
   mixture = tune()
) %>%
   set_mode("classification") %>%
   set_engine("glmnet")


# 5.2 - Arvore de Decisao -------------------------------------------------
adult_ad_model <- decision_tree(
   mode = "classification",
   cost_complexity = tune(),
   min_n = tune(),
   tree_depth = tune()
) %>%
   set_mode("classification") %>%
   set_engine("rpart")


# 5.3 - Boost Tree --------------------------------------------------------
adult_bt_model <- boost_tree(
   mode = "classification",
   mtry = tune(),
   trees = tune(),
   min_n = tune(),
   tree_depth = tune(),
   learn_rate = tune(),
   loss_reduction = tune(),
   sample_size = tune(),
   stop_iter = tune()
) %>%
   set_mode("classification") %>%
   set_engine("xgboost")


# 5.4 - MARS --------------------------------------------------------------
adult_ma_model <- mars(
   mode = "classification",
   num_terms = tune(),
   prod_degree = tune(),
   prune_method = tune()
) %>%
   set_mode("classification") %>%
   set_engine("earth")


# 5.5 - Multilayer Perceptron ---------------------------------------------
adult_mp_model <- mlp(
   mode = "classification",
   hidden_units = tune(),
   penalty = tune(),
   dropout = tune(),
   epochs = tune(),
   activation = tune()
) %>%
   set_mode("classification") %>%
   set_engine("keras")


# 5.6 - K-Nearest Neighbor ------------------------------------------------
adult_kn_model <- nearest_neighbor(
   mode = "classification",
   neighbors = tune(),
   weight_func = tune(),
   dist_power = tune()
) %>%
   set_mode("classification") %>%
   set_engine("kknn")


# 5.7 - Random Forest -----------------------------------------------------
adult_rf_model <- rand_forest(
   mode = "classification",
   mtry = tune(),
   trees = tune(),
   min_n = tune()
) %>%
   set_mode("classification") %>%
   set_engine("ranger")


# 5.8 - Polynomial Support Vector Machines --------------------------------
adult_ps_model <- svm_poly(
   mode = "classification",
   cost = tune(),
   degree = tune(),
   scale_factor = tune()
) %>%
   set_mode("classification") %>%
   set_engine("kernlab")


# 5.9 - Radial Basis Function Support Vector Machines ---------------------
adult_rs_model <- svm_rbf(
   mode = "classification",
   cost = tune(),
   rbf_sigma = tune(),
   margin = tune()
) %>%
   set_mode("classification") %>%
   set_engine("kernlab")


# 6 - Workflows -----------------------------------------------------------


# 6.1 - Regressao Logistica -----------------------------------------------
adult_rl_wf <- workflow() %>%
   add_model(adult_rl_model) %>%
   add_recipe(recipe_adult)


# 6.2 - Arvore de Decisao -------------------------------------------------
adult_ad_wf <- workflow() %>%
   add_model(adult_ad_model) %>%
   add_recipe(recipe_adult)


# 6.3 - Boost Tree --------------------------------------------------------
adult_bt_wf <- workflow() %>%
   add_model(adult_bt_model) %>%
   add_recipe(recipe_adult)


# 6.4 - MARS --------------------------------------------------------------
adult_ma_wf <- workflow() %>%
   add_model(adult_ma_model) %>%
   add_recipe(recipe_adult)


# 6.5 - Multilayer Perceptron ---------------------------------------------
adult_mp_wf <- workflow() %>%
   add_model(adult_mp_model) %>%
   add_recipe(recipe_adult)


# 6.6 - K-Nearest Neighbor ------------------------------------------------
adult_kn_wf <- workflow() %>%
   add_model(adult_kn_model) %>%
   add_recipe(recipe_adult)


# 6.7 - Random Forest -----------------------------------------------------
adult_rf_wf <- workflow() %>%
   add_model(adult_rf_model) %>%
   add_recipe(recipe_adult)


# 6.8 - Polynomial Support Vector Machines --------------------------------
adult_ps_wf <- workflow() %>%
   add_model(adult_ps_model) %>%
   add_recipe(recipe_adult)


# 6.9 - Radial Basis Function Support Vector Machines ---------------------
adult_rs_wf <- workflow() %>%
   add_model(adult_rs_model) %>%
   add_recipe(recipe_adult)


# 7 - Tunagem de Hiperparametros ------------------------------------------


# 7.0 - Cross-Validation --------------------------------------------------
adult_resamples <- vfold_cv(
   train_adult,
   v = 5,
   strata = resposta
)
adult_resamples


# 7.1 - Regressao Logistica -----------------------------------------------
adult_rl_tune_grid <- tune_grid(
   object = adult_rl_wf,
   resamples = adult_resamples,
   grid = 10,
   metrics = metric_set(roc_auc),
   control = control_grid(verbose = TRUE, allow_par = TRUE)
)


# 7.2 - Arvore de Decisao -------------------------------------------------
adult_ad_tune_grid <- tune_grid(
   object = adult_ad_wf,
   resamples = adult_resamples,
   grid = 10,
   metrics = metric_set(roc_auc),
   control = control_grid(verbose = TRUE, allow_par = TRUE)
)


# 7.3 - Boost Tree --------------------------------------------------------
adult_bt_tune_grid <- tune_grid(
   object = adult_bt_wf,
   resamples = adult_resamples,
   grid = 3,
   metrics = metric_set(roc_auc),
   control = control_grid(verbose = TRUE, allow_par = TRUE)
)


# 7.4 - MARS --------------------------------------------------------------
adult_ma_tune_grid <- tune_grid(
   object = adult_ma_wf,
   resamples = adult_resamples,
   grid = 10,
   metrics = metric_set(roc_auc),
   control = control_grid(verbose = TRUE, allow_par = TRUE)
)


# 7.5 - Multilayer Perceptron ---------------------------------------------
adult_mp_tune_grid <- tune_grid(
   object = adult_mp_wf,
   resamples = adult_resamples,
   grid = 10,
   metrics = metric_set(roc_auc),
   control = control_grid(verbose = TRUE, allow_par = TRUE)
)


# 7.6 - K-Nearest Neighbor ------------------------------------------------
adult_kn_tune_grid <- tune_grid(
   object = adult_kn_wf,
   resamples = adult_resamples,
   grid = 10,
   metrics = metric_set(roc_auc),
   control = control_grid(verbose = TRUE, allow_par = TRUE)
)


# 7.7 - Random Forest -----------------------------------------------------
adult_rf_tune_grid <- tune_grid(
   object = adult_rf_wf,
   resamples = adult_resamples,
   grid = 10,
   metrics = metric_set(roc_auc),
   control = control_grid(verbose = TRUE, allow_par = TRUE)
)


# 7.8 - Polynomial Support Vector Machines --------------------------------
adult_ps_tune_grid <- tune_grid(
   object = adult_ps_wf,
   resamples = adult_resamples,
   grid = 10,
   metrics = metric_set(roc_auc),
   control = control_grid(verbose = TRUE, allow_par = TRUE)
)


# 7.9 - Radial Basis Function Support Vector Machines ---------------------
adult_rs_tune_grid <- tune_grid(
   object = adult_rs_wf,
   resamples = adult_resamples,
   grid = 10,
   metrics = metric_set(roc_auc),
   control = control_grid(verbose = TRUE, allow_par = TRUE)
)


# 8 - Modelos Finais e Submissao ------------------------------------------
graphics.off()


# 8.0 - Olhando as Metricas de Desempenho ---------------------------------
collect_metrics(adult_rl_tune_grid) %>% arrange(desc(mean))
collect_metrics(adult_ad_tune_grid) %>% arrange(desc(mean))
collect_metrics(adult_bt_tune_grid) %>% arrange(desc(mean))
collect_metrics(adult_ma_tune_grid) %>% arrange(desc(mean))
collect_metrics(adult_mp_tune_grid) %>% arrange(desc(mean))
collect_metrics(adult_kn_tune_grid) %>% arrange(desc(mean))
collect_metrics(adult_rf_tune_grid) %>% arrange(desc(mean))
collect_metrics(adult_ps_tune_grid) %>% arrange(desc(mean))
collect_metrics(adult_rs_tune_grid) %>% arrange(desc(mean))


collect_metrics(adult_rl_tune_grid) %>%
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

Allocated_Memory <- paste(memory.size(), "Mb")


# 8.1 - Regressao Logistica -----------------------------------------------
adult_best_rl_params <- select_best(
   x = adult_rl_tune_grid,
   metric = "roc_auc"
)
adult_best_rl_params


# 8.2 - Arvore de Decisao -------------------------------------------------
adult_best_ad_params <- select_best(
   x = adult_ad_tune_grid,
   metric = "roc_auc"
)
adult_best_ad_params


# 8.3 - Boost Tree --------------------------------------------------------
adult_best_bt_params <- select_best(
   x = adult_bt_tune_grid,
   metric = "roc_auc"
)
adult_best_bt_params


# 8.4 - MARS --------------------------------------------------------------
adult_best_ma_params <- select_best(
   x = adult_ma_tune_grid,
   metric = "roc_auc"
)
adult_best_ma_params


# 8.5 - Multilayer Perceptron ---------------------------------------------
adult_best_mp_params <- select_best(
   x = adult_mp_tune_grid,
   metric = "roc_auc"
)
adult_best_mp_params


# 8.6 - K-Nearest Neighbor ------------------------------------------------
adult_best_kn_params <- select_best(
   x = adult_kn_tune_grid,
   metric = "roc_auc"
)
adult_best_kn_params


# 8.7 - Random Forest -----------------------------------------------------
adult_best_rf_params <- select_best(
   x = adult_rf_tune_grid,
   metric = "roc_auc"
)
adult_best_rf_params


# 8.8 - Polynomial Support Vector Machines --------------------------------
adult_best_ps_params <- select_best(
   x = adult_ps_tune_grid,
   metric = "roc_auc"
)
adult_best_ps_params


# 8.9 - Radial Basis Function Support Vector Machines ---------------------
adult_best_rs_params <- select_best(
   x = adult_rs_tune_grid,
   metric = "roc_auc"
)
adult_best_rs_params


# 9 - Finalizando... ------------------------------------------------------


# 9.1 - Workflows de Finalizacao dos Modelos  -----------------------------
adult_rl_wf <- adult_rl_wf %>%
   finalize_workflow(adult_best_rl_params)
adult_rl_wf

adult_ad_wf <- adult_ad_wf %>%
   finalize_workflow(adult_best_ad_params)
adult_ad_wf

adult_bt_wf <- adult_bt_wf %>%
   finalize_workflow(adult_best_bt_params)
adult_bt_wf

adult_ma_wf <- adult_ma_wf %>%
   finalize_workflow(adult_best_ma_params)
adult_ma_wf

adult_mp_wf <- adult_mp_wf %>%
   finalize_workflow(adult_best_mp_params)
adult_mp_wf

adult_kn_wf <- adult_kn_wf %>%
   finalize_workflow(adult_best_kn_params)
adult_kn_wf

adult_rf_wf <- adult_rf_wf %>%
   finalize_workflow(adult_best_rf_params)
adult_rf_wf

adult_ps_wf <- adult_ps_wf %>%
   finalize_workflow(adult_best_ps_params)
adult_ps_wf

adult_rs_wf <- adult_rs_wf %>%
   finalize_workflow(adult_best_rs_params)
adult_rs_wf


# 9.2 - Aplicando os Melhores Modelos -------------------------------------
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

adult_bt_last_fit <- last_fit(
   adult_bt_wf,
   split
)
adult_bt_last_fit

adult_ma_last_fit <- last_fit(
   adult_ma_wf,
   split
)
adult_ma_last_fit

adult_mp_last_fit <- last_fit(
   adult_mp_wf,
   split
)
adult_mp_last_fit

adult_kn_last_fit <- last_fit(
   adult_kn_wf,
   split
)
adult_kn_last_fit

adult_rf_last_fit <- last_fit(
   adult_rf_wf,
   split
)
adult_rf_last_fit

adult_ps_last_fit <- last_fit(
   adult_ps_wf,
   split
)
adult_ps_last_fit

adult_rs_last_fit <- last_fit(
   adult_rs_wf,
   split
)
adult_rs_last_fit


# 9.3 - Metricas de Desempenho --------------------------------------------
collect_metrics(adult_rl_last_fit)
collect_metrics(adult_ad_last_fit)
collect_metrics(adult_ad_last_fit)
collect_metrics(adult_ad_last_fit)
collect_metrics(adult_ad_last_fit)
collect_metrics(adult_ad_last_fit)
collect_metrics(adult_ad_last_fit)
collect_metrics(adult_ad_last_fit)
collect_metrics(adult_ad_last_fit)


adult_test_rl_preds <- collect_predictions(adult_rl_last_fit)
adult_test_rl_preds

adult_test_ad_preds <- collect_predictions(adult_ad_last_fit)
adult_test_ad_preds

adult_test_bt_preds <- collect_predictions(adult_bt_last_fit)
adult_test_bt_preds

adult_test_ma_preds <- collect_predictions(adult_ma_last_fit)
adult_test_ma_preds

adult_test_mp_preds <- collect_predictions(adult_mp_last_fit)
adult_test_mp_preds

adult_test_kn_preds <- collect_predictions(adult_kn_last_fit)
adult_test_kn_preds

adult_test_rf_preds <- collect_predictions(adult_rf_last_fit)
adult_test_rf_preds

adult_test_ps_preds <- collect_predictions(adult_ps_last_fit)
adult_test_ps_preds

adult_test_rs_preds <- collect_predictions(adult_rs_last_fit)
adult_test_rs_preds


# 9.4 - ROC para os Modelos -----------------------------------------------
adult_roc_rl_curve <- adult_test_rl_preds %>%
   roc_curve(resposta, `.pred_<=50K`)
autoplot(adult_roc_rl_curve)

adult_roc_ad_curve <- adult_test_ad_preds %>%
   roc_curve(resposta, `.pred_<=50K`)
autoplot(adult_roc_ad_curve)

adult_roc_bt_curve <- adult_test_bt_preds %>%
   roc_curve(resposta, `.pred_<=50K`)
autoplot(adult_roc_bt_curve)

adult_roc_ma_curve <- adult_test_ma_preds %>%
   roc_curve(resposta, `.pred_<=50K`)
autoplot(adult_roc_ma_curve)

adult_roc_mp_curve <- adult_test_mp_preds %>%
   roc_curve(resposta, `.pred_<=50K`)
autoplot(adult_roc_mp_curve)

adult_roc_kn_curve <- adult_test_kn_preds %>%
   roc_curve(resposta, `.pred_<=50K`)
autoplot(adult_roc_kn_curve)

adult_roc_rf_curve <- adult_test_rf_preds %>%
   roc_curve(resposta, `.pred_<=50K`)
autoplot(adult_roc_rf_curve)

adult_roc_ps_curve <- adult_test_ps_preds %>%
   roc_curve(resposta, `.pred_<=50K`)
autoplot(adult_roc_ps_curve)

adult_roc_rs_curve <- adult_test_rs_preds %>%
   roc_curve(resposta, `.pred_<=50K`)
autoplot(adult_roc_rs_curve)


# 9.5 - Importancia de Variaveis nos Modelos ------------------------------
adult_rl_last_fit_model <- adult_rl_last_fit$.workflow[[1]]$fit$fit
adult_rl_last_fit_model
vip(adult_rl_last_fit_model)

adult_ad_last_fit_model <- adult_ad_last_fit$.workflow[[1]]$fit$fit
adult_ad_last_fit_model
vip(adult_ad_last_fit_model)

adult_bt_last_fit_model <- adult_bt_last_fit$.workflow[[1]]$fit$fit
adult_bt_last_fit_model
vip(adult_bt_last_fit_model)

adult_ma_last_fit_model <- adult_ma_last_fit$.workflow[[1]]$fit$fit
adult_ma_last_fit_model
vip(adult_ma_last_fit_model)

adult_mp_last_fit_model <- adult_mp_last_fit$.workflow[[1]]$fit$fit
adult_mp_last_fit_model
vip(adult_mp_last_fit_model)

adult_kn_last_fit_model <- adult_kn_last_fit$.workflow[[1]]$fit$fit
adult_kn_last_fit_model
vip(adult_kn_last_fit_model)

adult_rf_last_fit_model <- adult_rf_last_fit$.workflow[[1]]$fit$fit
adult_rf_last_fit_model
vip(adult_rf_last_fit_model)

adult_ps_last_fit_model <- adult_ps_last_fit$.workflow[[1]]$fit$fit
adult_ps_last_fit_model
vip(adult_ps_last_fit_model)

adult_rs_last_fit_model <- adult_rs_last_fit$.workflow[[1]]$fit$fit
adult_rs_last_fit_model
vip(adult_rs_last_fit_model)



# 9.6 - Matrizes de Confusao ----------------------------------------------
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

adult_test_bt_preds %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

adult_test_ma_preds %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

adult_test_mp_preds %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

adult_test_kn_preds %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

adult_test_rf_preds %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

adult_test_ps_preds %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

adult_test_rs_preds %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)


# 9.7 - Finalmente Modelos Finais -----------------------------------------
adult_modelo_rl_final <- adult_rl_wf %>% fit(adult)
adult_modelo_ad_final <- adult_ad_wf %>% fit(adult)
adult_modelo_bt_final <- adult_bt_wf %>% fit(adult)
adult_modelo_ma_final <- adult_ma_wf %>% fit(adult)
adult_modelo_mp_final <- adult_mp_wf %>% fit(adult)
adult_modelo_kn_final <- adult_kn_wf %>% fit(adult)
adult_modelo_rf_final <- adult_rf_wf %>% fit(adult)
adult_modelo_ps_final <- adult_ps_wf %>% fit(adult)
adult_modelo_rs_final <- adult_rs_wf %>% fit(adult)

adult_val_submissao <- adult_val %>%
   mutate(
      more_than_50k = predict(

         adult_modelo_rl_final,

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


# 9.8 - Limpando os Arquivos Temporarios ----------------------------------
graphics.off()
cls()
Allocated_Memory <- paste(memory.size(), "Mb")
rm(list = ls())
