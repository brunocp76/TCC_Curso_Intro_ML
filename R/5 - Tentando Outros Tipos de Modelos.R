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
      parallel = FALSE
   )
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


# 3 - Bases de Treino e Validacao -----------------------------------------
split <-
   initial_split(
      data = adult2,
      strata = resposta,
      prop = 3/4
   )
split

train_adult <- training(split)
tests_adult <- testing(split)

train_adult %>% glimpse()
tests_adult %>% glimpse()


# 4 - Algumas Receitas no Capricho... -------------------------------------


# 4.0 - Generica ----------------------------------------------------------
adult_recipe <-
   recipe(
      formula = resposta ~ .,
      data = train_adult
   ) %>%
   step_modeimpute(all_nominal(), -all_outcomes()) %>%
   step_medianimpute(all_numeric(), -all_outcomes()) %>%
   step_novel(all_nominal(), -all_outcomes()) %>%
   step_dummy(all_nominal(), -all_outcomes()) %>%
   step_zv(all_predictors())

adult_recipe


# 4.1 - Modelos Lineares Generalizados ------------------------------------
glm_recipe <-
   recipe(
      formula = resposta ~ .,
      data = train_adult
   ) %>%
   step_modeimpute(all_nominal(), -all_outcomes()) %>%
   step_medianimpute(all_numeric(), -all_outcomes()) %>%
   ## For modeling, it is preferred to encode qualitative data as factors
   ## (instead of character).
   ## I have done this already...
   # step_string2factor(one_of(resposta)) %>%
   step_novel(all_nominal(), -all_outcomes()) %>%
   ## This model requires the predictors to be numeric. The most common
   ## method to convert qualitative predictors to numeric is to create
   ## binary indicator variables (aka dummy variables) from these
   ## predictors.
   step_dummy(all_nominal(), -all_outcomes()) %>%
   ## Regularization methods sum up functions of the model slope
   ## coefficients. Because of this, the predictor variables should be on
   ## the same scale. Before centering and scaling the numeric predictors,
   ## any predictors with a single unique value are filtered out.
   step_zv(all_predictors()) %>%
   step_normalize(all_predictors(), -all_nominal())

glm_recipe


# 4.3 - Boost Tree - XGBoost ----------------------------------------------
xgboost_recipe <-
   recipe(
      formula = resposta ~ .,
      data = train_adult
   ) %>%
   step_modeimpute(all_nominal(), -all_outcomes()) %>%
   step_medianimpute(all_numeric(), -all_outcomes()) %>%
   ## For modeling, it is preferred to encode qualitative data as factors
   ## (instead of character).
   ## I have done this already...
   # step_string2factor(one_of(resposta)) %>%
   step_novel(all_nominal(), -all_outcomes()) %>%
   ## This model requires the predictors to be numeric. The most common
   ## method to convert qualitative predictors to numeric is to create
   ## binary indicator variables (aka dummy variables) from these
   ## predictors. However, for this model, binary indicator variables can be
   ## made for each of the levels of the factors (known as 'one-hot
   ## encoding').
   ## Extra steps based on Vinicius Jacobs' sugestion...
   step_center(all_numeric()) %>%
   step_scale(all_numeric()) %>%
   ## Extra steps based on Vinicius Jacobs' sugestion...
   step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
   step_zv(all_predictors())

xgboost_recipe


# 4.4 - MARS --------------------------------------------------------------
earth_recipe <-
   recipe(
      formula = resposta ~ .,
      data = train_adult
   ) %>%
   step_modeimpute(all_nominal(), -all_outcomes()) %>%
   step_medianimpute(all_numeric(), -all_outcomes()) %>%
   ## For modeling, it is preferred to encode qualitative data as factors
   ## (instead of character).
   ## I have done this already...
   # step_string2factor(one_of(resposta)) %>%
   step_novel(all_nominal(), -all_outcomes()) %>%
   ## This model requires the predictors to be numeric. The most common
   ## method to convert qualitative predictors to numeric is to create
   ## binary indicator variables (aka dummy variables) from these
   ## predictors.
   step_dummy(all_nominal(), -all_outcomes()) %>%
   step_zv(all_predictors())

earth_recipe


# 4.6 - K-Nearest Neighbors -----------------------------------------------
knn_recipe <-
   recipe(
      formula = resposta ~ .,
      data = train_adult
   ) %>%
   ## For modeling, it is preferred to encode qualitative data as factors
   ## (instead of character).
   ## I have done this already...
   # step_string2factor(one_of(resposta)) %>%
   step_novel(all_nominal(), -all_outcomes()) %>%
   ## This model requires the predictors to be numeric. The most common
   ## method to convert qualitative predictors to numeric is to create
   ## binary indicator variables (aka dummy variables) from these
   ## predictors.
   step_dummy(all_nominal(), -all_outcomes()) %>%
   ## Since distance calculations are used, the predictor variables should
   ## be on the same scale. Before centering and scaling the numeric
   ## predictors, any predictors with a single unique value are filtered
   ## out.
   step_zv(all_predictors()) %>%
   step_normalize(all_predictors(), -all_nominal())

knn_recipe


# 4.7 - Random Forest -----------------------------------------------------
randomforest_recipe <-
   recipe(
      formula = resposta ~ .,
      data = train_adult
   ) %>%
   step_modeimpute(all_nominal(), -all_outcomes()) %>%
   step_medianimpute(all_numeric(), -all_outcomes()) %>%
   ## For modeling, it is preferred to encode qualitative data as factors
   ## (instead of character).
   ## I have done this already...
   # step_string2factor(one_of(resposta)) %>%
   step_novel(all_nominal(), -all_outcomes()) %>%
   step_zv(all_predictors())

randomforest_recipe


# 5 - Definindo os Modelos ------------------------------------------------


# 5.1 - Modelos Lineares Generalizados ------------------------------------
glm_spec <-
   multinom_reg(
      mode = "classification",
      penalty = tune(),
      mixture = tune()
   ) %>%
   set_mode("classification") %>%
   set_engine("glmnet")

logistic_spec <-
   logistic_reg(
      mode = "classification",
      penalty = tune(),
      mixture = tune()
   ) %>%
   set_mode("classification") %>%
   set_engine("glmnet")

glm_spec
logistic_spec


# 5.2 - Arvore de Decisao -------------------------------------------------
arvore_spec <-
   decision_tree(
      mode = "classification",
      cost_complexity = tune(),
      min_n = tune(),
      tree_depth = tune()
   ) %>%
   set_mode("classification") %>%
   set_engine("rpart")

arvore_spec


# 5.3 - Boost Tree - XGBoost ----------------------------------------------
xgboost_spec <-
   boost_tree(
      mode = "classification",
      mtry = tune(),
      trees = tune(),
      min_n = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      loss_reduction = tune(),
      sample_size = tune()
   ) %>%
   set_mode("classification") %>%
   set_engine("xgboost", nthread = 8, verbose = TRUE)

xgboost_spec


# 5.4 - MARS --------------------------------------------------------------
earth_spec <-
   mars(
      mode = "classification",
      num_terms = tune(),
      prod_degree = tune(),
      prune_method = tune()
   ) %>%
   set_mode("classification") %>%
   set_engine("earth")

earth_spec


# 5.5 - Multilayer Perceptron ---------------------------------------------
neural_spec <-
   mlp(
      mode = "classification",
      hidden_units = tune(),
      penalty = tune(),
      dropout = tune(),
      epochs = tune(),
      activation = tune()
   ) %>%
   set_mode("classification") %>%
   set_engine("keras")

neural_spec


# 5.6 - K-Nearest Neighbor ------------------------------------------------
knn_spec <-
   nearest_neighbor(
      mode = "classification",
      neighbors = tune(),
      weight_func = tune()
   ) %>%
   set_mode("classification") %>%
   set_engine("kknn")

knn_spec


# 5.7 - Random Forest -----------------------------------------------------
randomforest_spec <-
   rand_forest(
      mode = "classification",
      mtry = tune(),
      min_n = tune(),
      trees = tune()
   ) %>%
   set_mode("classification") %>%
   set_engine("ranger", num.threads = 8, verbose = TRUE)

randomforest_spec


# 5.8 - Polynomial Support Vector Machines --------------------------------
polysvm_spec <-
   svm_poly(
      mode = "classification",
      cost = tune(),
      degree = tune(),
      scale_factor = tune()
   ) %>%
   set_mode("classification") %>%
   set_engine("kernlab")

polysvm_spec


# 5.9 - Radial Basis Function Support Vector Machines ---------------------
radialsvm_spec <-
   svm_rbf(
      mode = "classification",
      cost = tune(),
      rbf_sigma = tune(),
      margin = tune()
   ) %>%
   set_mode("classification") %>%
   set_engine("kernlab")

radialsvm_spec


# 6 - Workflows -----------------------------------------------------------


# 6.1 - Modelos Lineares Generalizados ------------------------------------
glm_workflow <-
   workflow() %>%
   add_recipe(glm_recipe) %>%
   add_model(glm_spec)

logistic_workflow <-
   workflow() %>%
   add_recipe(glm_recipe) %>%
   add_model(logistic_spec)


# 6.2 - Arvore de Decisao -------------------------------------------------
arvore_workflow <-
   workflow() %>%
   add_recipe(adult_recipe) %>%
   add_model(arvore_spec)


# 6.3 - Boost Tree - XGBoost ----------------------------------------------
xgboost_workflow <-
   workflow() %>%
   add_recipe(xgboost_recipe) %>%
   add_model(xgboost_spec)


# 6.4 - MARS --------------------------------------------------------------
earth_workflow <-
   workflow() %>%
   add_recipe(earth_recipe) %>%
   add_model(earth_spec)


# 6.5 - Multilayer Perceptron ---------------------------------------------
neural_workflow <-
   workflow() %>%
   add_recipe(adult_recipe) %>%
   add_model(neural_spec)


# 6.6 - K-Nearest Neighbor ------------------------------------------------
knn_workflow <-
   workflow() %>%
   add_recipe(knn_recipe) %>%
   add_model(knn_spec)


# 6.7 - Random Forest -----------------------------------------------------
randomforest_workflow <-
   workflow() %>%
   add_recipe(randomforest_recipe) %>%
   add_model(randomforest_spec)


# 6.8 - Polynomial Support Vector Machines --------------------------------
polysvm_workflow <-
   workflow() %>%
   add_recipe(adult_recipe) %>%
   add_model(polysvm_spec)


# 6.9 - Radial Basis Function Support Vector Machines ---------------------
radialsvm_workflow <-
   workflow() %>%
   add_recipe(adult_recipe) %>%
   add_model(radialsvm_spec)


# 7 - Tunagem de Hiperparametros ------------------------------------------


# 7.0 - Cross-Validation --------------------------------------------------
adult_resamples <-
   vfold_cv(
      train_adult,
      v = 10,
      strata = resposta
   )
adult_resamples


# 7.1 - Modelos Lineares Generalizados ------------------------------------
glm_grid <-
   tidyr::crossing(
      penalty = c(10^seq(-6, -1, length.out = 20), 0.000000115, 0.00000217, 0.000477),
      mixture = c(0.05, 0.2, 0.4, 0.6, 0.8, 0.833, 0.918, 0.975, 1)
   )

glm_tune <-
   tune_grid(
      object = glm_workflow,
      resamples = adult_resamples,
      grid = glm_grid,
      metrics = metric_set(roc_auc),
      control = control_grid(
         verbose = TRUE,
         allow_par = TRUE
      )
   )

logistic_tune <-
   tune_grid(
      object = logistic_workflow,
      resamples = adult_resamples,
      grid = glm_grid,
      metrics = metric_set(roc_auc),
      control = control_grid(
         verbose = TRUE,
         allow_par = TRUE
      )
   )


# 7.2 - Arvore de Decisao -------------------------------------------------
arvore_tune <-
   tune_grid(
      object = adult_ad_wf,
      resamples = adult_resamples,
      grid = 10,
      metrics = metric_set(roc_auc),
      control = control_grid(
         verbose = TRUE,
         allow_par = TRUE
      )
   )


# 7.3 - Boost Tree - XGBoost ----------------------------------------------
xgboost_tune <-
   tune_grid(
      object = xgboost_workflow,
      resamples = adult_resamples,
      grid = 100,
      metrics = metric_set(roc_auc),
      control = control_grid(
         verbose = TRUE,
         allow_par = TRUE
      )
   )


# 7.4 - MARS --------------------------------------------------------------
## MARS models can make predictions on many _sub_models_, meaning that we
## can evaluate many values of `num_terms` without much computational
## cost. A regular grid is used to exploit this property. The first term
## is only the intercept, so the grid is a sequence of even numbered
## values.
earth_grid <-
   tidyr::crossing(
      num_terms = 2 * (1:6),
      prod_degree = 1:2
   )

earth_tune <-
   tune_grid(
      object = earth_workflow,
      resamples = adult_resamples,
      grid = earth_grid,
      metrics = metric_set(roc_auc),
      control = control_grid(
         verbose = TRUE,
         allow_par = TRUE
      )
   )


# 7.5 - Multilayer Perceptron ---------------------------------------------
neural_tune <-
   tune_grid(
      object = adult_mp_wf,
      resamples = adult_resamples,
      grid = 10,
      metrics = metric_set(roc_auc),
      control = control_grid(
         verbose = TRUE,
         allow_par = TRUE
      )
   )


# 7.6 - K-Nearest Neighbor ------------------------------------------------
knn_tune <-
   tune_grid(
      object = knn_workflow,
      resamples = adult_resamples,
      grid = 10,
      metrics = metric_set(roc_auc),
      control = control_grid(
         verbose = TRUE,
         allow_par = TRUE
      )
   )


# 7.7 - Random Forest -----------------------------------------------------
randomforest_tune <-
   tune_grid(
      object = randomforest_workflow,
      resamples = adult_resamples,
      grid = 10,
      metrics = metric_set(roc_auc),
      control = control_grid(
         verbose = TRUE,
         allow_par = TRUE
      )
   )


# 7.8 - Polynomial Support Vector Machines --------------------------------
polysvm_tune <-
   tune_grid(
      object = adult_ps_wf,
      resamples = adult_resamples,
      grid = 5,
      metrics = metric_set(roc_auc),
      control = control_grid(
         verbose = TRUE,
         allow_par = TRUE
      )
   )


# 7.9 - Radial Basis Function Support Vector Machines ---------------------
radialsvm_tune <-
   tune_grid(
      object = adult_rs_wf,
      resamples = adult_resamples,
      grid = 4,
      metrics = metric_set(roc_auc),
      control = control_grid(
         verbose = TRUE,
         allow_par = TRUE
      )
   )


# 8 - Modelos Finais e Submissao ------------------------------------------
graphics.off()


# 8.0 - Olhando as Metricas de Desempenho ---------------------------------
collect_metrics(glm_tune) %>% arrange(desc(mean)) %>% print.data.frame()
collect_metrics(logistic_tune) %>% arrange(desc(mean)) %>% print.data.frame()
collect_metrics(arvore_tune) %>% arrange(desc(mean)) %>% print.data.frame()
collect_metrics(xgboost_tune) %>% arrange(desc(mean)) %>% print.data.frame()
collect_metrics(earth_tune) %>% arrange(desc(mean)) %>% print.data.frame()
collect_metrics(neural_tune) %>% arrange(desc(mean)) %>% print.data.frame()
collect_metrics(knn_tune) %>% arrange(desc(mean)) %>% print.data.frame()
collect_metrics(randomforest_tune) %>% arrange(desc(mean)) %>% print.data.frame()
collect_metrics(polysvm_tune) %>% arrange(desc(mean)) %>% print.data.frame()
collect_metrics(radialsvm_tune) %>% arrange(desc(mean)) %>% print.data.frame()


# collect_metrics(adult_rl_tune_grid) %>%
#    ggplot(aes(x = penalty, y = mean)) +
#    geom_point() +
#    geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err)) +
#    facet_wrap(~.metric, scales = "free") +
#    scale_x_log10()
#
# collect_metrics(adult_ad_tune_grid) %>%
#    ggplot(aes(x = cost_complexity, y = mean)) +
#    geom_point() +
#    geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err)) +
#    facet_wrap(~.metric, scales = "free") +
#    scale_x_log10()

Allocated_Memory <- paste(memory.size(), "Mb")


# 8.1 - Modelos Lineares Generalizados ------------------------------------
glm_best_params <-
   select_best(
      x = glm_tune,
      metric = "roc_auc"
   )

logistic_best_params <-
   select_best(
      x = logistica_tune,
      metric = "roc_auc"
   )

glm_best_params
logistic_best_params


# 8.2 - Arvore de Decisao -------------------------------------------------
arvore_best_params <-
   select_best(
      x = arvore_tune,
      metric = "roc_auc"
   )
arvore_best_params


# 8.3 - Boost Tree - XGBoost ----------------------------------------------
xgboost_best_params <-
   select_best(
      x = xgboost_tune,
      metric = "roc_auc"
   )
xgboost_best_params


# 8.4 - MARS --------------------------------------------------------------
earth_best_params <-
   select_best(
      x = earth_tune,
      metric = "roc_auc"
   )
earth_best_params


# 8.5 - Multilayer Perceptron ---------------------------------------------
neural_best_params <-
   select_best(
      x = neural_tune,
      metric = "roc_auc"
   )
neural_best_params


# 8.6 - K-Nearest Neighbor ------------------------------------------------
knn_best_params <-
   select_best(
      x = knn_tune,
      metric = "roc_auc"
   )
knn_best_params


# 8.7 - Random Forest -----------------------------------------------------
randomforest_best_params <-
   select_best(
      x = randomforest_tune,
      metric = "roc_auc"
   )
randomforest_best_params


# 8.8 - Polynomial Support Vector Machines --------------------------------
polysvm_best_params <-
   select_best(
      x = polysvm_tune,
      metric = "roc_auc"
   )
polysvm_best_params


# 8.9 - Radial Basis Function Support Vector Machines ---------------------
radialsvm_best_params <-
   select_best(
      x = radialsvm_tune,
      metric = "roc_auc"
   )
radialsvm_best_params


# 9 - Finalizando... ------------------------------------------------------


# 9.1 - Workflows de Finalizacao dos Modelos  -----------------------------
glm_workflow <-
   glm_workflow %>%
   finalize_workflow(glm_best_params)
glm_workflow

logistic_workflow <-
   logistic_workflow %>%
   finalize_workflow(logistic_best_params)
adult_rl_wf

arvore_workflow <-
   arvore_workflow %>%
   finalize_workflow(arvore_best_params)
arvore_workflow

xgboost_workflow <-
   xgboost_workflow %>%
   finalize_workflow(xgboost_best_params)
xgboost_workflow

earth_workflow <-
   earth_workflow %>%
   finalize_workflow(earth_best_params)
earth_workflow

neural_workflow <-
   neural_workflow %>%
   finalize_workflow(neural_best_params)
neural_workflow

knn_workflow <-
   knn_workflow %>%
   finalize_workflow(knn_best_params)
knn_workflow

randomforest_workflow <-
   randomforest_workflow %>%
   finalize_workflow(randomforest_best_params)
randomforest_workflow

polysvm_workflow <-
   polysvm_workflow %>%
   finalize_workflow(polysvm_best_params)
polysvm_workflow

radialsvm_workflow <-
   radialsvm_workflow %>%
   finalize_workflow(radialsvm_best_params)
radialsvm_workflow


# 9.2 - Aplicando os Melhores Modelos -------------------------------------
glm_last_fit <-
   last_fit(
      object = glm_workflow,
      split = split
   )
glm_last_fit

logistic_last_fit <-
   last_fit(
      object = logistic_workflow,
      split = split
   )
logistic_last_fit

arvore_last_fit <-
   last_fit(
      object = arvore_workflow,
      split = split
   )
arvore_last_fit

xgboost_last_fit <-
   last_fit(
      object = xgboost_workflow,
      split = split
   )
xgboost_last_fit

earth_last_fit <-
   last_fit(
      object = earth_workflow,
      split = split
   )
earth_last_fit

neural_last_fit <-
   last_fit(
      object = neural_workflow,
      split = split
   )
neural_last_fit

knn_last_fit <-
   last_fit(
      object = knn_workflow,
      split = split
   )
knn_last_fit

randomforest_last_fit <-
   last_fit(
      object = randomforest_workflow,
      split = split
   )
randomforest_last_fit

polysvm_last_fit <-
   last_fit(
      object = polysvm_workflow,
      split = split
   )
polysvm_last_fit

radialsvm_last_fit <-
   last_fit(
      object = radialsvm_workflow,
      split = split
   )
radialsvm_last_fit



# 9.3 - Metricas de Desempenho --------------------------------------------
collect_metrics(glm_last_fit) %>% print.data.frame()
collect_metrics(logistic_last_fit) %>% print.data.frame()
collect_metrics(arvore_last_fit) %>% print.data.frame()
collect_metrics(xgboost_last_fit) %>% print.data.frame()
collect_metrics(earth_last_fit) %>% print.data.frame()
collect_metrics(neural_last_fit) %>% print.data.frame()
collect_metrics(knn_last_fit) %>% print.data.frame()
collect_metrics(randomforest_last_fit) %>% print.data.frame()
collect_metrics(polysvm_last_fit) %>% print.data.frame()
collect_metrics(radialsvm_last_fit) %>% print.data.frame()


glm_preds <- collect_predictions(glm_last_fit)
glm_preds

logistic_preds <- collect_predictions(logistic_last_fit)
logistic_preds

arvore_preds <- collect_predictions(arvore_last_fit)
arvore_preds

xgboost_preds <- collect_predictions(xgboost_last_fit)
xgboost_preds

earth_preds <- collect_predictions(earth_last_fit)
earth_preds

neural_preds <- collect_predictions(neural_last_fit)
neural_preds

knn_preds <- collect_predictions(knn_last_fit)
knn_preds

randomforest_preds <- collect_predictions(randomforest_last_fit)
randomforest_preds

polysvm_preds <- collect_predictions(polysvm_last_fit)
polysvm_preds

radialsvm_preds <- collect_predictions(radialsvm_last_fit)
radialsvm_preds


# 9.4 - ROC para os Modelos -----------------------------------------------
glm_roc <- glm_preds %>%
   roc_curve(resposta, `.pred_<=50K`)
autoplot(glm_roc)

logistic_roc <- logistic_preds %>%
   roc_curve(resposta, `.pred_<=50K`)
autoplot(logistic_roc)

arvore_roc <- arvore_preds %>%
   roc_curve(resposta, `.pred_<=50K`)
autoplot(arvore_roc)

xgboost_roc <- xgboost_preds %>%
   roc_curve(resposta, `.pred_<=50K`)
autoplot(xgboost_roc)

earth_roc <- earth_preds %>%
   roc_curve(resposta, `.pred_<=50K`)
autoplot(earth_roc)

neural_roc <- neural_preds %>%
   roc_curve(resposta, `.pred_<=50K`)
autoplot(neural_roc)

knn_roc <- knn_preds %>%
   roc_curve(resposta, `.pred_<=50K`)
autoplot(knn_roc)

randomforest_roc <- randomforest_preds %>%
   roc_curve(resposta, `.pred_<=50K`)
autoplot(randomforest_roc)

polysvm_roc <- polysvm_preds %>%
   roc_curve(resposta, `.pred_<=50K`)
autoplot(polysvm_roc)

radialsvm_roc <- radialsvm_preds %>%
   roc_curve(resposta, `.pred_<=50K`)
autoplot(radialsvm_roc)


# 9.5 - Importancia de Variaveis nos Modelos ------------------------------
glm_last_fit_model <- glm_last_fit$.workflow[[1]]$fit$fit
glm_last_fit_model
vip(glm_last_fit_model)

logistic_last_fit_model <- logistic_last_fit$.workflow[[1]]$fit$fit
logistic_last_fit_model
vip(logistic_last_fit_model)

arvore_last_fit_model <- arvore_last_fit$.workflow[[1]]$fit$fit
arvore_last_fit_model
vip(arvore_last_fit_model)

xgboost_last_fit_model <- xgboost_last_fit$.workflow[[1]]$fit$fit
xgboost_last_fit_model
vip(xgboost_last_fit_model)

earth_last_fit_model <- earth_last_fit$.workflow[[1]]$fit$fit
earth_last_fit_model
vip(earth_last_fit_model)

neural_last_fit_model <- neural_last_fit$.workflow[[1]]$fit$fit
neural_last_fit_model
vip(neural_last_fit_model)

knn_last_fit_model <- knn_last_fit$.workflow[[1]]$fit$fit
knn_last_fit_model
vip(knn_last_fit_model)

randomforest_last_fit_model <- randomforest_last_fit$.workflow[[1]]$fit$fit
randomforest_last_fit_model
vip(randomforest_last_fit_model)

polysvm_last_fit_model <- polysvm_last_fit$.workflow[[1]]$fit$fit
polysvm_last_fit_model
vip(polysvm_last_fit_model)

radialsvm_last_fit_model <- radialsvm_last_fit$.workflow[[1]]$fit$fit
radialsvm_last_fit_model
vip(radialsvm_last_fit_model)



# 9.6 - Matrizes de Confusao ----------------------------------------------
glm_preds %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

logistic_preds %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

arvore_preds %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

xgboost_preds %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

earth_preds %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

neural_preds %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

knn_preds %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

randomforest_preds %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

polysvm_preds %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

radialsvm_preds %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)


# 9.7 - Finalmente Modelos Finais -----------------------------------------
glm_final <- glm_workflow %>% fit(adult2)
logistic_final <- logistic_workflow %>% fit(adult2)
arvore_final <- arvore_workflow %>% fit(adult2)
xgboost_final <- xgboost_workflow %>% fit(adult2)
earth_final <- earth_workflow %>% fit(adult2)
neural_final <- neural_workflow %>% fit(adult2)
knn_final <- knn_workflow %>% fit(adult2)
randomforest_final <- randomforest_workflow %>% fit(adult2)
polysvm_final <- polysvm_workflow %>% fit(adult2)
radialsvm_final <- radialsvm_workflow %>% fit(adult2)

adult_val_submissao <- adult_val %>%
   mutate(
      more_than_50k = predict(

         xgboost_final,

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
