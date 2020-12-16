# 0 - Macro e Bibliotecas -------------------------------------------------
Allocated_Memory <- paste(memory.size(), "Mb")

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

questionr::freq.na(adult)

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


# 3 - Divisoes na Base ----------------------------------------------------


# 3.1 - Bases de Treino e Validacao -----------------------------------------
split <- adult2 %>%
   mutate(chave = str_c(resposta, '_', marital_status, '_', relationship)) %>%
   initial_split(
      strata = chave,
      prop = 3/4
   )
split

train_adult <- training(split) %>% select(-chave)
tests_adult <- testing(split) %>% select(-chave)

train_adult %>% glimpse()
tests_adult %>% glimpse()


# 3.2 - Cross-Validation --------------------------------------------------
adult_resamples <-
   vfold_cv(
      train_adult,
      v = 10,
      strata = resposta
   )
adult_resamples


# 4 - Sai uma Receita de XGBoost no Capricho... ---------------------------
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


# 5 - Melhores Hiperparametros --------------------------------------------


# 5.1 - trees() e learn_rate() --------------------------------------------
xgboost_spec1 <-
   boost_tree(
      mode = "classification",
      mtry = 25,
      trees = tune(),
      min_n = 2,
      tree_depth = 3,
      learn_rate = tune(),
      loss_reduction = 0.00001766597,
      sample_size = 1
   ) %>%
   set_mode("classification") %>%
   set_engine("xgboost", nthread = 8, verbose = TRUE)
xgboost_spec1


xgboost_workflow1 <-
   workflow() %>%
   add_recipe(xgboost_recipe) %>%
   add_model(xgboost_spec1)


testing_grid1 <- expand.grid(
   learn_rate = seq(from = 0.01, to = 0.045, by = 0.001),
   trees = c(1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000)
)
testing_grid1


tuning_round1 <- xgboost_workflow1 %>%
   tune_grid(
      resamples = adult_resamples,
      grid = testing_grid1,
      control = control_grid(save_pred = TRUE, verbose = TRUE, allow_par = TRUE),
      metrics = metric_set(roc_auc)
   )


autoplot(tuning_round1) + ggtitle("Round 1 - trees() e learn_rate()")
tuning_round1 %>% show_best(metric = "roc_auc", n = 10) %>% print.data.frame()
collect_metrics(tuning_round1) %>% arrange(desc(mean)) %>% print.data.frame()
best_round1 <- tuning_round1 %>% select_best(metric = "roc_auc")
best_round1


# 5.2 - min_n() e tree_depth() --------------------------------------------
xgboost_spec2 <-
   boost_tree(
      mode = "classification",
      mtry = 25,
      trees = best_round1$trees,
      min_n = tune(),
      tree_depth = tune(),
      learn_rate = best_round1$learn_rate,
      loss_reduction = 0.00001766597,
      sample_size = 1
   ) %>%
   set_mode("classification") %>%
   set_engine("xgboost", nthread = 8, verbose = TRUE)
xgboost_spec2


xgboost_workflow2 <-
   workflow() %>%
   add_recipe(xgboost_recipe) %>%
   add_model(xgboost_spec2)


testing_grid2 <- expand.grid(
   min_n = seq(from = 2, to = 9, by = 1),
   tree_depth = seq(from = 2, to = 12, by = 1)
)
testing_grid2


tuning_round2 <- xgboost_workflow2 %>%
   tune_grid(
      resamples = adult_resamples,
      grid = testing_grid2,
      control = control_grid(save_pred = TRUE, verbose = TRUE, allow_par = TRUE),
      metrics = metric_set(roc_auc)
   )


autoplot(tuning_round2) + ggtitle("Round 2 - min_n() e tree_depth()")
tuning_round2 %>% show_best(metric = "roc_auc", n = 10) %>% print.data.frame()
collect_metrics(tuning_round2) %>% arrange(desc(mean)) %>% print.data.frame()
best_round2 <- tuning_round2 %>% select_best(metric = "roc_auc")
best_round1
best_round2


# 5.3 - loss_reduction() e lambda() ---------------------------------------
xgboost_spec3 <-
   boost_tree(
      mode = "classification",
      mtry = 25,
      trees = best_round1$trees,
      min_n = best_round2$min_n,
      tree_depth = best_round2$tree_depth,
      learn_rate = best_round1$learn_rate,
      loss_reduction = tune(),
      sample_size = 1
   ) %>%
   set_mode("classification") %>%
   set_engine("xgboost", lambda = tune("lambda"), nthread = 8, verbose = TRUE)
xgboost_spec3


xgboost_workflow3 <-
   workflow() %>%
   add_recipe(xgboost_recipe) %>%
   add_model(xgboost_spec3)


testing_grid3 <- expand.grid(
   loss_reduction = c(1e-5, 0.00001766597, seq(5e-5, 0.35, length.out = 20)),
   lambda = c(0, 0.1, 0.12, 0.15, 0.17)
)
testing_grid3


tuning_round3 <- xgboost_workflow3 %>%
   tune_grid(
      resamples = adult_resamples,
      grid = testing_grid3,
      control = control_grid(save_pred = TRUE, verbose = TRUE, allow_par = TRUE),
      metrics = metric_set(roc_auc)
   )


autoplot(tuning_round3) + ggtitle("Round 3 - loss_reduction() e lambda()")
tuning_round3 %>% show_best(metric = "roc_auc", n = 10) %>% print.data.frame()
collect_metrics(tuning_round3) %>% arrange(desc(mean)) %>% print.data.frame()
best_round3 <- tuning_round3 %>% select_best(metric = "roc_auc")
best_round1
best_round2
best_round3


# 5.4 - mtry() e sample_size() --------------------------------------------
xgboost_spec4 <-
   boost_tree(
      mode = "classification",
      mtry = tune(),
      trees = best_round1$trees,
      min_n = best_round2$min_n,
      tree_depth = best_round2$tree_depth,
      learn_rate = best_round1$learn_rate,
      loss_reduction = best_round3$loss_reduction,
      sample_size = tune()
   ) %>%
   set_mode("classification") %>%
   set_engine("xgboost", lambda = best_round3$lambda, nthread = 8, verbose = TRUE)
xgboost_spec4


xgboost_workflow4 <-
   workflow() %>%
   add_recipe(xgboost_recipe) %>%
   add_model(xgboost_spec4)


testing_grid4 <- expand.grid(
   mtry = seq(from = 5, to = 45, by = 5),
   sample_size = seq(from = 0.6, to = 1, by = 0.02)
)
testing_grid4


tuning_round4 <- xgboost_workflow4 %>%
   tune_grid(
      resamples = adult_resamples,
      grid = testing_grid4,
      control = control_grid(save_pred = TRUE, verbose = TRUE, allow_par = TRUE),
      metrics = metric_set(roc_auc)
   )


autoplot(tuning_round4) + ggtitle("Round 4 - mtry() e sample_size()")
tuning_round4 %>% show_best(metric = "roc_auc", n = 10) %>% print.data.frame()
collect_metrics(tuning_round4) %>% arrange(desc(mean)) %>% print.data.frame()
best_round4 <- tuning_round4 %>% select_best(metric = "roc_auc")
best_round1
best_round2
best_round3
best_round4


# 5.5 - trees() e learn_rate() --------------------------------------------
xgboost_spec5 <-
   boost_tree(
      mode = "classification",
      mtry = best_round4$mtry,
      trees = tune(),
      min_n = best_round2$min_n,
      tree_depth = best_round2$tree_depth,
      learn_rate = tune(),
      loss_reduction = best_round3$loss_reduction,
      sample_size = best_round4$sample_size
   ) %>%
   set_mode("classification") %>%
   set_engine("xgboost", lambda = best_round3$lambda, nthread = 8, verbose = TRUE)
xgboost_spec5


xgboost_workflow5 <-
   workflow() %>%
   add_recipe(xgboost_recipe) %>%
   add_model(xgboost_spec5)



tuning_round1 %>%
   show_best(metric = "roc_auc", n = 2) %>%
   select(trees, learn_rate) %>%
   summarise(
      minimo_trees = (tuning_round1 %>% show_best(metric = "roc_auc"))[1, 1]
         - 1 * sd((tuning_round1 %>% show_best(metric = "roc_auc"))[, 1]),
      maximo_trees = (tuning_round1 %>% show_best(metric = "roc_auc"))[1, 1]
         + 1 * sd((tuning_round1 %>% show_best(metric = "roc_auc"))[, 1]),
      minimo_learn_rate = (tuning_round1 %>% show_best(metric = "roc_auc"))[1, 2] - 1 * sd((tuning_round1 %>% show_best(metric = "roc_auc"))[, 2]),
      maximo_learn_rate = (tuning_round1 %>% show_best(metric = "roc_auc"))[1, 2] + 1 * sd((tuning_round1 %>% show_best(metric = "roc_auc"))[, 2])
   )


testing_grid5 <- expand.grid(
   learn_rate = seq(from = 0.01, to = 0.04, by = 0.001),
   trees = c(1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000)
)
testing_grid5


tuning_round5 <- xgboost_workflow5 %>%
   tune_grid(
      resamples = adult_resamples,
      grid = testing_grid5,
      control = control_grid(save_pred = TRUE, verbose = TRUE, allow_par = TRUE),
      metrics = metric_set(roc_auc)
   )


autoplot(tuning_round5) + ggtitle("Round 5 - trees() e learn_rate()")
tuning_round5 %>% show_best(metric = "roc_auc", n = 10) %>% print.data.frame()
collect_metrics(tuning_round5) %>% arrange(desc(mean)) %>% print.data.frame()
best_round5 <- tuning_round5 %>% select_best(metric = "roc_auc")
best_round1
best_round2
best_round3
best_round4
best_round5


# 5.6 - min_n() e tree_depth() --------------------------------------------
xgboost_spec6 <-
   boost_tree(
      mode = "classification",
      mtry = best_round4$mtry,
      trees = best_round5$trees,
      min_n = tune(),
      tree_depth = tune(),
      learn_rate = best_round5$learn_rate,
      loss_reduction = best_round3$loss_reduction,
      sample_size = best_round4$sample_size
   ) %>%
   set_mode("classification") %>%
   set_engine("xgboost", lambda = best_round3$lambda, nthread = 8, verbose = TRUE)
xgboost_spec6


xgboost_workflow6 <-
   workflow() %>%
   add_recipe(xgboost_recipe) %>%
   add_model(xgboost_spec6)


testing_grid6 <- expand.grid(
   min_n = seq(from = 2, to = 7, by = 1),
   tree_depth = seq(from = 2, to = 12, by = 1)
)
testing_grid6


tuning_round6 <- xgboost_workflow6 %>%
   tune_grid(
      resamples = adult_resamples,
      grid = testing_grid6,
      control = control_grid(save_pred = TRUE, verbose = TRUE, allow_par = TRUE),
      metrics = metric_set(roc_auc)
   )


autoplot(tuning_round6) + ggtitle("Round 6 - min_n() e tree_depth()")
tuning_round6 %>% show_best(metric = "roc_auc", n = 10) %>% print.data.frame()
collect_metrics(tuning_round6) %>% arrange(desc(mean)) %>% print.data.frame()
best_round6 <- tuning_round6 %>% select_best(metric = "roc_auc")
best_round1
best_round2
best_round3
best_round4
best_round5
best_round6


# 5.7 - loss_reduction() e lambda() ---------------------------------------
xgboost_spec7 <-
   boost_tree(
      mode = "classification",
      mtry = best_round4$mtry,
      trees = best_round5$trees,
      min_n = best_round6$min_n,
      tree_depth = best_round6$tree_depth,
      learn_rate = best_round5$learn_rate,
      loss_reduction = tune(),
      sample_size = best_round4$sample_size
   ) %>%
   set_mode("classification") %>%
   set_engine("xgboost", lambda = tune("lambda"), nthread = 8, verbose = TRUE)
xgboost_spec7


xgboost_workflow7 <-
   workflow() %>%
   add_recipe(xgboost_recipe) %>%
   add_model(xgboost_spec7)


testing_grid7 <- expand.grid(
   loss_reduction = c(1e-5, 0.00001766597, seq(5e-5, 0.35, length.out = 20)),
   lambda = c(0, 0.1, 0.12, 0.15, 0.17)
)
testing_grid7


tuning_round7 <- xgboost_workflow7 %>%
   tune_grid(
      resamples = adult_resamples,
      grid = testing_grid7,
      control = control_grid(save_pred = TRUE, verbose = TRUE, allow_par = TRUE),
      metrics = metric_set(roc_auc)
   )


autoplot(tuning_round7) + ggtitle("Round 7 - loss_reduction() e lambda()")
tuning_round7 %>% show_best(metric = "roc_auc", n = 10) %>% print.data.frame()
collect_metrics(tuning_round7) %>% arrange(desc(mean)) %>% print.data.frame()
best_round7 <- tuning_round7 %>% select_best(metric = "roc_auc")
best_round1
best_round2
best_round3
best_round4
best_round5
best_round6
best_round7


# 5.8 - mtry() e sample_size() --------------------------------------------
xgboost_spec8 <-
   boost_tree(
      mode = "classification",
      mtry = tune(),
      trees = best_round5$trees,
      min_n = best_round6$min_n,
      tree_depth = best_round6$tree_depth,
      learn_rate = best_round5$learn_rate,
      loss_reduction = best_round7$loss_reduction,
      sample_size = tune()
   ) %>%
   set_mode("classification") %>%
   set_engine("xgboost", lambda = best_round7$lambda, nthread = 8, verbose = TRUE)
xgboost_spec8


xgboost_workflow8 <-
   workflow() %>%
   add_recipe(xgboost_recipe) %>%
   add_model(xgboost_spec8)


testing_grid8 <- expand.grid(
   mtry = seq(from = 5, to = 45, by = 5),
   sample_size = seq(from = 0.6, to = 1, by = 0.02)
)
testing_grid8


tuning_round8 <- xgboost_workflow8 %>%
   tune_grid(
      resamples = adult_resamples,
      grid = testing_grid8,
      control = control_grid(save_pred = TRUE, verbose = TRUE, allow_par = TRUE),
      metrics = metric_set(roc_auc)
   )


autoplot(tuning_round8) + ggtitle("Round 8 - mtry() e sample_size()")
tuning_round8 %>% show_best(metric = "roc_auc", n = 10) %>% print.data.frame()
collect_metrics(tuning_round8) %>% arrange(desc(mean)) %>% print.data.frame()
best_round8 <- tuning_round8 %>% select_best(metric = "roc_auc")
best_round1
best_round2
best_round3
best_round4
best_round5
best_round6
best_round7
best_round8



grid_max_entropy()
grid_latin_hypercube()



# 6 - Olhando as Metricas de Desempenho -----------------------------------
graphics.off()


collect_metrics(tuning_round8) %>% arrange(desc(mean)) %>% print.data.frame()


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


# 7 - Finalizando ---------------------------------------------------------


# 7.1 - Melhores Parametros de Desempenho ---------------------------------
xgboost_best_params <-
   select_best(
      x = tuning_round8,
      metric = "roc_auc"
   )
xgboost_best_params


# 7.2 - Workflows de Finalizacao ------------------------------------------
xgboost_workflow <-
   xgboost_workflow8 %>%
   finalize_workflow(xgboost_best_params)
xgboost_workflow


# 7.3 - Aplicando o Ultimo Modelo -----------------------------------------
xgboost_last_fit <-
   last_fit(
      object = xgboost_workflow,
      split = split
   )
xgboost_last_fit


# 8 - Finalizando... ------------------------------------------------------


# 8.1 - Metricas de Desempenho --------------------------------------------
collect_metrics(xgboost_last_fit) %>% print.data.frame()


xgboost_preds <- collect_predictions(xgboost_last_fit)
xgboost_preds


# 8.2 - ROC para os Modelos -----------------------------------------------
xgboost_roc <- xgboost_preds %>%
   roc_curve(resposta, `.pred_<=50K`)
autoplot(xgboost_roc) + ggtitle("Curva ROC")


# 8.3 - Importancia de Variaveis ------------------------------------------
xgboost_last_fit_model <- xgboost_last_fit$.workflow[[1]]$fit$fit
xgboost_last_fit_model
vip(xgboost_last_fit_model)


# 8.4 - Matrizes de Confusao ----------------------------------------------
xgboost_preds %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)


# 8.5 - Base para o Kaggle ------------------------------------------------
xgboost_final <- xgboost_workflow %>% fit(adult2)

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


# 9 - Limpando os Temporarios ---------------------------------------------
graphics.off()
cls()
Allocated_Memory <- paste(memory.size(), "Mb")
rm(list = ls())
