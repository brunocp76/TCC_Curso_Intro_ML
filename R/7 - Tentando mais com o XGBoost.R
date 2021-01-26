# 0 - Macro e Bibliotecas -------------------------------------------------
Allocated_Memory <- paste(memory.size(), "Mb")

cls <- function() cat("\f")

library(vip)
library(tidyverse)
library(tidymodels)

# Parameter for Parallel Processing
logical_cores <- parallel::detectCores(logical = TRUE)

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
# xgboost_spec1 <-
#    boost_tree(
#       mode = "classification",
#       mtry = 25,
#       trees = tune(),
#       min_n = 2,
#       tree_depth = 3,
#       learn_rate = tune(),
#       loss_reduction = 0.00001766597,
#       sample_size = 1
#    ) %>%
#    set_mode("classification") %>%
#    set_engine("xgboost", nthread = logical_cores, verbose = TRUE)
# xgboost_spec1

# xgboost_spec1 <-
#    boost_tree(
#       mode = "classification",
#       mtry = 20,
#       trees = tune(),
#       min_n = 2,
#       tree_depth = 4,
#       learn_rate = tune(),
#       loss_reduction = 0.166,
#       sample_size = 1
#    ) %>%
#    set_mode("classification") %>%
#    set_engine("xgboost", lambda = 0.17, nthread = logical_cores, verbose = TRUE)
# xgboost_spec1

xgboost_spec1 <-
   boost_tree(
      mode = "classification",
      mtry = 20,
      trees = tune(),
      min_n = 2,
      tree_depth = 4,
      learn_rate = tune(),
      loss_reduction = 0.0369,
      sample_size = 1
   ) %>%
   set_mode("classification") %>%
   set_engine("xgboost", lambda = 0.18, nthread = logical_cores, verbose = TRUE)
xgboost_spec1


xgboost_workflow1 <-
   workflow() %>%
   add_recipe(xgboost_recipe) %>%
   add_model(xgboost_spec1)


testing_grid1 <- expand.grid(
   learn_rate = seq(from = 0.015, to = 0.045, by = 0.001),
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
# xgboost_spec2 <-
#    boost_tree(
#       mode = "classification",
#       mtry = 25,
#       trees = best_round1$trees,
#       min_n = tune(),
#       tree_depth = tune(),
#       learn_rate = best_round1$learn_rate,
#       loss_reduction = 0.00001766597,
#       sample_size = 1
#    ) %>%
#    set_mode("classification") %>%
#    set_engine("xgboost", lambda = 0.17, nthread = logical_cores, verbose = TRUE)
# xgboost_spec2

# xgboost_spec2 <-
#    boost_tree(
#       mode = "classification",
#       mtry = 20,
#       trees = best_round1$trees,
#       min_n = tune(),
#       tree_depth = tune(),
#       learn_rate = best_round1$learn_rate,
#       loss_reduction = 0.166,
#       sample_size = 1
#    ) %>%
#    set_mode("classification") %>%
#    set_engine("xgboost", lambda = 0.17, nthread = logical_cores, verbose = TRUE)
# xgboost_spec2

xgboost_spec2 <-
   boost_tree(
      mode = "classification",
      mtry = 20,
      trees = best_round1$trees,
      min_n = tune(),
      tree_depth = tune(),
      learn_rate = best_round1$learn_rate,
      loss_reduction = 0.0369,
      sample_size = 1
   ) %>%
   set_mode("classification") %>%
   set_engine("xgboost", lambda = 0.18, nthread = logical_cores, verbose = TRUE)
xgboost_spec2


xgboost_workflow2 <-
   workflow() %>%
   add_recipe(xgboost_recipe) %>%
   add_model(xgboost_spec2)


testing_grid2 <- expand.grid(
   min_n = seq(from = 2, to = 9, by = 1),
   tree_depth = seq(from = 2, to = 7, by = 1)
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
# xgboost_spec3 <-
#    boost_tree(
#       mode = "classification",
#       mtry = 25,
#       trees = best_round1$trees,
#       min_n = best_round2$min_n,
#       tree_depth = best_round2$tree_depth,
#       learn_rate = best_round1$learn_rate,
#       loss_reduction = tune(),
#       sample_size = 1
#    ) %>%
#    set_mode("classification") %>%
#    set_engine("xgboost", lambda = tune("lambda"), nthread = logical_cores, verbose = TRUE)
# xgboost_spec3

xgboost_spec3 <-
   boost_tree(
      mode = "classification",
      mtry = 20,
      trees = best_round1$trees,
      min_n = best_round2$min_n,
      tree_depth = best_round2$tree_depth,
      learn_rate = best_round1$learn_rate,
      loss_reduction = tune(),
      sample_size = 1
   ) %>%
   set_mode("classification") %>%
   set_engine("xgboost", lambda = tune("lambda"), nthread = logical_cores, verbose = TRUE)
xgboost_spec3


xgboost_workflow3 <-
   workflow() %>%
   add_recipe(xgboost_recipe) %>%
   add_model(xgboost_spec3)


testing_grid3 <- expand.grid(
   loss_reduction = c(1e-5, 0.00001766597, seq(5e-5, 0.35, length.out = 20)),
   lambda = c(0, 0.1, 0.12, 0.13, 0.14, 0.15, 0.16, 0.17, 0.18, 0.2)
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
   set_engine("xgboost", lambda = best_round3$lambda, nthread = logical_cores, verbose = TRUE)
xgboost_spec4


xgboost_workflow4 <-
   workflow() %>%
   add_recipe(xgboost_recipe) %>%
   add_model(xgboost_spec4)


testing_grid4 <- expand.grid(
   mtry = seq(from = 10, to = 45, by = 5),
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
   set_engine("xgboost", lambda = best_round3$lambda, nthread = logical_cores, verbose = TRUE)
xgboost_spec5


xgboost_workflow5 <-
   workflow() %>%
   add_recipe(xgboost_recipe) %>%
   add_model(xgboost_spec5)



# tuning_round1 %>%
#    show_best(metric = "roc_auc", n = 2) %>%
#    select(trees, learn_rate) %>%
#    summarise(
#       minimo_trees = (tuning_round1 %>% show_best(metric = "roc_auc"))[1, 1]
#       - 1 * sd((tuning_round1 %>% show_best(metric = "roc_auc"))[, 1]),
#       maximo_trees = (tuning_round1 %>% show_best(metric = "roc_auc"))[1, 1]
#       + 1 * sd((tuning_round1 %>% show_best(metric = "roc_auc"))[, 1]),
#       minimo_learn_rate = (tuning_round1 %>% show_best(metric = "roc_auc"))[1, 2] - 1 * sd((tuning_round1 %>% show_best(metric = "roc_auc"))[, 2]),
#       maximo_learn_rate = (tuning_round1 %>% show_best(metric = "roc_auc"))[1, 2] + 1 * sd((tuning_round1 %>% show_best(metric = "roc_auc"))[, 2])
#    )


trees_mean <- collect_metrics(tuning_round1) %>% arrange(desc(mean)) %>% slice_head() %>% select(1) %>% as.double()

trees_sd <- collect_metrics(tuning_round1) %>% arrange(desc(mean)) %>% slice_head(n = 125) %>% select(1) %>% gather() %>% group_by(key) %>% summarise('sd' = sd(value, na.rm = TRUE)) %>% select(2) %>% as.double()


learn_rate_mean <- collect_metrics(tuning_round1) %>% arrange(desc(mean)) %>% slice_head() %>% select(2) %>% as.double()

learn_rate_sd <- collect_metrics(tuning_round1) %>% arrange(desc(mean)) %>% slice_head(n = 125) %>% select(2) %>% gather() %>% group_by(key) %>% summarise('sd' = sd(value, na.rm = TRUE)) %>% select(2) %>% as.double()


testing_grid5 <- expand.grid(
   learn_rate = sort(rnorm(n = 30, mean = learn_rate_mean, sd = learn_rate_sd)),
   trees = round(sort(rnorm(n = 30, mean = trees_mean, sd = trees_sd)))
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
   set_engine("xgboost", lambda = best_round3$lambda, nthread = logical_cores, verbose = TRUE)
xgboost_spec6


xgboost_workflow6 <-
   workflow() %>%
   add_recipe(xgboost_recipe) %>%
   add_model(xgboost_spec6)


min_n_mean <- collect_metrics(tuning_round2) %>% arrange(desc(mean)) %>% slice_head() %>% select(1) %>% as.double()

min_n_sd <- collect_metrics(tuning_round2) %>% arrange(desc(mean)) %>% slice_head(n = 125) %>% select(1) %>% gather() %>% group_by(key) %>% summarise('sd' = sd(value, na.rm = TRUE)) %>% select(2) %>% as.double()


tree_depth_mean <- collect_metrics(tuning_round2) %>% arrange(desc(mean)) %>% slice_head() %>% select(2) %>% as.double()

tree_depth_sd <- collect_metrics(tuning_round2) %>% arrange(desc(mean)) %>% slice_head(n = 125) %>% select(2) %>% gather() %>% group_by(key) %>% summarise('sd' = sd(value, na.rm = TRUE)) %>% select(2) %>% as.double()


testing_grid6 <- expand.grid(
   min_n = seq(from = 2, to = 7, by = 1),
   tree_depth = seq(from = 2, to = 7, by = 1)
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
   set_engine("xgboost", lambda = tune("lambda"), nthread = logical_cores, verbose = TRUE)
xgboost_spec7


xgboost_workflow7 <-
   workflow() %>%
   add_recipe(xgboost_recipe) %>%
   add_model(xgboost_spec7)


loss_reduction_mean <- collect_metrics(tuning_round3) %>% arrange(desc(mean)) %>% slice_head() %>% select(1) %>% as.double()

loss_reduction_sd <- collect_metrics(tuning_round3) %>% arrange(desc(mean)) %>% slice_head(n = 125) %>% select(1) %>% gather() %>% group_by(key) %>% summarise('sd' = sd(value, na.rm = TRUE)) %>% select(2) %>% as.double()


lambda_mean <- collect_metrics(tuning_round3) %>% arrange(desc(mean)) %>% slice_head() %>% select(2) %>% as.double()

lambda_sd <- collect_metrics(tuning_round3) %>% arrange(desc(mean)) %>% slice_head(n = 125) %>% select(2) %>% gather() %>% group_by(key) %>% summarise('sd' = sd(value, na.rm = TRUE)) %>% select(2) %>% as.double()


testing_grid7 <- expand.grid(
   loss_reduction = rnorm(n = 10, mean = loss_reduction_mean, sd = loss_reduction_sd),
   lambda = rnorm(n = 10, mean = lambda_mean, sd = lambda_sd)
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
   set_engine("xgboost", lambda = best_round7$lambda, nthread = logical_cores, verbose = TRUE)
xgboost_spec8


xgboost_workflow8 <-
   workflow() %>%
   add_recipe(xgboost_recipe) %>%
   add_model(xgboost_spec8)


mtry_mean <- collect_metrics(tuning_round4) %>% arrange(desc(mean)) %>% slice_head() %>% select(1) %>% as.double()

mtry_sd <- collect_metrics(tuning_round4) %>% arrange(desc(mean)) %>% slice_head(n = 125) %>% select(1) %>% gather() %>% group_by(key) %>% summarise('sd' = sd(value, na.rm = TRUE)) %>% select(2) %>% as.double()


sample_size_mean <- collect_metrics(tuning_round4) %>% arrange(desc(mean)) %>% slice_head() %>% select(2) %>% as.double()

sample_size_sd <- collect_metrics(tuning_round4) %>% arrange(desc(mean)) %>% slice_head(n = 125) %>% select(2) %>% gather() %>% group_by(key) %>% summarise('sd' = sd(value, na.rm = TRUE)) %>% select(2) %>% as.double()


testing_grid8 <- expand.grid(
   mtry = seq(from = 10, to = 45, by = 5),
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



# grid_max_entropy()
# grid_latin_hypercube()



# 6 - Olhando as Metricas de Desempenho -----------------------------------
collect_metrics(tuning_round1) %>% arrange(desc(mean)) %>% head(1) %>% print.data.frame()
collect_metrics(tuning_round2) %>% arrange(desc(mean)) %>% head(1) %>% print.data.frame()
collect_metrics(tuning_round3) %>% arrange(desc(mean)) %>% head(1) %>% print.data.frame()
collect_metrics(tuning_round4) %>% arrange(desc(mean)) %>% head(1) %>% print.data.frame()
collect_metrics(tuning_round5) %>% arrange(desc(mean)) %>% head(1) %>% print.data.frame()
collect_metrics(tuning_round6) %>% arrange(desc(mean)) %>% head(1) %>% print.data.frame()
collect_metrics(tuning_round7) %>% arrange(desc(mean)) %>% head(1) %>% print.data.frame()
collect_metrics(tuning_round8) %>% arrange(desc(mean)) %>% head(1) %>% print.data.frame()


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
xgboost_best_params1 <- select_best(x = tuning_round1, metric = "roc_auc")
xgboost_best_params2 <- select_best(x = tuning_round2, metric = "roc_auc")
xgboost_best_params3 <- select_best(x = tuning_round3, metric = "roc_auc")
xgboost_best_params4 <- select_best(x = tuning_round4, metric = "roc_auc")
xgboost_best_params5 <- select_best(x = tuning_round5, metric = "roc_auc")
xgboost_best_params6 <- select_best(x = tuning_round6, metric = "roc_auc")
xgboost_best_params7 <- select_best(x = tuning_round7, metric = "roc_auc")
xgboost_best_params8 <- select_best(x = tuning_round8, metric = "roc_auc")
xgboost_best_params1
xgboost_best_params2
xgboost_best_params3
xgboost_best_params4
xgboost_best_params5
xgboost_best_params6
xgboost_best_params7
xgboost_best_params8


# 7.2 - Workflows de Finalizacao ------------------------------------------
xgboost_workflow_final1 <- xgboost_workflow1 %>%
   finalize_workflow(xgboost_best_params1)
xgboost_workflow_final2 <- xgboost_workflow2 %>%
   finalize_workflow(xgboost_best_params2)
xgboost_workflow_final3 <- xgboost_workflow3 %>%
   finalize_workflow(xgboost_best_params3)
xgboost_workflow_final4 <- xgboost_workflow4 %>%
   finalize_workflow(xgboost_best_params4)
xgboost_workflow_final5 <- xgboost_workflow5 %>%
   finalize_workflow(xgboost_best_params5)
xgboost_workflow_final6 <- xgboost_workflow6 %>%
   finalize_workflow(xgboost_best_params6)
xgboost_workflow_final7 <- xgboost_workflow7 %>%
   finalize_workflow(xgboost_best_params7)
xgboost_workflow_final8 <- xgboost_workflow8 %>%
   finalize_workflow(xgboost_best_params8)
xgboost_workflow_final1
xgboost_workflow_final2
xgboost_workflow_final3
xgboost_workflow_final4
xgboost_workflow_final5
xgboost_workflow_final6
xgboost_workflow_final7
xgboost_workflow_final8


# 7.3 - Aplicando o Ultimo Modelo -----------------------------------------
xgboost_last_fit1 <- last_fit(object = xgboost_workflow_final1, split = split)
xgboost_last_fit2 <- last_fit(object = xgboost_workflow_final2, split = split)
xgboost_last_fit3 <- last_fit(object = xgboost_workflow_final3, split = split)
xgboost_last_fit4 <- last_fit(object = xgboost_workflow_final4, split = split)
xgboost_last_fit5 <- last_fit(object = xgboost_workflow_final5, split = split)
xgboost_last_fit6 <- last_fit(object = xgboost_workflow_final6, split = split)
xgboost_last_fit7 <- last_fit(object = xgboost_workflow_final7, split = split)
xgboost_last_fit8 <- last_fit(object = xgboost_workflow_final8, split = split)
xgboost_last_fit1
xgboost_last_fit2
xgboost_last_fit3
xgboost_last_fit4
xgboost_last_fit5
xgboost_last_fit6
xgboost_last_fit7
xgboost_last_fit8


# 8 - Finalizando... ------------------------------------------------------


# 8.1 - Metricas de Desempenho --------------------------------------------
collect_metrics(xgboost_last_fit1) %>% head() %>% print.data.frame()
collect_metrics(xgboost_last_fit2) %>% head() %>% print.data.frame()
collect_metrics(xgboost_last_fit3) %>% head() %>% print.data.frame()
collect_metrics(xgboost_last_fit4) %>% head() %>% print.data.frame()
collect_metrics(xgboost_last_fit5) %>% head() %>% print.data.frame()
collect_metrics(xgboost_last_fit6) %>% head() %>% print.data.frame()
collect_metrics(xgboost_last_fit7) %>% head() %>% print.data.frame()
collect_metrics(xgboost_last_fit8) %>% head() %>% print.data.frame()


xgboost_preds1 <- collect_predictions(xgboost_last_fit1)
xgboost_preds2 <- collect_predictions(xgboost_last_fit2)
xgboost_preds3 <- collect_predictions(xgboost_last_fit3)
xgboost_preds4 <- collect_predictions(xgboost_last_fit4)
xgboost_preds5 <- collect_predictions(xgboost_last_fit5)
xgboost_preds6 <- collect_predictions(xgboost_last_fit6)
xgboost_preds7 <- collect_predictions(xgboost_last_fit7)
xgboost_preds8 <- collect_predictions(xgboost_last_fit8)
xgboost_preds1
xgboost_preds2
xgboost_preds3
xgboost_preds4
xgboost_preds5
xgboost_preds6
xgboost_preds7
xgboost_preds8


# 8.2 - ROC para os Modelos -----------------------------------------------
xgboost_roc1 <- xgboost_preds1 %>% roc_curve(resposta, `.pred_<=50K`)
xgboost_roc2 <- xgboost_preds2 %>% roc_curve(resposta, `.pred_<=50K`)
xgboost_roc3 <- xgboost_preds3 %>% roc_curve(resposta, `.pred_<=50K`)
xgboost_roc4 <- xgboost_preds4 %>% roc_curve(resposta, `.pred_<=50K`)
xgboost_roc5 <- xgboost_preds5 %>% roc_curve(resposta, `.pred_<=50K`)
xgboost_roc6 <- xgboost_preds6 %>% roc_curve(resposta, `.pred_<=50K`)
xgboost_roc7 <- xgboost_preds7 %>% roc_curve(resposta, `.pred_<=50K`)
xgboost_roc8 <- xgboost_preds8 %>% roc_curve(resposta, `.pred_<=50K`)
autoplot(xgboost_roc1) + ggtitle("Curva ROC - Rodada 1")
autoplot(xgboost_roc2) + ggtitle("Curva ROC - Rodada 2")
autoplot(xgboost_roc3) + ggtitle("Curva ROC - Rodada 3")
autoplot(xgboost_roc4) + ggtitle("Curva ROC - Rodada 4")
autoplot(xgboost_roc5) + ggtitle("Curva ROC - Rodada 5")
autoplot(xgboost_roc6) + ggtitle("Curva ROC - Rodada 6")
autoplot(xgboost_roc7) + ggtitle("Curva ROC - Rodada 7")
autoplot(xgboost_roc8) + ggtitle("Curva ROC - Rodada 8")


# 8.3 - Importancia de Variaveis ------------------------------------------
xgboost_last_fit_model1 <- xgboost_last_fit1$.workflow[[1]]$fit$fit
xgboost_last_fit_model2 <- xgboost_last_fit2$.workflow[[1]]$fit$fit
xgboost_last_fit_model3 <- xgboost_last_fit3$.workflow[[1]]$fit$fit
xgboost_last_fit_model4 <- xgboost_last_fit4$.workflow[[1]]$fit$fit
xgboost_last_fit_model5 <- xgboost_last_fit5$.workflow[[1]]$fit$fit
xgboost_last_fit_model6 <- xgboost_last_fit6$.workflow[[1]]$fit$fit
xgboost_last_fit_model7 <- xgboost_last_fit7$.workflow[[1]]$fit$fit
xgboost_last_fit_model8 <- xgboost_last_fit8$.workflow[[1]]$fit$fit
xgboost_last_fit_model1
xgboost_last_fit_model2
xgboost_last_fit_model3
xgboost_last_fit_model4
xgboost_last_fit_model5
xgboost_last_fit_model6
xgboost_last_fit_model7
xgboost_last_fit_model8
vip(xgboost_last_fit_model1)
vip(xgboost_last_fit_model2)
vip(xgboost_last_fit_model3)
vip(xgboost_last_fit_model4)
vip(xgboost_last_fit_model5)
vip(xgboost_last_fit_model6)
vip(xgboost_last_fit_model7)
vip(xgboost_last_fit_model8)


# 8.4 - Matrizes de Confusao ----------------------------------------------
xgboost_preds1 %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

xgboost_preds2 %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

xgboost_preds3 %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

xgboost_preds4 %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

xgboost_preds5 %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

xgboost_preds6 %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

xgboost_preds7 %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

xgboost_preds8 %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)


# 8.5 - Decidindo qual eh o melhor modelo... ------------------------------
cls()

collect_metrics(xgboost_last_fit1) %>% select(mean, n, std_err, everything(), -c('.estimator', '.config')) %>%  head(1) %>% print.data.frame()
collect_metrics(xgboost_last_fit2) %>% select(mean, n, std_err, everything(), -c('.estimator', '.config')) %>%  head(1) %>% print.data.frame()
collect_metrics(xgboost_last_fit3) %>% select(mean, n, std_err, everything(), -c('.estimator', '.config')) %>%  head(1) %>% print.data.frame()
collect_metrics(xgboost_last_fit4) %>% select(mean, n, std_err, everything(), -c('.estimator', '.config')) %>%  head(1) %>% print.data.frame()
collect_metrics(xgboost_last_fit5) %>% select(mean, n, std_err, everything(), -c('.estimator', '.config')) %>%  head(1) %>% print.data.frame()
collect_metrics(xgboost_last_fit6) %>% select(mean, n, std_err, everything(), -c('.estimator', '.config')) %>%  head(1) %>% print.data.frame()
collect_metrics(xgboost_last_fit7) %>% select(mean, n, std_err, everything(), -c('.estimator', '.config')) %>%  head(1) %>% print.data.frame()
collect_metrics(xgboost_last_fit8) %>% select(mean, n, std_err, everything(), -c('.estimator', '.config')) %>%  head(1) %>% print.data.frame()

xgboost_preds1 %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

xgboost_preds2 %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

xgboost_preds3 %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

xgboost_preds4 %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

xgboost_preds5 %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

xgboost_preds6 %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

xgboost_preds7 %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)

xgboost_preds8 %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)


# 8.6 - Base para o Kaggle ------------------------------------------------
xgboost_final1 <- xgboost_workflow_final1 %>% fit(adult2)
xgboost_final2 <- xgboost_workflow_final2 %>% fit(adult2)
xgboost_final3 <- xgboost_workflow_final3 %>% fit(adult2)
xgboost_final4 <- xgboost_workflow_final4 %>% fit(adult2)
xgboost_final5 <- xgboost_workflow_final5 %>% fit(adult2)
xgboost_final6 <- xgboost_workflow_final6 %>% fit(adult2)
xgboost_final7 <- xgboost_workflow_final7 %>% fit(adult2)
xgboost_final8 <- xgboost_workflow_final8 %>% fit(adult2)

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
