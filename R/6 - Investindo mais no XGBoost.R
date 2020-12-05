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


# 5 - Definindo os Modelos ------------------------------------------------


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


# 6 - Workflows -----------------------------------------------------------


# 6.3 - Boost Tree - XGBoost ----------------------------------------------
xgboost_workflow <-
   workflow() %>%
   add_recipe(xgboost_recipe) %>%
   add_model(xgboost_spec)


# 7 - Tunagem de Hiperparametros ------------------------------------------


# 7.0 - Cross-Validation --------------------------------------------------
adult_resamples <-
   vfold_cv(
      train_adult,
      v = 10,
      strata = resposta
   )
adult_resamples


# 7.3 - Boost Tree - XGBoost ----------------------------------------------
xgboost_tune <-
   tune_grid(
      object = xgboost_workflow,
      resamples = adult_resamples,
      grid = 300,
      metrics = metric_set(roc_auc),
      control = control_grid(
         verbose = TRUE,
         allow_par = TRUE
      )
   )


# 8 - Modelos Finais e Submissao ------------------------------------------
graphics.off()


# 8.0 - Olhando as Metricas de Desempenho ---------------------------------
collect_metrics(xgboost_tune) %>% arrange(desc(mean)) %>% print.data.frame()


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


# 8.3 - Boost Tree - XGBoost ----------------------------------------------
xgboost_best_params <-
   select_best(
      x = xgboost_tune,
      metric = "roc_auc"
   )
xgboost_best_params


# 9 - Finalizando... ------------------------------------------------------


# 9.1 - Workflows de Finalizacao dos Modelos  -----------------------------
xgboost_workflow <-
   xgboost_workflow %>%
   finalize_workflow(xgboost_best_params)
xgboost_workflow


# 9.2 - Aplicando os Melhores Modelos -------------------------------------
xgboost_last_fit <-
   last_fit(
      object = xgboost_workflow,
      split = split
   )
xgboost_last_fit



# 9.3 - Metricas de Desempenho --------------------------------------------
collect_metrics(xgboost_last_fit) %>% print.data.frame()


xgboost_preds <- collect_predictions(xgboost_last_fit)
xgboost_preds


# 9.4 - ROC para os Modelos -----------------------------------------------
xgboost_roc <- xgboost_preds %>%
   roc_curve(resposta, `.pred_<=50K`)
autoplot(xgboost_roc)


# 9.5 - Importancia de Variaveis nos Modelos ------------------------------
xgboost_last_fit_model <- xgboost_last_fit$.workflow[[1]]$fit$fit
xgboost_last_fit_model
vip(xgboost_last_fit_model)


# 9.6 - Matrizes de Confusao ----------------------------------------------
xgboost_preds %>%
   mutate(
      resposta_class = factor(if_else(`.pred_<=50K` > 0.6, "<=50K", ">50K"))
   ) %>%
   conf_mat(resposta, resposta_class)


# 9.7 - Finalmente Modelos Finais -----------------------------------------
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


# 9.8 - Limpando os Arquivos Temporarios ----------------------------------
graphics.off()
cls()
Allocated_Memory <- paste(memory.size(), "Mb")
rm(list = ls())
