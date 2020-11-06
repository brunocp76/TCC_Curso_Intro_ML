cls <- function() cat("\f")


# 0.0 - Bibliotecas para a Análise ----------------------------------------
library(tidyverse)
library(tidymodels)


# 1.0 - Importando os dados -----------------------------------------------
adult_val <- readRDS(file = "data-raw/adult_val.rds")

adult <- readRDS(file = "data-raw/adult.rds")

adult %>% glimpse()


# 2.0 - Limpeza dos dados -------------------------------------------------


# 2.1 - Criando fatores ordenados de dados categoricos --------------------
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
      ),
      resposta = factor(
         resposta,
         levels = c(
            '<=50K',
            '>50K'
         )
      )
   )

cls()
adult %>% glimpse()
adult2 %>% glimpse()


# 2.2 - Olhando como ficou a base de dados --------------------------------
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

cls()
adult2 %>% glimpse()
table(adult2$relationship, adult2$resposta)
table(adult2$marital_status, adult2$resposta)
table(adult2$occupation, adult2$resposta)
table(adult2$education, adult2$resposta)
table(adult2$sex, adult2$resposta)
table(adult2$workclass, adult2$resposta)
table(adult2$race, adult2$resposta)


# 3.0 - Modelagem ---------------------------------------------------------
cls()


# 3.1 - Separacao de Base de Treino e Base de Teste -----------------------
split <- adult2 %>%
   initial_split(strata = resposta, prop = 3/4)
split

train_adult <- training(split)
tests_adult <- testing(split)

# Budega de Modelo ----

# 3.5 - Modelo de Arvore de Decisao ---------------------------------------
adult_tree_model <- decision_tree(
   cost_complexity = 0.001,
   min_n = 5,
   tree_depth = tune()
) %>%
   set_engine("rpart") %>%
   set_mode("classification")


# 3.2 - Cross Validation por K-folds --------------------------------------
adult_resamples <- vfold_cv(
   data = train_adult,
   v = 5,
   strata = resposta
)
adult_resamples



# 3.3 - Pre-processamento -------------------------------------------------
adult2 %>% map_dbl(~mean(is.na(.x))) %>% scales::percent()

ls("package:recipes") %>%  .[str_starts(., "step_")]

rec_medianimpute_modeimpute_zv_dummy <- recipe(
   formula = resposta ~ .,
   data = train_adult
) %>%
   step_zv(all_predictors()) %>%
   step_medianimpute(all_numeric()) %>%
   step_modeimpute(all_nominal(), -resposta) %>%
   step_dummy(all_nominal())

rec_medianimpute_modeimpute_zv_dummy$var_info
rec_medianimpute_modeimpute_zv_dummy

# prep(rec_medianimpute_modeimpute_zv_dummy)
# prep(rec_medianimpute_modeimpute_zv_dummy) %>%
#    bake(train_adult)


# 3.4 - Modelo de Regressao Logistica -------------------------------------
adult_logi_model <- logistic_reg(
   # penalty = tune(),
   # mixture = 1
) %>%
   set_engine("glm") %>%
   set_mode("classification")

rl_workflow <-
   workflow() %>%
   add_recipe(rec_medianimpute_modeimpute_zv_dummy) %>%
   add_model(adult_logi_model)

rl_workflow

rl_fit <- rl_workflow %>%
   fit_resamples(adult_resamples,
                 metrics = metric_set(roc_auc, accuracy, sens, spec),
                 control = control_resamples(save_pred = TRUE)
   )








# 3.5 - Modelo de Arvore de Decisao ---------------------------------------
# adult_tree_model <- decision_tree(
#    cost_complexity = tune(),
#    min_n = 5,
#    tree_depth = tune()
# ) %>%
#    set_engine("rpart") %>%
#    set_mode("classification")


# 3.6 - Modelo de Random Forest -------------------------------------------
adult_rand_model <- rand_forest(
   mtry = tune(),
   trees = tune(),
   min_n = tune()
) %>%
   set_engine("ranger") %>%
   set_mode("classification")


# 4.0 - Workflows ---------------------------------------------------------


# 4.1 - Workflow da Regressao Logistica -----------------------------------
adult_logi_wf <- workflow() %>%
   add_recipe(rec_medianimpute_modeimpute_zv_dummy) %>%
   add_model(adult_logi_model)
adult_logi_wf


# 4.2 - Workflow de Arvores de Decisao ------------------------------------
adult_tree_wf <- workflow() %>%
   add_recipe(rec_medianimpute_modeimpute_zv_dummy) %>%
   add_model(adult_tree_model)
adult_tree_wf


# 4.3 - Workflow de Random Forest -----------------------------------------
adult_rand_wf <- workflow() %>%
   add_recipe(rec_medianimpute_modeimpute_zv_dummy) %>%
   add_model(adult_rand_model)
adult_rand_wf


# 5.0 - Processamento dos Modelos -----------------------------------------

# Tunagem de Hiperparametros ----------------------------------------------
adult_tune_grid <- tune_grid(
   model = adult_tree_model,
   resamples = adult_resamples,
   grid = 10,
   metrics = metric_set(roc_auc, accuracy, sens, spec),
   control = control_grid(verbose = TRUE, allow_par = TRUE)
)



# 5.1 - Processamento da Regressao Logistica ------------------------------
adult_logi_fit <- adult_logi_wf %>%
   fit_resamples(
      resamples = folds,
      metrics = metric_set(roc_auc, accuracy, sens, spec),
      control = control_resamples(save_pred = TRUE)
   )


# Tunagem...

adult_train_tune <- tune_grid(
   model = adult_tree_model,
   formula = resposta ~.,
   resamples = adult_resamples,
   grid = 100,
   control = control_grid(verbose = TRUE, allow_par = TRUE)
)
