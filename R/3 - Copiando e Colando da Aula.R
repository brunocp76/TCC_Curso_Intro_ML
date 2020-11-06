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
split <- adult %>%
   select(-id) %>%
   initial_split(strata = resposta, prop = 3/4)
split

train_adult <- training(split)
tests_adult <- testing(split)


# 3.2 - Definicao do Modelo -----------------------------------------------
adult_model <- decision_tree(
   # cost_complexity = 0.001,
   min_n = 5,
   tree_depth = tune()
) %>%
   set_engine("rpart") %>%
   set_mode("regression")


# 3.3 - Reamostragem com Cross-Validation ---------------------------------
adult_resamples <- vfold_cv(
   data = train_adult,
   v = 10,
   strata = resposta
)


# 3.4 - Tunagem dos Hiperparametros ---------------------------------------
adult_tune_grid <- tune_grid(
   adult_model,
   resposta ~ .,
   resamples = adult_resamples,
   grid = 10,
   metric_set(roc_auc, accuracy, sens, spec),
   control = control_grid(verbose = TRUE, allow_par = TRUE)
)
