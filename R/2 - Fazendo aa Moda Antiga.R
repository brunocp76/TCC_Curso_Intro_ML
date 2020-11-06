cls <- function() cat("\f")


# 0.0 - Bibliotecas para a Análise ----------------------------------------
library(pROC)
library(rpart)
library(rpart.plot)
library(tidyverse)
# library(tidymodels)


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

adult2 %>% map_dbl(~mean(is.na(.x))) %>% scales::percent()

adult3 <- adult2 %>%
   filter(!is.na(workclass),
          !is.na(occupation),
          !is.na(native_country))

adult3 %>% map_dbl(~mean(is.na(.x))) %>% scales::percent()

cls()
adult %>% glimpse()
adult2 %>% glimpse()
adult3 %>% glimpse()


# 3.0 - Modelo de Arvore de Decisao ---------------------------------------
rtree_def <- rpart(
   resposta ~ . ,
   data = adult3
)

summary(rtree_def)

rpart.plot(x = rtree_def, digits = 3, cex = 0.7)

plotcp(rtree_def)

rsq.rpart(rtree_def)

graphics.off()


# 4.0 - Modelo de Regressao Logistica -------------------------------------


# 4.1 - Modelo Intercepto -------------------------------------------------
noglm_def <- glm(
   resposta ~ 1,
   family = binomial(link = logit),
   data = adult3
)

summary(noglm_def)


# 4.2 - Modelo Completo ---------------------------------------------------
fullglm_def <- glm(
   resposta ~ .,
   family = binomial(link = logit),
   data = adult3
)

summary(fullglm_def)


# 4.3 - Modelos Parciais --------------------------------------------------
backward_def <- step(
   object = fullglm_def,
   scope = list(lower = formula(noglm_def),
                upper = formula(fullglm_def)),
   direction = "backward",
   trace = 9
)

forward_def <- step(
   object = noglm_def,
   scope = list(lower = formula(noglm_def),
                upper = formula(fullglm_def)),
   direction = "forward",
   trace = 9
)

stepwise_def <- step(
   object = noglm_def,
   scope = list(lower = formula(noglm_def),
                upper = formula(fullglm_def)),
   direction = "both",
   trace = 9
)


# 4.4 - Analisando os Resultados ------------------------------------------
summary(backward_def)
summary(forward_def)
summary(stepwise_def)

formula(backward_def)
formula(forward_def)
formula(stepwise_def)

backward_def$anova
forward_def$anova
stepwise_def$anova


# 5.0 - Avaliando a Qualidade do Modelo com a ROC -------------------------

par(pty = 's')  # Grafico Quadrado

roc(response = adult3$resposta,
    predictor = stepwise_def$fitted.values,
    plot = TRUE,
    legacy.axes = TRUE,
    percent = TRUE,
    xlab = "False Positive Percentage",
    ylab = "True Positive Percentage",
    col = "#377eb8",
    lwd = 4,
    print.auc = TRUE
)
