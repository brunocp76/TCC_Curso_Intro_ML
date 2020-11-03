cls <- function() cat("\f")


# Bibliotecas para a Análise ----------------------------------------------
library(SmartEDA)
library(tidyverse)
library(Information)
library(DataExplorer)


# Importando os dados... --------------------------------------------------
adult_val <- readRDS(file = "data-raw/adult_val.rds")

adult <- readRDS(file = "data-raw/adult.rds")


# Primeiras Análises ------------------------------------------------------
adult %>% skimr::skim()

adult_val %>% glimpse()

adult %>% glimpse()

adult %>% count(resposta)


# Análises Exploratórias --------------------------------------------------
SmartEDA::ExpData(
   data = adult,
   type = 1
)

SmartEDA::ExpData(
   data = adult,
   type = 2
)


SmartEDA::ExpNumStat(
   data = adult,
   by = "GA",
   gp = "resposta",
   Qnt = NULL,
   MesofShape = 2,
   Outlier = TRUE,
   round = 2
)

SmartEDA::ExpNumViz(
   data = adult,
   target = "resposta",
   type = 1,
   # nlim = NULL,
   fname = NULL,
   col = c("blue", "red"),
   Page = c(3, 2),
   sample = NULL
)

SmartEDA::ExpCTable(
   data = adult,
   Target = "resposta",
   margin = 1,
   clim = 15,
   # nlim = NULL,
   round = 2,
   bin = NULL,
   per = TRUE
)

SmartEDA::ExpCatStat(
   data = adult,
   Target = "resposta",
   result = "IV",
   clim = 20,
   # nlim = NULL,
   bins = 20,
   plot = TRUE,
   top = 30,
   Round = 3
)


# Análises um pouco mais conclusivas --------------------------------------
cls()

adult %>%
   mutate(
      resp_num = ifelse(test = resposta == "<=50K",
                        yes = 0L,
                        no = 1L)
   ) %>%
   select(-id, -resposta) %>%
   Information::create_infotables(
      y = "resp_num",
      bins = 20,
      parallel = TRUE
   )

SmartEDA::ExpCatStat(
   data = adult,
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
table(adult$relationship, adult$resposta)
table(adult$marital_status, adult$resposta)
table(adult$occupation, adult$resposta)
table(adult$education, adult$resposta)
table(adult$sex, adult$resposta)
table(adult$workclass, adult$resposta)
table(adult$race, adult$resposta)

