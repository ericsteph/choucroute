
rm(list = ls())

library(tidyverse)
library(readxl)
library(zoo)
library(scales)
library(themeric)

data("pal_ustat")
source("01_FUN_figure.R", encoding = "UTF-8")

d <- readRDS("cubi esterni/swissPCI.rds")

# T.6 CONTR_m per posizione, da gennaio 2022

vociCONTR <- c("100_100", "1170_101", "1170_102", "1170_103",
               "100_3", "100_4004", "100_4070", "100_4050", "100_4090", 
               "100_4110",	"100_4112",	"100_5002", "100_7300",
               "100_7003", "100_7035", "100_7105", "100_7129", 
               "100_1307", "100_1002", "100_1014", "100_1074",
               "100_11002", "100_11129", "100_1179",
               "100_1198", "100_1294", "100_1360", "100_2017",
               "100_9046", "100_5101", "100_1530",
               "100_1544", "100_1284", "100_1448",
               "100_7200", "110_101", "110_102",
               "1819_118", "1819_119")

d2 <- d %>%
  group_by(Code) %>%
  filter(var.x %in% "IPC",
         freq.x %in% "month",
         year.x >= 2019,
         Code %in% vociCONTR
  ) %>%
  mutate(value1 = value,
         value2 = lag(value1, 12),
         value = ((value1 - value2) / value2) * 100,
         value = value * w2022 / 100) %>%
  ungroup()
  
  # select(freq.x, var.x, Code, COICOP, PosTxt_F, w2022, year.x, month.yr, value) %>%
  # filter(year.x >= 2021) %>%
  # select(-year.x) %>%
  # arrange(Code) %>%
  # pivot_wider(names_from = "month.yr")

subtitle = paste0("Base décembre ", base0, " = 100")

################################################################################
#
###  Blocco III
#
##### F.3 uso della variabile "Contr_yr" (contributo alla crescita annua: tx_yr * w22)
# - beni e servizi
# - importazione e produzione indigena
# - zoccolo dell'inflazione vs energia e carburanti
# - beni alimentari: pane, carne, patate,..

a0 <- "Contribution au renchérissement de l'IPC (en p.p.), "
subtitle0 <- "Base: IPC, taux de croissance annuel (en %)"

# Figura 3.1, contributo alla crescita dell'indice dei prezzi, secondo il tipo di prodotto
# in Svizzera, dal 2019
code.x <- c("110_101", "110_102")
title0 <- paste0(a0, "selon le type de produit", ", depuis janvier ", year0)

p1 <- ipc_line_faceting(d2, code.x,
                        y0 = 0, x0 = 2022 - 1/12, year0 = 2020, 
                        title0, subtitle0,
                        lim.y = c(-1.5, 3.5))


# Figura 2.2, evoluzione dell'indice dei prezzi, secondo la provenienza
# in Svizzera, dal 2019
code.x <- c("1819_118", "1819_119")
title0 <- paste0(a0, "selon le type de produit", ", depuis janvier ", year0)

p2 <- ipc_line_faceting(d2, code.x,
                        y0 = 0, x0 = 2022 - 1/12, year0 = 2020, 
                        title0, subtitle0,
                        lim.y = c(-1.5, 3.5))


# Figura 2.3, evoluzione dell'indice dei prezzi, secondo lo zoccolo, energia e prdtt. stag.
# in Svizzera, dal 2019
code.x <- c("1170_101", "1170_102", "1170_103")
title0 <- paste0(a0, "selon l'inflation sous-jacente", ", depuis janvier ", year0)

p3 <- ipc_line_faceting(d2, code.x,
                        y0 = 0, x0 = 2022 - 1/12, year0 = 2020, 
                        title0, subtitle0,
                        lim.y = c(-1.5, 3.5))


# Figura 2.4, evoluzione dell'indice dei prezzi, secondo alcuni beni energetici
# in Svizzera, dal 2019
code.x <- c("100_4070",	"100_4050",	"100_4090", "100_7105")
code0 <- "1170_102"

title0 <- paste0(a0, "selon la source d'énergie", ", depuis janvier ", year0)
subtitle0 <- "Base: Énergie et carburants, taux de croissance annuel (en %)"

p4 <- ipc_line_faceting(d2, code.x, code0 = code0,
                        y0 = 0, x0 = 2022 - 1/12, year0 = 2020, 
                        title0, subtitle0,
                        lim.y = c(-1, 2), k = 2)


# Figura 2.5, evoluzione dell'indice dei prezzi, secondo i gruppi principali
# in Svizzera, dal 2019
code.x <- c("100_1014",	"100_3", "100_11002",	"100_9046")
code0 <- "1170_103"

title0 <- paste0(a0, "selon certains postes de dépense", ", depuis janvier ", year0)
subtitle0 <- "Base: IPC, taux de croissance annuel (en %)"

p5 <- ipc_line_faceting(d2, code.x, code0 = code0,
                        y0 = 0, x0 = 2022 - 1/12, year0 = 2020, 
                        title0, subtitle0,
                        lim.y = c(-.06, .08), k = 2)



ggsave(file = "immagini/p3.1.svg", plot = p1, width = 10, height = 6.875)
ggsave(file = "immagini/p3.2.svg", plot = p2, width = 10, height = 6.875)
ggsave(file = "immagini/p3.3.svg", plot = p3, width = 10, height = 6.875)
ggsave(file = "immagini/p3.4.svg", plot = p4, width = 10, height = 6.875)
ggsave(file = "immagini/p3.5.svg", plot = p5, width = 10, height = 6.875)
