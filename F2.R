
rm(list = ls())

library(tidyverse)
library(readxl)
library(zoo)
library(scales)
library(themeric)

data("pal_ustat")
source("01_FUN_figure.R", encoding = "UTF-8")

d <- readRDS("cubi esterni/swissPCI.rds")

################################################################################
#
###  Blocco II
#
##### F.2 confronto tra:
# - beni e servizi
# - importazione e produzione indigena
# - zoccolo dell'inflazione vs energia e carburanti


# Le prime figure si riferiscono al Totale (Code = "100_100"),

a0 <- "Taux de croissance de l'IPC (en %), "
title0 <- paste0(a0, "selon le type de produit", ", depuis janvier ", year0)

subtitle0 <- "Base: décembre 2020 = 100"

year0 <- 2019

# Figura 2.1, evoluzione dell'indice dei prezzi, secondo il tipo di prodotto
# in Svizzera, dal 2019
code.x <- c("110_101", "110_102")
title0 <- paste0(a0, "selon le type de produits", ", depuis janvier ", year0)

p1 <- ipc_line_faceting_var(d, code.x, year0 = 2019, title0, subtitle0, lim.y = c(-2.5, 7.5))


# Figura 2.2, evoluzione dell'indice dei prezzi, secondo la provenienza
# in Svizzera, dal 2019
code.x <- c("1819_118", "1819_119")
title0 <- paste0(a0, "selon la provenance des produits", ", depuis janvier ", year0)

p2 <- ipc_line_faceting_var(d, code.x, year0 = 2019, title0, subtitle0, lim.y = c(-5, 10))


# Figura 2.3, evoluzione dell'indice dei prezzi, secondo lo zoccolo, energia e prdtt. stag.
# in Svizzera, dal 2019
code.x <- c("1170_101", "1170_102", "1170_103")
title0 <- paste0(a0, "inflation sous-jacente et autres groupes", ", depuis janvier ", year0)

p3 <- ipc_line_faceting_var(d, code.x, year0 = 2019, title0, subtitle0, lim.y = c(-15, 30))


# Figura 2.4, evoluzione dell'indice dei prezzi, secondo alcuni beni energetici
# in Svizzera, dal 2019
code.x <- c("100_4070",	"100_4050",	"100_4090", "100_4110",	"100_4112",	"100_7105")
code0 <- "1170_102"
title0 <- paste0("Taux de croissance des prix de l'énergie (en %), depuis janvier ", year0)

p4 <- ipc_line_faceting_var(d, code.x, code0 = "1170_102", year0 = 2019, title0, subtitle0, lim.y = c(-50, 100), k = 2)


# Figura 2.5, evoluzione dell'indice dei prezzi, secondo i gruppi principali
# in Svizzera, dal 2019
code.x <- c("100_1",	"100_2",	"100_3", "100_4",	"100_5",
            "100_6",	"100_7",	"100_8", "100_9", "100_10",
            "100_11", "100_12")
title0 <- paste0(a0, "selon les groupes principaux", ", depuis janvier ", year0)


p5 <- ipc_line_faceting_var(d, code.x, year0 = 2019, title0, subtitle0, lim.y = c(-10, 15), k = 3)

ggsave(file = "immagini/p2.1.svg", plot = p1, width = 10, height = 6.875)
ggsave(file = "immagini/p2.2.svg", plot = p2, width = 10, height = 6.875)
ggsave(file = "immagini/p2.3.svg", plot = p3, width = 10, height = 6.875)
ggsave(file = "immagini/p2.4.svg", plot = p4, width = 10, height = 6.875)
ggsave(file = "immagini/p2.5.svg", plot = p5, width = 10, height = 6.875)
