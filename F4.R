
rm(list = ls())

library(tidyverse)
library(readxl)
library(zoo)
library(scales)
library(themeric)

data("pal_ustat")
source("01_FUN_figure.R", encoding = "UTF-8")

d0 <- readRDS("cubi esterni/swissPCI.rds")

d <- d0 %>%
  mutate(etichetta = paste0(if_else(is.na(substr(COICOP, 2, 3)), "",
                                    paste0(substr(COICOP, 2, 3), ". ")),
                            if_else(substr(PosTxt_F, 1, 21) == PosTxt_F,
                                    PosTxt_F, paste0(substr(PosTxt_F, 1, 21), "...")),
                            " (", round(w2022, digits = 2L), ")"))



crea_classe <- function(d){

d0 <- d %>%
    filter(var.x == "VAR_m-12") %>%
    mutate(classe = factor(
      if_else(value < 0, "a. VAR_m-12 < 0%",
              if_else(value < 1, "b. 0% <= VAR_m-12 < 1%",
                      if_else(value < 2, "c. 1% <= VAR_m-12 < 2%",
                              if_else(value < 5, "d. 2% <= VAR_m-12 < 5%",
                                      "e. VAR_m-12 >= 5%")))))
    )
  
tmp <- d %>%
    filter(var.x == "IPC",
           month.yr == max(d$month.yr)) %>%
    arrange(value) %>%
    ungroup() %>%
    mutate(rango = row_number())
  
  
tmp <- tmp %>%
    select("PosTxt_F", "rango")
  
tmp <- as.data.frame(tmp)

d <- merge(d0, tmp, by = "PosTxt_F")
  
}

base <- "Taux de croissance (VAR_m-12), selon les "               

# T.6 CONTR_m per posizione, da gennaio 2022

title0 <- paste0()
d1 <- d %>%
  group_by(month.yr) %>%
  filter(var.x %in% c("IPC", "VAR_m-12"),
         freq.x %in% "month",
         year.x >= 2019,
         Level %in% 2
         ) %>%
  mutate(prova = sum(w2022) / 2)

d1_ <- crea_classe(d1)

p2 <- ipc_heatmap(d1_, title0 = "prova", subtitle0 = "34")

p1 <- d1_ %>%
  ggplot(aes(month.yr, fct_reorder(etichetta, rango))) +
  geom_tile(aes(fill = classe), colour = "white") +
  scale_fill_brewer(palette = "OrRd",
                    name = "IPC, taux de\n croissance (en %)",
                    na.translate = F) +
  theme_minimal()


d2 <- d %>%
  group_by(month.yr) %>%
  filter(var.x %in% c("IPC", "VAR_m-12"),
         freq.x %in% "month",
         year.x >= 2019,
         Level %in% 3
  ) %>%
  mutate(prova = sum(w2022) / 2)

d3 <- d %>%
  group_by(month.yr) %>%
  filter(var.x %in% c("IPC", "VAR_m-12"),
         freq.x %in% "month",
         year.x >= 2019,
         Level %in% 4)


d3_plus <- d %>%
  group_by(month.yr) %>%
  filter(var.x %in% c("IPC", "VAR_m-12"),
         freq.x %in% "month",
         year.x >= 2019,
         PosNo %in% c("6059", "8001", "8006", "10070", "10100", "12190")
  )


d3 <- rbind(d3, d3_plus)
d3 <- d3 %>% mutate(prova = sum(w2022) / 2)



d4 <- d %>%
  group_by(month.yr) %>%
  filter(var.x %in% "VAR_m-12",
         freq.x %in% "month",
         year.x >= 2019,
         Level %in% 5)

d4_plus <- d %>%
  group_by(month.yr) %>%
  filter(var.x %in% "VAR_m-12",
         freq.x %in% "month",
         year.x >= 2019,
         PosNo %in% c("2064", "2076", "2082", "3183", "3237", "4005", "4010", "4047", "4090", "4100",
                      "5060", "5090", "5071", "5085", "5120", "5150", "5165", "5181", "5280", "6002",
                      "6070", "6036", "7300", "7320", "7500", "8018", "8042", "8045", "9029", "9120",
                      "9151", "9200", "9320", "9340", "9480", "9545", "9555", "9570", "9580", "10041",
                      "10060", "11171", "11190", "12150", "12140", "12161", "12170", "12501", "12510",
                      "12520", "12543", "12547", "12549", "12534", "12536"))

d4 <- rbind(d4, d3_plus, d4_plus)
d4 <- d4 %>% mutate(prova = sum(w2022))

d4 <- d4 %>%
  mutate(classe = factor(if_else(value < 0, "VAR_m-12 < 0%",
                          if_else(value < 1, "0% <= VAR_m-12 < 1%",
                                  if_else(value < 2, "1% <= VAR_m-12 < 2%",
                                          if_else(value < 5, "2% <= VAR_m-12 < 5%",
                                                  "VAR_m-12 >= 5%")))))
         )


p1 <- d1 %>%
  ggplot(aes(month.yr, fct_reorder(etichetta, rango))) +
  geom_tile(aes(fill = classe), colour = "white") +
  scale_fill_brewer(palette = "Spectral",
                    name = "IPC, taux de\n croissance (en %)",
                    na.translate = F) +
  theme_minimal()

p1 <- d3_ %>%
  ggplot(aes(month.yr, fct_reorder(etichetta, rango))) +
  geom_tile(aes(fill = classe), colour = "white") +
  scale_fill_brewer(palette = "Spectral",
                    name = "IPC, taux de\n croissance (en %)",
                    na.translate = F) +
  theme_minimal()


d4_2021 <- d4 %>%
  filter(month.yr == 2021.5)

################################################################################
#
###  Blocco IV
#
##### F.4: creo dei gruppi per "classi di crescita"
# - tx < 0
# - 0 <= tx < 1
# - 1 <= tx < 2
# - 2 <= tx < 3
# - tx >= 3


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
