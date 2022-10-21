
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
  mutate(etichetta = paste0(if_else(is.na(substr(COICOP, 2, 3)),
                                    "", paste0(substr(COICOP, 2, 3), ". ")),
                            if_else(substr(PosTxt_F, 1, 30) == PosTxt_F,
                                    PosTxt_F, paste0(substr(PosTxt_F, 1, 30), "...")),
                            " (", format(round(w2022, digits = 1L), nsmall = 1L), "%)"))


################################################################################
#
###  Blocco IV
#
##### F.4: creo dei gruppi per "classi di crescita"
# - tx < 0
# - 0 <= tx < 1
# - 1 <= tx < 2
# - 2 <= tx < 5
# - tx >= 5

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
  
tmp0 <- d %>%
    filter(var.x == "IPC",
           month.yr == max(d$month.yr)) %>%
    arrange(value) %>%
    ungroup() %>%
    mutate(rango = row_number())

tmp0 <- tmp0 %>%
  select("PosTxt_F", "rango")

tmp0 <- as.data.frame(tmp0)


tmp <- d %>%
  filter(var.x == "VAR_m-12",
         month.yr == max(d$month.yr)) %>%
  mutate(value2 = value * w2022 / 100) %>%
  arrange(value2) %>%
  ungroup() %>%
  mutate(rango3 = row_number())

tmp <- tmp %>%
  arrange(desc(etichetta)) %>%
  ungroup() %>%
  mutate(rango0 = row_number())

tmp <- tmp %>%
  arrange(w2022) %>%
  ungroup() %>%
  mutate(rango2 = row_number())
  
  
tmp <- tmp %>%
    select("PosTxt_F", "rango0", "rango2", "rango3")

tmp <- as.data.frame(tmp)

tmp <- merge(tmp, tmp0, by = "PosTxt_F")

d2 <- merge(d0, tmp, by = "PosTxt_F")

d2
  
}
                        

base <- "Taux de croissance (en %), selon la classe de croissance et "
fin <- " , depuis "

year0 <- 2019

subtitle_b <- "Groupes de produits, selon la contribution au renchérissement (total des poids: "
fin2 <- "%)"


# F.1 per gruppi principali
d1 <- d %>%
  group_by(month.yr) %>%
  filter(var.x %in% c("IPC", "VAR_m-12"),
         freq.x %in% "month",
         year.x >= 2019,
         Level %in% 2
  ) %>%
  mutate(prova = sum(w2022) / 2)

d1_ <- crea_classe(d1)

tmp <- d1_ %>%
  filter(month.yr == year0)

a <- sum(tmp$w2022)[1]

title0 <- paste0(base, "les groupes principaux", fin, year0)
subtitle0 <- paste0(subtitle_b, format(round(a, digits = 1L), nsmall = 1L), fin2)


p1 <- ipc_heatmap(d1_, title0, subtitle0)


# F.2 secondo "level 3"

d2 <- d %>%
  group_by(month.yr) %>%
  filter(var.x %in% c("IPC", "VAR_m-12"),
         freq.x %in% "month",
         year.x >= 2019,
         Level %in% 3
  ) %>%
  mutate(prova = sum(w2022) / 2)


d2_ <- crea_classe(d2)
tmp <- d2_ %>%
  filter(month.yr == year0)

a <- sum(tmp$w2022)[1]

title0 <- paste0(base, "le groupe de produits (Lev. 2)", fin, year0)
subtitle0 <- paste0(subtitle_b, format(round(a, digits = 1L), nsmall = 1L), fin2)


p2 <- ipc_heatmap(d2_, title0, subtitle0)


# F.3 secondo "level 3"

d3 <- d %>%
  group_by(month.yr) %>%
  filter(var.x %in% c("IPC", "VAR_m-12"),
         freq.x %in% "month",
         year.x >= year0,
         Level %in% 4)


d3_plus <- d %>%
  group_by(month.yr) %>%
  filter(var.x %in% c("IPC", "VAR_m-12"),
         freq.x %in% "month",
         year.x >= year0,
         PosNo %in% c("6059", "8001", "8006", "10070", "10100", "12190")
  )


d3 <- rbind(d3, d3_plus)
d3 <- d3 %>% mutate(prova = sum(w2022) / 2)


d3_ <- crea_classe(d3)

d3_1 <- d3_ %>% filter(rango3 <= 25)

tmp <- d3_1 %>%
  filter(month.yr == year0)

a <- sum(tmp$w2022)[1]

title0 <- paste0(base, "les groupes de produit (Lev. 3)", fin, year0)
subtitle0 <- paste0("Last 25 | ", subtitle_b, format(round(a, digits = 1L), nsmall = 1L), fin2)


p3 <- ipc_heatmap(d3_1, title0, subtitle0)


d3_2 <- d3_ %>% filter(rango3 >= max(rango3) - 26)

tmp <- d3_2 %>%
  filter(month.yr == year0)

a <- sum(tmp$w2022)[1]

title0 <- paste0(base, "les groupes de produit (Lev. 3)", fin, year0)
subtitle0 <- paste0("First 25 | ", subtitle_b, format(round(a, digits = 1L), nsmall = 1L), fin2)


p4 <- ipc_heatmap(d3_2, title0, subtitle0)



# F.3 secondo "level 4"

d4 <- d %>%
  group_by(month.yr) %>%
  filter(var.x %in% c("IPC", "VAR_m-12"),
         freq.x %in% "month",
         year.x >= year0,
         Level %in% 5)

d4_plus <- d %>%
  group_by(month.yr) %>%
  filter(var.x %in% c("IPC", "VAR_m-12"),
         freq.x %in% "month",
         year.x >= year0,
         PosNo %in% c("2064", "2076", "2082", "3183", "3237", "4005", "4010", "4047", "4090", "4100",
                      "5060", "5090", "5071", "5085", "5120", "5150", "5165", "5181", "5280", "6002",
                      "6070", "6036", "7300", "7320", "7500", "8018", "8042", "8045", "9029", "9120",
                      "9151", "9200", "9320", "9340", "9480", "9545", "9555", "9570", "9580", "10041",
                      "10060", "11171", "11190", "12150", "12140", "12161", "12170", "12501", "12510",
                      "12520", "12543", "12547", "12549", "12534", "12536"))

d4 <- rbind(d4, d3_plus, d4_plus)
d4 <- d4 %>% mutate(prova = sum(w2022))


d4_ <- crea_classe(d4)

tmp2 <- d4_ %>% filter(month.yr == max(month.yr))

d4_ <- d4_ %>% filter(rango3 >= max(rango3) - 26)

tmp <- d4_ %>%
  filter(month.yr == year0)

a <- sum(tmp$w2022)[1]

title0 <- paste0(base, "les groupes de produit (Lev. 4)", fin, year0)
subtitle0 <- paste0("First 25 | ", subtitle_b, format(round(a, digits = 1L), nsmall = 1L), fin2)


p5 <- ipc_heatmap(d4_, title0, subtitle0)




ggsave(file = "immagini/p4.1.svg", plot = p1, width = 10, height = 6.875)
ggsave(file = "immagini/p4.2.svg", plot = p2, width = 10, height = 6.875)
ggsave(file = "immagini/p4.3.svg", plot = p3, width = 10, height = 6.875)
ggsave(file = "immagini/p4.4.svg", plot = p4, width = 10, height = 6.875)
ggsave(file = "immagini/p4.5.svg", plot = p5, width = 10, height = 6.875)
