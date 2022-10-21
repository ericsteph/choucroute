
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
###  F.1 Figure sull'IPC generale, indicatore: indice 100


# Le prime figure si riferiscono al Totale (Code = "100_100"),

tmp <- d %>% 
  filter(freq.x %in% "month",
         var.x == "IPC",
         Code %in% "100_100") %>%
  mutate(Period = if_else(month.yr < 2022, "1", "2")) %>%
  ungroup()


# Preparo figure in serie temporale, trasformo l'indice base con diverse basi
# a0: base dicembre 2010

a0 <- tmp %>%
  filter(year.x %in% "2010",
         month.n %in% "12") %>%
  select(value) %>%
  as.double()


# a: base dicembre 1987

a <- tmp %>%
  filter(year.x %in% "1987",
         month.n %in% "12") %>%
  select(value) %>%
  as.double()

tmp <- tmp %>%
  mutate(value2 = (value / a0) * 100,
         value3 = (value / a) * 100 )


# Figura 1.1.1, evoluzione dell'indice dei prezzi, in Svizzera, dal 2015

p1 <- ipc_line(tmp, y0 = "value", year0 = 2015, z0 = "Period")


# Figura 1.2.1, evoluzione dell'indice dei prezzi, in Svizzera, dal 2005

p2 <- ipc_line(tmp, y0 = "value2", year0 = 2005, base0 = 2010, a = 5,
               lim.y = c(90, 105))


# Figura 1.3.1, evoluzione dell'indice dei prezzi, in Svizzera, dal 1985

p3 <- ipc_line(tmp, y0 = "value3", year0 = 1985, base0 = 1987, a = 5,
               lim.y = c(80, 160))


# Figura 1.4, confronto come in Articolo "Il rebus dell'inflazione
# Evoluzione base 1986 vs base 2020

tmp1 <- tmp %>%
  filter(month.yr >= 1985,
         month.yr < 1993) %>%
  select(month.yr, value3) %>%
  mutate(nr = row_number())

tmp2 <- tmp %>%
  filter(month.yr >= 2018) %>%
  select(month.yr, value) %>%
  mutate(nr = row_number())

tmp3 <- left_join(tmp1, tmp2, by = "nr") %>%
  select(nr, month.yr.x, month.yr.y, value, value3) %>%
  pivot_longer(cols = 4:5, names_to = "base")

tmp3$base <- as.factor(tmp3$base)
levels(tmp3$base) <- c("déc. 2020 = 100", "déc. 1987 = 100")

p4 <- tmp3 %>%
  ggplot() +
  aes(x = month.yr.x, y = value, color = base) +
  geom_hline(yintercept = 100, size = .6, colour = "black") +
  geom_vline(xintercept = 1988 - 1/12, size = .6, colour = "gray", linetype = 3) +
  geom_line(na.rm = TRUE, size = 1.2) +
  scale_x_yearmon(breaks = c(1986 - 1/12, 1988 - 1/12, 1990 - 1/12, 1992 - 1/12),
                  labels = c("t0 - 2", "t0", "t0 + 2", "t0 + 4"),
                  limits = c(1985, 1993 - 11/12)) +
  scale_y_continuous(limits = c(95, 120)) +
  scale_color_manual(values = c(pal_ustat[2], pal_ustat[1]),
                     name = "") +
  labs(x = "", y = "",
       title = "Indice des prix à la consommation, en Suisse, 1985-1992 vs 2018-2022",
       subtitle = "Base décembre 1987 = 100, Base décembre 2020 = 100",
       caption = "Source: Indice des prix à la consommation, OFS, Neuchâtel") +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "gray"),
    plot.caption = element_text(size = rel(0.75), colour = "gray"),
    axis.text = element_text(size = rel(0.75)),
    legend.text=element_text(size = rel(0.875)),
    legend.position = c(.975, .025),
    legend.justification = c("right", "bottom"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(fill= "transparent"))


# Faccio ancora una figura, ma con la variazione su base annua, sempre rispetto al Totale (Code = "100_100"),
# Figura 1.5, confronto come in Articolo "Il rebus dell'inflazione
# Evoluzione base 1986 vs base 2020

tmp <- d %>% 
  filter(freq.x %in% "month",
         var.x == "VAR_m-12",
         Code %in% "100_100") %>%
  ungroup()

tmp1 <- tmp %>%
  filter(month.yr >= 1985,
         month.yr < 1993) %>%
  mutate(value3 = value) %>%
  select(month.yr, value3) %>%
  mutate(nr = row_number())

tmp2 <- tmp %>%
  filter(month.yr >= 2018) %>%
  select(month.yr, value) %>%
  mutate(nr = row_number())

tmp3 <- left_join(tmp1, tmp2, by = "nr") %>%
  select(nr, month.yr.x, month.yr.y, value, value3) %>%
  pivot_longer(cols = 4:5, names_to = "base")

tmp3$base <- as.factor(tmp3$base)
levels(tmp3$base) <- c("déc. 2020 = 100", "déc. 1987 = 100")

p5 <- tmp3 %>%
  ggplot() +
  aes(x = month.yr.x, y = value / 100, fill = base) +
  geom_hline(yintercept = .02, size = .6, colour = "black") +
  geom_vline(xintercept = 1988 - 1/12, size = .6, colour = "gray", linetype = 3) +
  geom_col(na.rm = TRUE, position = "dodge", colour = "white") +
  scale_x_yearmon(breaks = c(1986 - 1/12, 1988 - 1/12, 1990 - 1/12, 1992 - 1/12),
                  labels = c("t0 - 2", "t0", "t0 + 2", "t0 + 4"),
                  limits = c(1985, 1993 - 11/12)) +
  scale_y_continuous(breaks = c(-.02, 0, .02, .04, .06),
                     labels = scales::percent,
                     limits = c(-.02, .069)) +
  scale_fill_manual(values = c(pal_ustat[2], pal_ustat[1]),
                     name = "") +
  labs(x = "", y = "",
       title = "Taux de croissance de l'IPC, en Suisse, 1985-1992 vs 2018-2022",
       subtitle = "Base décembre 1987 = 100, Base décembre 2020 = 100",
       caption = "Source: Indice des prix à la consommation, OFS, Neuchâtel") +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "gray"),
    plot.caption = element_text(size = rel(0.75), colour = "gray"),
    axis.text = element_text(size = rel(0.75)),
    legend.text=element_text(size = rel(0.875)),
    legend.position = c(.975, .025),
    legend.justification = c("right", "bottom"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(fill= "transparent"))



ggsave(file = "immagini/p1.svg", plot = p1, width = 10, height = 6.875)
ggsave(file = "immagini/p2.svg", plot = p2, width = 10, height = 6.875)
ggsave(file = "immagini/p3.svg", plot = p3, width = 10, height = 6.875)
ggsave(file = "immagini/p4.svg", plot = p4, width = 10, height = 6.875)
ggsave(file = "immagini/p5.svg", plot = p5, width = 10, height = 6.875)
