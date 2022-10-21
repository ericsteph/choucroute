library(tidyverse)
library(readxl)

# d <- read_excel("G:/Giornalisti/ES/excel/base.xlsx")
d <- read_excel("./excel/base2.xlsx")

breaks_fig <- c("1990, II trim.", "1994, II trim.", "1998, II trim.",
                "2002, II trim.", "2006, II trim.", "2010, II trim.",
                "2014, II trim.", "2018, II trim.", "2022, II trim.")

etichette <- c("1990\n(II trim.)", "1994\n(II trim.)", "1998\n(II trim.)",
               "2002\n(II trim.)", "2006\n(II trim.)", "2010\n(II trim.)",
               "2014\n(II trim.)", "2018\n(II trim.)", "2022\n(II trim.)")

etichette0 <- c("1990", "1994", "1998", "2002",
               "2006", "2010", "2014", "2018", "2021")

etichette2 <- c("1990\naprile", "1994\naprile", "1998\naprile", "2002\naprile",
               "2006\naprile", "2010\naprile", "2014\naprile", "2018\naprile",
               "2022\naprile")

breaks_fig2 <- c("1990, IV trim.", "1994, IV trim.", "1998, IV trim.",
                "2002, IV trim.", "2006, IV trim.", "2010, IV trim.",
                "2014, IV trim.", "2018, IV trim.", "2022, IV trim.")



# Figura 1, Percezione dell'evoluzione dei prezzi, in Svizzera
p1 <- d %>%
  select(QUANDO, GEO, `Previsione prezzi`) %>%
  filter(GEO %in% "Svizzera") %>%
  ggplot() +
  aes(x = QUANDO, y = `Previsione prezzi`, group = GEO) +
  geom_smooth(colour = "red", fill = "gray80", size = .4, na.rm = TRUE, method = "loess"
              , span = .35
              ) +
  geom_line(na.rm = TRUE, size = 1.2) +
  scale_x_discrete(breaks = breaks_fig,
                   labels = etichette2) +
  labs(x = "", y = "",
       title = "Percezione dell'evoluzione dei prezzi nei prossimi 12 mesi, in Svizzera, dal 1990",
       subtitle = "Indice",
       caption = "Fonte: Indice del clima di fiducia dei consumatori, Seco, Berna") +
  theme_bw() +
  theme(
    plot.title = element_text(size = rel(0.8), face = "bold"),
    plot.subtitle = element_text(size = rel(0.7), colour = "gray"),
    plot.caption = element_text(size = rel(0.6), colour = "gray"),
    axis.text = element_text(size = rel(0.5)))


# Figura 2, Percezione dell'evoluzione dei prezzi e evoluzione dei prezzi, in Svizzera
Y.labs <- c("Variazione dell'IPC\nbase annua, in%", "Percezione dell'evoluzione\ndei prezzi")
names(Y.labs) <- c("IPC", "Previsione prezzi")

p2 <- d %>%
  filter(GEO %in% "Svizzera") %>%
  select(QUANDO, GEO, IPC, `Previsione prezzi`) %>%
  pivot_longer(cols = 3:4, names_to = "Y") %>%
  ggplot() +
  aes(x = QUANDO, y = value, group = 1) +
  geom_smooth(colour = "red", fill = "gray80", size = .4, na.rm = TRUE, method = "loess"
              , span = .35
  ) +
  geom_line(na.rm = TRUE, size = 1.2) +
  scale_x_discrete(breaks = breaks_fig,
                   labels = etichette2) +
  facet_grid(rows = vars(Y), labeller = labeller(Y = Y.labs), scales = "free_y") +
  labs(x = "", y = "",
       title = "Percezione dell'evoluzione dei prezzi vs. evoluzione dei prezzi, in Svizzera, dal 1990",
       subtitle = "Indice del clima di fiducia vs. variazione su base annua dell'indice dei prezzi al consumo",
       caption = "Fonti: IFC, Seco, Berna; IPC, UST, Neuchatel") +
  theme_bw() +
  theme(
    plot.title = element_text(size = rel(0.8), face = "bold"),
    plot.subtitle = element_text(size = rel(0.7), colour = "gray"),
    plot.caption = element_text(size = rel(0.6), colour = "gray"),
    axis.text = element_text(size = rel(0.5)))


# Figura 3, Indice di fiducia dei consumatori, in Svizzera
p3 <- d %>%
  select(QUANDO, GEO, IFC) %>%
  filter(GEO %in% "Svizzera") %>%
  ggplot() +
  aes(x = QUANDO, y = IFC, group = GEO) +
  geom_smooth(colour = "red", fill = "gray80", size = .4, na.rm = TRUE, method = "loess"
              , span = .35
  ) +
  geom_line(na.rm = TRUE, size = 1.2) +
  scale_x_discrete(breaks = breaks_fig,
                   labels = etichette2) +
  labs(x = "", y = "",
       title = "Indice di fiducia dei consumatori, in Svizzera\nogni tre mesi, dal 1990",
       subtitle = "Indice",
       caption = "Fonte: Indice del clima di fiducia dei consumatori, Seco, Berna") +
  theme_bw() +
  theme(
    plot.title = element_text(size = rel(0.8), face = "bold"),
    plot.subtitle = element_text(size = rel(0.7), colour = "gray"),
    plot.caption = element_text(size = rel(0.6), colour = "gray"),
    axis.text = element_text(size = rel(0.5)))


# Figura 4, Sottoindici dell'IFC, in Svizzera
Y.labs <- c("Grandi acquisti", "Situazione economica futura", "Situazione finanziaria passata", "Situazione finanziaria futura")
names(Y.labs) <- c("Acquisti", "Eco_nxt", "Finanze", "Finanze_next")

p4 <- d %>%
  filter(GEO %in% "Svizzera") %>%
  select(QUANDO, GEO, Acquisti, Eco_nxt, Finanze, Finanze_next) %>%
  pivot_longer(cols = 3:6, names_to = "Y") %>%
  ggplot() +
  aes(x = QUANDO, y = value, group = GEO) +
  geom_smooth(colour = "red", fill = "gray80", size = .4, na.rm = TRUE, method = "loess"
              , span = .35
  ) +
  geom_line(na.rm = TRUE, size = 1.2) +
  scale_x_discrete(breaks = breaks_fig,
                   labels = etichette2) +
  facet_wrap(vars(Y), labeller = labeller(Y = Y.labs), ncol = 2) +
  labs(x = "", y = "",
       title = "Prossimi grandi acquisti, in Svizzera\nogni tre mesi, dal 2008",
       subtitle = "Indice",
       caption = "Fonte: Indice del clima di fiducia dei consumatori, Seco, Berna") +
  theme_bw() +
  theme(
    plot.title = element_text(size = rel(0.8), face = "bold"),
    plot.subtitle = element_text(size = rel(0.7), colour = "gray"),
    plot.caption = element_text(size = rel(0.6), colour = "gray"),
    axis.text = element_text(size = rel(0.5)))




# Figura 5, evoluzione del PIL e dell'IPC in Svizzera
Y.labs <- c("Indice dei prezzi al consumo (IPC)", "Prodotto interno lordo (PIL)")
names(Y.labs) <- c("IPC", "PIL_tx")

p5 <- d %>%
  filter(GEO %in% "Svizzera") %>%
  select(QUANDO, GEO, PIL_tx, IPC) %>%
  pivot_longer(cols = 3:4, names_to = "Y") %>%
  ggplot() +
  aes(x = QUANDO, y = value, group = 1) +
  geom_smooth(colour = "red", fill = "gray80", size = .4, na.rm = TRUE, method = "gam") +
  geom_line(na.rm = TRUE, size = 1.2) +
  facet_grid(cols = vars(Y), labeller = labeller(Y = Y.labs)) +
  scale_x_discrete(breaks = breaks_fig,
                   labels = etichette) +
  labs(x = "", y = "",
       title = "Evoluzione del PIL e dell'IPC in Svizzera,\nper trimestre, dal 1990",
       subtitle = "Variazione in % rispetto allo stesso periodo dell'anno precedente",
       caption = "Fonti: PIL, Seco, Berna; IPC, UST, Neuchatel") +
  theme_bw() +
  theme(
    plot.title = element_text(size = rel(0.8), face = "bold"),
    plot.subtitle = element_text(size = rel(0.7), colour = "gray"),
    plot.caption = element_text(size = rel(0.6), colour = "gray"),
    axis.text = element_text(size = rel(0.5)))


# Figura 6, variazione mensile dell'IPC in Svizzera
Y.labs <- c("Febbraio", "Novembre")
names(Y.labs) <- c(1, 4)

p6 <- d %>%
  filter(GEO %in% "Svizzera",
         Trimestre %in% c(1, 4)) %>%
  select(QUANDO, GEO, Trimestre, IPC_m2) %>%
  ggplot() +
  aes(x = QUANDO, y = IPC_m2, group = 1) +
  geom_smooth(colour = "red", fill = "gray80", size = .4, na.rm = TRUE, method = "gam") +
  geom_line(na.rm = TRUE, size = 1.2) +
  facet_grid(cols = vars(Trimestre), labeller = labeller(Trimestre = Y.labs)) +
  scale_x_discrete(breaks = breaks_fig2,
                   labels = etichette0) +
  scale_y_continuous(breaks = c(-1, -.5, 0, .5, 1, 1.5),
                     labels = c("-1,0%", "-0,5%", "0,0%", " 0,5%", " 1,0%", " 1,5%"), limits = c(-1.05, 1.55)) +
  labs(x = "", y = "",
       title = "Evoluzione dell'IPC in Svizzera,\nnel mese di febbraio e di novembre, dal 1990",
       subtitle = "Variazione in % rispetto al mese precedente",
       caption = "Fonte:  IPC, UST, Neuchatel") +
  theme_bw() +
  theme(
    plot.title = element_text(size = rel(0.8), face = "bold"),
    plot.subtitle = element_text(size = rel(0.7), colour = "gray"),
    plot.caption = element_text(size = rel(0.6), colour = "gray"),
    axis.text = element_text(size = rel(0.5)))


a <- 4 * 1.85
b <- 3 * 1.85


# Salvo le figure
ggsave(file="plot/p1.svg", plot=p1, width=a, height=b)
ggsave(file="plot/p2.svg", plot=p2, width=a, height=b)
ggsave(file="plot/p3.svg", plot=p3, width=a, height=b)
ggsave(file="plot/p4.svg", plot=p4, width=a, height=b)
ggsave(file="plot/p5.svg", plot=p5, width=a, height=b)
ggsave(file="plot/p6.svg", plot=p6, width=a, height=b)


# Figura 3, tasso di crescita dei prezzi negli Stati Uniti

p3 <- d %>%
  filter(GEO %in% "USA") %>%
  select(QUANDO, GEO, IPC) %>%
  ggplot() +
  aes(x = QUANDO, y = IPC, group = GEO) +
  geom_smooth(colour = "red", fill = "gray80", size = .4, na.rm = TRUE, method = "gam") +
  geom_line(na.rm = TRUE, size = 1.2) +
  # facet_grid(rows = vars(GEO), scales = "free_y") +
  scale_x_discrete(breaks = breaks_fig,
                   labels = etichette) +
  labs(x = "", y = "",
       title = "Tasso di crescita dei prezzi, negli Stati Uniti (in %),\nper trimestre, dal 1990",
       subtitle = "Rispetto allo stesso periodo dell'anno precedente",
       caption = "Fonte: IPCA, OECD, Parigi") +
  theme_bw() +
  theme(
    plot.title = element_text(size = rel(0.8), face = "bold"),
    plot.subtitle = element_text(size = rel(0.7), colour = "gray"),
    plot.caption = element_text(size = rel(0.6), colour = "gray"),
    axis.text = element_text(size = rel(0.5)))


# Figura 4, tasso di crescita dei prezzi, in Svizzera e negli Stati Uniti

p4 <- d %>%
  select(QUANDO, GEO, IPC, IPC_energia) %>%
  pivot_longer(cols = 3:4) %>%
  ggplot() +
  aes(x = QUANDO, y = value, group = 1) +
  geom_smooth(colour = "red", fill = "gray80", size = .4, na.rm = TRUE, method = "gam") +
  geom_line(na.rm = TRUE, size = 1.2) +
  facet_grid(rows = vars(name), cols = vars(GEO), scales = "free_y") +
  scale_x_discrete(breaks = breaks_fig,
                   labels = etichette) +
  labs(x = "", y = "",
       title = "Tasso di crescita dei prezzi, in Svizzera e negli Stati Uniti (in %),\ntotale vs. ''Energia'', per trimestre, dal 1990",
       subtitle = "Rispetto allo stesso periodo dell'anno precedente",
       caption = "Fonte: IPCA, OECD, Parigi") +
  theme_bw() +
  theme(
    plot.title = element_text(size = rel(0.8), face = "bold"),
    plot.subtitle = element_text(size = rel(0.7), colour = "gray"),
    plot.caption = element_text(size = rel(0.6), colour = "gray"),
    axis.text = element_text(size = rel(0.5)))

