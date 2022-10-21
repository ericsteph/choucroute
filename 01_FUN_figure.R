


ipc_line <- function(d, y0, year0,
                     z0 = "Period",
                     base0 = 2020, a = 2,
                     lim.y = c(95, 105),
                     legend.pos = c("bottom", 0.05)){

b <- 1/a

p <- d %>%
    filter(month.yr >= year0) %>%
    ggplot() +
    aes(x = month.yr, y = .data[[y0]], color = .data[[z0]]) +
    geom_hline(yintercept = 100, size = .6, colour = "black") +
    geom_vline(xintercept = base0 + 11/12, size = .6, colour = "gray", linetype = 3) +
    geom_line(na.rm = TRUE, size = 1.2) +
    scale_x_yearmon(breaks = c(seq(year0 + 1 - 1/12, 2024, by = a)),
                    limits = c(year0, 2024 - 1/12),
                    format = "%b\n%Y") +
    scale_y_continuous(limits = lim.y) +
    scale_color_manual(values = pal_ustat,
                       name = "",
                       labels = c("avant 2022", "après 2022")) +
    labs(x = "", y = "",
         title = paste0("Indice des prix à la consommation, en Suisse, depuis janvier ", year0),
         subtitle = paste0("Base décembre ", base0, " = 100"),
         caption = "Source: Indice des prix à la consommation, OFS, Neuchâtel") +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(colour = "gray"),
      plot.caption = element_text(size = rel(0.7), colour = "gray"),
      axis.text = element_text(size = rel(0.75)),
      legend.text=element_text(size = rel(0.875)),
      legend.justification = c("right", legend.pos[1]),
      legend.position = c(.95, legend.pos[2]),
      legend.box.just = "left",
      legend.margin = margin(6, 6, 6, 6),
      legend.background = element_rect(fill= "transparent"))

p

}


ipc_line_faceting <- function(d,
                              year0,
                              code.x,
                              title0,
                              subtitle0,
                              y0 = 100,
                              x0 = 2020 + 11 /12,
                              code0 = "100_100",
                              lim.y = c(95, 105),
                              k = 1
                              ){
  
d0 <- d %>%
  filter(freq.x %in% "month",
         var.x == "IPC",
         month.yr >= year0,
         Code %in% code0) %>%
  select(month.yr, Code, var.x, PosNo, year.x, value)

tmp <- d %>%
  filter(freq.x %in% "month",
         var.x == "IPC",
         month.yr >= year0,
         Code %in% code.x) %>%
  mutate(Label = paste0(PosTxt_F, " (Poid: ", w2022, ")"))

p <- ggplot() +
  geom_line(data = d0, aes(x = month.yr, y = value), colour = "black", size = .9, na.rm = TRUE) +
  geom_line(data = tmp, aes(x = month.yr, y = value, colour = PosTxt_F), na.rm = TRUE, size = 1.2) +
  geom_hline(yintercept = y0, size = .6, colour = "black") +
  geom_vline(xintercept = x0, size = .6, colour = "gray", linetype = 3) +
  scale_x_yearmon(breaks = c(seq(year0 + 1 - 1/12, 2024, by = 1)),
                  limits = c(year0, 2023 - 1/12),
                  format = "%b\n%Y") +
  scale_y_continuous(limits = lim.y) +
  scale_color_manual(values = c(pal_ustat, "red", "green"),
                     name = "") +
  facet_wrap(vars(Label), nrow = k) +
  labs(x = "", y = "",
       title = title0,
       subtitle = subtitle0,
       caption = "Source: Indice des prix à la consommation, OFS, Neuchâtel") +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "gray"),
    plot.caption = element_text(size = rel(0.7), colour = "gray"),
    axis.text = element_text(size = rel(0.75)),
    legend.position = "none"
    )

p

}


ipc_line_faceting_var <- function(d,
                              year0,
                              code.x,
                              title0,
                              subtitle0,
                              y0 = 0,
                              x0 = 2020 + 11 /12,
                              code0 = "100_100",
                              lim.y = c(-10, 50),
                              k = 1
){
  
  d0 <- d %>%
    filter(freq.x %in% "month",
           var.x == "VAR_m-12",
           month.yr >= year0,
           Code %in% code0) %>%
    select(month.yr, Code, var.x, PosNo, year.x, value)
  
  tmp <- d %>%
    filter(freq.x %in% "month",
           var.x == "VAR_m-12",
           month.yr >= year0,
           Code %in% code.x) %>%
    mutate(Label = paste0(PosTxt_F, " (Poids: ", w2022, ")"))
  
  p <- ggplot() +
    geom_line(data = d0, aes(x = month.yr, y = value), colour = "black", size = .9, na.rm = TRUE) +
    geom_line(data = tmp, aes(x = month.yr, y = value, colour = PosTxt_F), na.rm = TRUE, size = 1.2) +
    geom_hline(yintercept = y0, size = .6, colour = "black") +
    geom_vline(xintercept = x0, size = .6, colour = "gray", linetype = 3) +
    scale_x_yearmon(breaks = c(seq(year0 + 1 - 1/12, 2024, by = 1)),
                    limits = c(year0, 2023 - 1/12),
                    format = "%b\n%Y") +
    scale_y_continuous(limits = lim.y,
                       labels = percent_format(scale = 1)) +
    scale_color_manual(values = c(pal_ustat, "red", "green"),
                       name = "") +
    facet_wrap(vars(Label), nrow = k) +
    labs(x = "", y = "",
         title = title0,
         subtitle = subtitle0,
         caption = "Source: Indice des prix à la consommation, OFS, Neuchâtel") +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(colour = "gray"),
      plot.caption = element_text(size = rel(0.7), colour = "gray"),
      axis.text = element_text(size = rel(0.75)),
      legend.position = "none"
    )
  
  p
  
}


ipc_heatmap <- function(d,
                        title0,
                        subtitle0,
                        z = "rango3"
                        ){

p <- d %>%
  ggplot(aes(month.yr, fct_reorder(etichetta, .data[[z]]))) +
  geom_tile(aes(fill = classe), colour = "white") +
  scale_fill_brewer(palette = "OrRd",
                    name = "",
                    na.translate = F,
                    labels = c("tx < 0,0%", "0,0% <= tx < 1,0%",
                               "1,0% <= tx < 2,0%", "2,0% <= tx < 5,0%",
                               "tx >= 5,0%")) +
  labs(x = "", y = "",
       title = title0,
       subtitle = subtitle0,
       caption = "Source: Indice des prix à la consommation, OFS, Neuchâtel") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = rel(0.825), face = "bold"),
    plot.subtitle = element_text(size = rel(0.825), colour = "gray"),
    plot.caption = element_text(size = rel(0.7), colour = "gray"),
    axis.text = element_text(size = rel(0.75)),
    legend.text = element_text(size = rel(0.75)),
    legend.position = "bottom",
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6),
    legend.key.size = unit(.4, "cm")
    )

p

}

