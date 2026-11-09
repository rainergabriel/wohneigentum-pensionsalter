## 03  Print figures for the manuscript (German labels, no titles, "abfedern" not "puffern").
## Reads the ggplot .rds objects written by 02_analysis.R; writes WRITING/manuscript/figures/Abbildung1-3.png (300 dpi).
## Font: set FONT to "Arial" on a machine that has it.
suppressMessages({library(ggplot2); library(grid)})
P <- "output/"   # run from analysis/
FONT <- "DejaVu Sans"
th <- theme_minimal(base_size = 10, base_family = FONT) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(colour = "black"),
        legend.position = "top", legend.title = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0), plot.margin = margin(6, 10, 4, 4))
BLUE <- "#1F4E79"; GREY <- "#7F7F7F"

## ---- Abbildung 1 ----
d1 <- as.data.frame(lapply(as.data.frame(readRDS(paste0(P, "fig1_owner-coef.rds"))$data), function(v) if (is.factor(v)) as.character(v) else v), stringsAsFactors = FALSE)
d1$lab <- factor(c("M0\nWohnform", "M1\n+ Soziodemografie", "M2\n+ Gesundheit", "M3\n+ Region",
                   "M4\n+ objektive\nRessourcen", "M5\n+ subjektive\nEinschätzung"),
                 levels = c("M0\nWohnform", "M1\n+ Soziodemografie", "M2\n+ Gesundheit", "M3\n+ Region",
                            "M4\n+ objektive\nRessourcen", "M5\n+ subjektive\nEinschätzung"))
d1$xpos <- 1:6
p1 <- ggplot(d1, aes(xpos, est)) +
  geom_hline(yintercept = 0, colour = "black", linewidth = .3) +
  geom_vline(xintercept = 5.5, linetype = "dashed", colour = GREY) +
  geom_col(fill = BLUE, width = .55) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = .15, linewidth = .4) +
  geom_text(aes(y = ucl + .08, label = sub(".", ",", sprintf("%+.2f", est), fixed = TRUE)), size = 3, family = FONT) +
  annotate("text", x = 5.5, y = max(d1$ucl) + .25, label = "objektiv  |  subjektiv", size = 3, family = FONT, colour = GREY) +
  scale_y_continuous(limits = c(-0.3, max(d1$ucl) + .35), breaks = seq(0, 2, .5), labels = function(v) sub(".", ",", v, fixed = TRUE)) +
  scale_x_continuous(breaks = 1:6, labels = levels(d1$lab)) +
  labs(x = NULL, y = "Unterschied zu Mietenden (SWLS-Punkte, 95 %-KI)") + th
ggsave("../WRITING/manuscript/figures/Abbildung1.png", p1, width = 16, height = 9.5, units = "cm", dpi = 300, bg = "white")

## ---- Abbildung 2 ----
d2 <- as.data.frame(lapply(as.data.frame(readRDS(paste0(P, "fig2_owner-x-strain.rds"))$data), function(v) if (is.factor(v)) as.character(v) else v), stringsAsFactors = FALSE)
d2$Wohnform <- factor(ifelse(grepl("^Mieter", d2$owner), "Mietende", "Eigentümerinnen und Eigentümer"),
                      levels = c("Mietende", "Eigentümerinnen und Eigentümer"))
d2$Lage <- factor(ifelse(grepl("^nicht", d2$strain), "nicht einkommensarm", "einkommensarm"),
                  levels = c("nicht einkommensarm", "einkommensarm"))
d2$xp <- as.numeric(d2$Lage)
p2 <- ggplot(d2, aes(xp, swls, colour = Wohnform, group = Wohnform)) +
  geom_line(position = position_dodge(.25), linewidth = .5) +
  geom_pointrange(aes(ymin = lcl, ymax = ucl), position = position_dodge(.25), size = .5, linewidth = .5) +
  geom_text(aes(label = sub(".", ",", sprintf("%.1f", swls), fixed = TRUE)), position = position_dodge(.25), vjust = -1.1, size = 3, family = FONT, show.legend = FALSE) +
  scale_colour_manual(values = c(GREY, BLUE)) +
  scale_y_continuous(limits = c(24, 30), breaks = 24:30) +
  scale_x_continuous(breaks = 1:2, labels = levels(d2$Lage), limits = c(.6, 2.4)) +
  labs(x = NULL, y = "Lebenszufriedenheit (SWLS, Mittelwert, 95 %-KI)") + th
ggsave("../WRITING/manuscript/figures/Abbildung2.png", p2, width = 12, height = 9, units = "cm", dpi = 300, bg = "white")

## ---- Abbildung 3 ----
d3 <- as.data.frame(lapply(as.data.frame(readRDS(paste0(P, "fig3_owner-burden.rds"))$data), function(v) if (is.factor(v)) as.character(v) else v), stringsAsFactors = FALSE)
d3$panel <- ifelse(grepl("Belehnung", d3$panel), "Belehnungsgrad", "Zinslast in % des Haushaltseinkommens")
d3$xl <- c("keine\nHypothek", "bis 33 %", "33–66 %", "über 66 %", "keine\nHypothek", "bis 15 %", "15–30 %", "über 30 %")
d3$xl <- factor(d3$xl, levels = unique(d3$xl))
p3 <- ggplot(d3, aes(xl, swls)) +
  geom_pointrange(aes(ymin = lcl, ymax = ucl), colour = BLUE, size = .5, linewidth = .5) +
  facet_wrap(~panel, scales = "free_x") +
  scale_y_continuous(limits = c(27, 31), breaks = 27:31) +
  labs(x = NULL, y = "Lebenszufriedenheit (SWLS, Mittelwert, 95 %-KI)") + th
ggsave("../WRITING/manuscript/figures/Abbildung3.png", p3, width = 16, height = 8, units = "cm", dpi = 300, bg = "white")
cat("ok\n")
