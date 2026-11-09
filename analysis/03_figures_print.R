## 03  Print figures for the manuscript (German labels, no titles, "abfedern" not "puffern").
## Reads the ggplot .rds objects written by 02_analysis.R / 02c_pk-vorbezug.R; writes WRITING/manuscript/figures/Abbildung1-4.png (300 dpi).
## Font: set FONT to "Arial" on a machine that has it.
suppressMessages({library(ggplot2); library(grid)})
P <- "output/"   # run from analysis/
FONT <- "Helvetica"   # clean sans (Arial-metric); renders to PNG AND cairo_pdf on macOS.
                      # (DejaVu Sans is Linux-only -> was silently substituted + broke PDF export.)
th <- theme_minimal(base_size = 10, base_family = FONT) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(colour = "black"),
        legend.position = "top", legend.title = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0), plot.margin = margin(6, 14, 4, 4))
BLUE <- "#1F4E79"; GREY <- "#7F7F7F"
## save every manuscript figure as PNG (web/Word) AND PDF (vector, for the journal).
## PDF via the macOS-native quartz(type="pdf") device — cairo fails to load after the
## PNG device in-session here. quartz sizes are in inches, so convert cm -> inch.
save2 <- function(p, name, w, h) {
  base <- paste0("../WRITING/manuscript/figures/", name)
  ggsave(paste0(base, ".png"), p, width = w, height = h, units = "cm", dpi = 300, bg = "white")
  pdf_dev <- if (capabilities("aqua")) function(f, wi, he) grDevices::quartz(type = "pdf", file = f, width = wi, height = he)
             else function(f, wi, he) grDevices::cairo_pdf(f, width = wi, height = he)
  pdf_dev(paste0(base, ".pdf"), w/2.54, h/2.54); print(p); dev.off() }

## ---- Abbildung 1: zwei Eigentümergruppen (ohne / mit weitere Immobilie) vs Mietende, M0-M5 ----
t2 <- read.csv(paste0(P, "tab2_nested-models.csv"), check.names = FALSE)
d1 <- t2[t2$spec == "tenure3", ]
d1$grp <- factor(ifelse(grepl("no other RE", d1$term), "Eigentum ohne weitere Immobilie", "Eigentum mit weiterer Immobilie"),
                 levels = c("Eigentum ohne weitere Immobilie", "Eigentum mit weiterer Immobilie"))
d1$xpos <- as.integer(factor(d1$model, levels = c("M0","M1","M2","M3","M4","M5")))
modlab <- c("M0\nWohnform", "M1\n+ Soziodemografie", "M2\n+ Gesundheit", "M3\n+ Region",
            "M4\n+ objektive\nRessourcen", "M5\n+ subjektive\nEinschätzung")
p1 <- ggplot(d1, aes(xpos, est, colour = grp, group = grp)) +
  geom_hline(yintercept = 0, colour = "black", linewidth = .3) +
  geom_vline(xintercept = 5.5, linetype = "dashed", colour = GREY) +
  geom_pointrange(aes(ymin = lcl, ymax = ucl), position = position_dodge(.45), size = .4, linewidth = .45) +
  geom_text(aes(label = sub(".", ",", sprintf("%.2f", est), fixed = TRUE)), position = position_dodge(.45),
            hjust = -0.25, size = 2.5, family = FONT, show.legend = FALSE) +
  annotate("text", x = 5.5, y = max(d1$ucl) + .18, label = "objektiv  |  subjektiv", size = 3, family = FONT, colour = GREY) +
  scale_colour_manual(values = c("Eigentum ohne weitere Immobilie" = GREY, "Eigentum mit weiterer Immobilie" = BLUE)) +
  scale_y_continuous(labels = function(v) sub(".", ",", v, fixed = TRUE)) +
  scale_x_continuous(breaks = 1:6, labels = modlab, expand = expansion(add = .5)) +
  labs(x = NULL, y = "Unterschied zu Mietenden (SWLS-Punkte, 95 %-KI)") + th
save2(p1, "Abbildung1", 16, 9.5)

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
save2(p2, "Abbildung2", 12, 9)

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
save2(p3, "Abbildung3", 16, 8)

## ---- Abbildung 4: PK-Vorbezug ----
d4 <- as.data.frame(lapply(as.data.frame(readRDS(paste0(P, "fig_pkvorbezug.rds"))$data), function(v) if (is.factor(v)) as.character(v) else v), stringsAsFactors = FALSE)
d4$grp <- factor(ifelse(grepl("^kein", d4$grp), "kein PK-Vorbezug", "PK-Vorbezug für das Eigenheim"),
                 levels = c("kein PK-Vorbezug", "PK-Vorbezug für das Eigenheim"))
lab <- c("noch mit Hypothek" = "Hat noch eine Hypothek", "auskommen: leicht" = "Kommt problemlos mit dem Einkommen aus",
         "kann unerwartete Ausgabe decken" = "Könnte eine unerwartete Ausgabe bezahlen", "Einkommensarm" = "Einkommensarm")
d4$metric <- factor(lab[d4$metric], levels = rev(unname(lab)))
p4 <- ggplot(d4, aes(val, metric, fill = grp)) +
  geom_col(position = position_dodge(.7), width = .62) +
  geom_text(aes(label = paste0(sub(".", ",", sprintf("%.0f", val), fixed = TRUE), " %")), position = position_dodge(.7), hjust = -0.15, size = 3, family = FONT) +
  scale_fill_manual(values = c(GREY, BLUE)) +
  scale_x_continuous(limits = c(0, 110), expand = c(0, 0), breaks = seq(0, 100, 25)) +
  labs(x = "Anteil in % (Eigentümerinnen und Eigentümer ab 65, gewichtet)", y = NULL) +
  th + theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(), legend.position = "top")
save2(p4, "Abbildung4", 16, 8.5)
cat("ok: Abbildung 1-4 (PNG + PDF)\n")
