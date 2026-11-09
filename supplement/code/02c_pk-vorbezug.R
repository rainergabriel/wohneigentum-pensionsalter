## ---------------------------------------------------------------------------
## Supplement zu "Wohneigentum im Pensionsalter" (Gabriel & N.N., SAS 2022, 65+).
## Die Mikrodaten (SAS 2022 ReleaseB) sind NICHT enthalten — erhältlich via
## SWISSUbase / Pro Senectute. SAS_DATA_PATH auf die .sav setzen zum Reproduzieren.
## Kanonische Quelle + Historie: siehe Projekt-Repository.
## ---------------------------------------------------------------------------

## =============================================================================
## 02c  PK-Vorbezug fürs Eigenheim — finanzielle Ressourcen & Zufriedenheit
## -----------------------------------------------------------------------------
## Owners 65+, weighted (design weight w, strata strata_22). Run 01_setup-data.R first.
## Question (Rainer): the 2nd-pillar ADVANCE WITHDRAWAL for the home (`pil2_home`,
## WEF-Vorbezug) draws down retirement capital years before retirement.
##   (1) Do owners who did it end up with FEWER financial resources in old age?
##   (2) Does that then relate to life satisfaction — i.e. does any PK-Vorbezug↔SWLS
##       link run THROUGH the resource channel?
## Outputs: tab_pkvorbezug_resources.csv, tab_pkvorbezug_swls.csv, fig_pkvorbezug.png
## =============================================================================

suppressWarnings(suppressMessages({ library(survey); library(dplyr); library(ggplot2); library(tidyr) }))
options(survey.lonely.psu = "adjust"); set.seed(20221029)
BLUE <- "#2b6cb0"; GREY <- "#9fb6cf"; ORANGE <- "#dd6b20"
d <- readRDS("data/wohnen_working.rds")

## owners 65+ with a known PK-Vorbezug status
dOwn <- d[which(d$owner == 1 & !is.na(d$w) & !is.na(d$pil2_home)), ]
dOwn$log_eqinc <- log(pmax(dOwn$eqinc, 1))
dOwn$pk_f <- factor(dOwn$pil2_home, 0:1, c("kein PK-Vorbezug", "PK-Vorbezug fürs Eigenheim"))
desOwn <- svydesign(ids = ~1, strata = ~strata_22, weights = ~w, data = dOwn, nest = TRUE)
nPK  <- sum(dOwn$pil2_home == 1); nNo <- sum(dOwn$pil2_home == 0)
cat(sprintf("Owners 65+ with PK status: n=%d (PK-Vorbezug %d / kein %d)\n", nrow(dOwn), nPK, nNo))

wmean_g <- function(v) {                         # weighted mean by pil2_home (+ design-based p)
  b <- as.data.frame(svyby(as.formula(paste0("~", v)), ~pil2_home, desOwn, svymean, na.rm = TRUE))
  p <- tryCatch(summary(svyglm(as.formula(paste0(v, " ~ pil2_home")), design = desOwn))$coefficients["pil2_home", 4],
                error = function(e) NA_real_)
  c(no = b[b$pil2_home == 0, 2], pk = b[b$pil2_home == 1, 2], p = p) }
wmed_g <- function(v) {                          # weighted median by pil2_home (CHF)
  q <- function(sub) as.numeric(svyquantile(as.formula(paste0("~", v)), sub, 0.5, ci = FALSE, na.rm = TRUE)[[1]][1])
  p <- tryCatch(summary(svyglm(as.formula(paste0(v, " ~ pil2_home")), design = desOwn))$coefficients["pil2_home", 4],
                error = function(e) NA_real_)
  c(no = q(subset(desOwn, pil2_home == 0)), pk = q(subset(desOwn, pil2_home == 1)), p = p) }

## =============================================================================
## PART 1 — PK-Vorbezug and financial resources (owners, weighted)
## =============================================================================
rows <- list(
  "Äquiv.-Einkommen Median (CHF)"        = c("med","eqinc"),
  "Finanzvermögen Median (CHF)"          = c("med","wealth_fin"),
  "Nettovermögen Median (CHF)"           = c("med","wealth_net"),
  "Hauswert Median (CHF)"                = c("med","home_value"),
  "Einkommensarm (%)"                    = c("bin","strain"),
  "kann unerwartete Ausgabe decken (%)"  = c("bin","suddenexp_subj"),
  "auskommen: leicht (%)"                = c("bin","endsmeet_easy"),
  "noch mit Hypothek (%)"                = c("bin","has_mortgage"),
  "Belehnungsgrad (LTV) Median"          = c("med","ltv"),
  "Hypothekenlast % Eink. Median"        = c("med","mortgage_burden"))
dOwn$endsmeet_easy <- as.integer(dOwn$endsmeet_f == "easily")
desOwn <- svydesign(ids = ~1, strata = ~strata_22, weights = ~w, data = dOwn, nest = TRUE)  # refresh (new col)
res <- do.call(rbind, lapply(names(rows), function(lab) {
  spec <- rows[[lab]]; v <- spec[2]
  r <- if (spec[1] == "med") wmed_g(v) else { x <- wmean_g(v); c(no = 100*x["no"], pk = 100*x["pk"], p = x["p"]) }
  sc <- if (lab == "Hypothekenlast % Eink. Median") 100 else 1   # burden to %
  data.frame(metric = lab, kein_PK = round(sc*r[1], if (spec[1]=="med" && sc==1 && !grepl("LTV", lab)) 0 else 2),
             PK_Vorbezug = round(sc*r[2], if (spec[1]=="med" && sc==1 && !grepl("LTV", lab)) 0 else 2),
             diff = round(sc*(r[2]-r[1]), if (spec[1]=="med" && sc==1 && !grepl("LTV", lab)) 0 else 2),
             p = round(r[3], 3), row.names = NULL) }))
write.csv(res, "output/tab_pkvorbezug_resources.csv", row.names = FALSE)
cat("Part 1 done: tab_pkvorbezug_resources.csv\n"); print(res, row.names = FALSE)

## =============================================================================
## PART 2 — satisfaction (SWLS): does the PK-Vorbezug link run through resources?
## Nested within-owner models; watch the pil2_home coefficient as we add resources.
## =============================================================================
dOwn$wealth_fin_tert <- d$wealth_fin_tert[match(dOwn$id, d$id)]
desOwn <- svydesign(ids = ~1, strata = ~strata_22, weights = ~w, data = dOwn, nest = TRUE)
socdem <- "age + female + civstat + alone + edu3 + swiss + haskids + gali_any + dep_score + srh_poor + lingreg + degurba"
mods <- list(
  "mA: roh (nur PK)"              = "swls ~ pil2_home",
  "mB: + Soziodem./Gesundh./Reg." = paste("swls ~ pil2_home +", socdem),
  "mC: + objektive Finanzen"      = paste("swls ~ pil2_home +", socdem, "+ log_eqinc + wealth_fin_tert"),
  "mD: + subjektive Lage (=voll)" = paste("swls ~ pil2_home +", socdem, "+ log_eqinc + wealth_fin_tert + endsmeet_f + suddenexp_subj"))
swls_tab <- do.call(rbind, lapply(names(mods), function(nm) {
  fit <- svyglm(as.formula(mods[[nm]]), design = desOwn)
  s <- summary(fit)$coefficients["pil2_home", ]
  data.frame(modell = nm, pil2_home_b = round(s[1], 3), se = round(s[2], 3),
             lcl = round(s[1]-1.96*s[2], 3), ucl = round(s[1]+1.96*s[2], 3), p = round(s[4], 3),
             n = length(fit$y), row.names = NULL) }))
write.csv(swls_tab, "output/tab_pkvorbezug_swls.csv", row.names = FALSE)
cat("\nPart 2 done: tab_pkvorbezug_swls.csv\n"); print(swls_tab, row.names = FALSE)

## =============================================================================
## Figure — the DISCRIMINATING picture (single %-panel): PK-Vorbezüger are more
## leveraged and feel less at ease, despite similar income/wealth (medians -> table)
## =============================================================================
keep <- c("noch mit Hypothek (%)","auskommen: leicht (%)",
          "kann unerwartete Ausgabe decken (%)","Einkommensarm (%)")
gdf <- res[res$metric %in% keep, ] %>% select(metric, kein_PK, PK_Vorbezug) %>%
  pivot_longer(c(kein_PK, PK_Vorbezug), names_to = "grp", values_to = "val") %>%
  mutate(grp = factor(grp, c("kein_PK","PK_Vorbezug"), c("kein PK-Vorbezug","PK-Vorbezug fürs Eigenheim")),
         metric = factor(sub(" \\(%\\)", "", metric),
                         levels = rev(c("Einkommensarm","kann unerwartete Ausgabe decken",
                                        "auskommen: leicht","noch mit Hypothek"))))
p <- ggplot(gdf, aes(val, metric, fill = grp)) +
  geom_col(position = position_dodge(.65), width = .6) +
  geom_text(aes(label = sprintf("%.0f%%", val)), position = position_dodge(.65), hjust = -0.15, size = 3) +
  scale_fill_manual(values = c("kein PK-Vorbezug" = GREY, "PK-Vorbezug fürs Eigenheim" = BLUE), name = NULL) +
  scale_x_continuous(limits = c(0, 108), expand = c(0, 0)) +
  labs(x = "Gewichteter Anteil (%)", y = NULL,
       title = "PK-Vorbezug fürs Eigenheim: höher verschuldet, subjektiv knapper",
       subtitle = sprintf("Eigentümer 65+, gewichtet — gleiches Einkommen/Vermögen (Mediane: Tabelle). PK n=%d vs. kein n=%d.", nPK, nNo)) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom", panel.grid.major.y = element_blank())
ggsave("output/fig_pkvorbezug.png", p, width = 9, height = 4.5, dpi = 300)
saveRDS(p, "output/fig_pkvorbezug.rds")
cat("\nFigure done: fig_pkvorbezug.png\n")
