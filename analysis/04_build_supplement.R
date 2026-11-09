## =============================================================================
## 04  Build the GitHub supplement (D-SUPPLEMENT) — the manuscript has no appendix;
## all tables live on a public GitHub page instead. Regenerates `../supplement/`
## from the output CSVs + manuscript figures, so the page is reproducible after any
## rerun. Run AFTER 02 / 02c / 03.  Rscript 04_build_supplement.R
## No microdata, no .rds, no raw numbers beyond the aggregated tables.
## =============================================================================

SUP <- "../supplement"
REPO_URL <- "https://github.com/<user>/wohneigentum-pensionsalter"   # <-- Rainer: replace <user>
dir.create(file.path(SUP, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(SUP, "code"),    recursive = TRUE, showWarnings = FALSE)
rd <- function(f) read.csv(file.path("output", f), check.names = FALSE)

## --- markdown helpers --------------------------------------------------------
md_table <- function(df) {
  df[] <- lapply(df, function(x) { x <- as.character(x); x[is.na(x) | x == "NA"] <- ""; x })
  hdr <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", ncol(df)), collapse = " | "), " |")
  rows <- vapply(seq_len(nrow(df)), function(i) paste0("| ", paste(df[i, ], collapse = " | "), " |"), "")
  paste(c(hdr, sep, rows), collapse = "\n")
}
star <- function(p) ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
dc  <- function(x, d = 2) formatC(round(as.numeric(x), d), format = "f", digits = d)   # fixed decimals

## =============================================================================
## S1 — Deskription (3 Blöcke, inkl. PK-Vorbezug)
## =============================================================================
t1 <- rd("tab1_descriptives.csv")
s1 <- t1[, c("block","variable","Gesamt","Miete","Eig_ohne","Eig_mit")]
names(s1) <- c("Block","Merkmal (gewichtet)","Gesamt","Miete","Eigentum o. w. RE","Eigentum m. w. RE")

## =============================================================================
## S2 — Nested models M0-M5 (tenure3 + binär), mit Sternen, N, R²; + MI-Zeilen
## =============================================================================
t2 <- rd("tab2_nested-models.csv"); t2c <- rd("tab2c_MI.csv")
mkcell <- function(est, p, se) paste0(dc(est), star(p), " (", dc(se), ")")
s2 <- data.frame(
  Spezifikation = t2$spec, Modell = t2$model,
  `Wohnform (vs. Mietende)` = gsub("tenure3", "", t2$term),
  `b (SE)` = mkcell(t2$est, t2$p, t2$se), `95%-KI` = paste0("[", dc(t2$lcl), "; ", dc(t2$ucl), "]"),
  R2 = dc(t2$R2, 3), N = t2$N, check.names = FALSE)
s2mi <- data.frame(Modell = t2c$model, `Wohnform (vs. Mietende)` = gsub("tenure3", "", t2c$term),
  `b (SE), MI m=20` = paste0(dc(t2c$est_MI), " (", dc(t2c$se_MI), ")"), check.names = FALSE)

## S2b — vollständige Regressionsübersicht (schon im wide-Format)
s2b <- rd("tab2b_full-controls.csv")
names(s2b)[1] <- "Term"

## =============================================================================
## S3 — Interaktion owner × Einkommensarmut
## =============================================================================
t3 <- rd("tab3_interaction.csv")
s3 <- data.frame(Block = t3$block, Term = t3$term, `b (SE)` = mkcell(t3$est, t3$p, t3$se),
  `95%-KI` = paste0("[", dc(t3$lcl), "; ", dc(t3$ucl), "]"), p = dc(t3$p, 3), check.names = FALSE)

## =============================================================================
## S4 — Innerhalb der Eigentümer (inkl. Last > 30% mit n)
## =============================================================================
t4 <- rd("tab4_owner-burden.csv")
s4 <- data.frame(Fokus = t4$block, Term = t4$term, `b (SE)` = mkcell(t4$est, t4$p, t4$se),
  p = dc(t4$p, 3), n = t4$n, Anmerkung = ifelse(is.na(t4$note), "", t4$note), check.names = FALSE)

## =============================================================================
## S5 — PK-Vorbezug: Ressourcen + schrittweise SWLS-Modelle
## =============================================================================
t5r <- rd("tab_pkvorbezug_resources.csv"); t5s <- rd("tab_pkvorbezug_swls.csv")
s5r <- data.frame(`Kennzahl (Eigentümer 65+)` = t5r$metric, `kein PK-Vorbezug` = t5r$kein_PK,
  `PK-Vorbezug` = t5r$PK_Vorbezug, p = dc(t5r$p, 3), check.names = FALSE)
s5s <- data.frame(Modell = t5s$modell, `b (PK-Vorbezug)` = dc(t5s$pil2_home_b), SE = dc(t5s$se),
  p = dc(t5s$p, 3), n = t5s$n, check.names = FALSE)

## =============================================================================
## S6 — Robustheit (inkl. Altersgruppen 65-79 / 80+)
## =============================================================================
t6 <- rd("tab5_robustness.csv")
s6 <- data.frame(Spezifikation = t6$spec, Term = t6$term, b = dc(t6$est), SE = dc(t6$se), check.names = FALSE)

## =============================================================================
## Assemble README.md
## =============================================================================
anm <- function(x) paste0("\n*Anmerkung: ", x, "*\n")
md <- c(
"# Wohneigentum im Pensionsalter — Online-Supplement",
"",
"**Sonnen- und Schattenseiten des Eigenheims im Rentenalter: Eigentum, objektive Ressourcen und subjektives Wohlbefinden (Swiss Aging Survey 2022, 65+).**",
"",
"Rainer Gabriel & Selina N.N. (ZHAW). Supplement zum Manuskript; ersetzt den Anhang.",
"",
"## Daten & Methoden (Kurz)",
"",
paste0("Swiss Aging Survey 2022 (SAS, ReleaseB), Personen ab 65 (analytische n = 3'313; ",
"gemeinsame Complete-Case-Stichprobe der genesteten Modelle n = ", t2$N[1], "). Gewichtet ",
"(Designgewicht, kantonale Schichtung). Zielgrösse: Diener-SWLS (5–35). Die finanzielle Lage ",
"wird getrennt in einen **objektiven** Block (äquivalisiertes Einkommen, Finanzvermögens-Terzile ",
"mit einer „fehlend\"-Kategorie; M4) und einen **subjektiven** Block (mit dem Einkommen auskommen, ",
"unerwartete Ausgabe decken; M5) — Letzterer erklärt nur, klassiert aber nie (Gabriel & Hümbelin 2026). ",
"Owner-spezifische Grössen (Hypothek, Belehnungsgrad, PK-Vorbezug, imputierte Miete) werden separat ",
"innerhalb der Eigentümer untersucht (S4/S5). Vermögens-Item-Nonresponse wird zusätzlich per ",
"multipler Imputation (mice, m = 20) adressiert. Keine Mikrodaten im Supplement."),
"",
"Reproduzierbar: der Code (`code/`) leitet alle Silver-Variablen direkt aus dem Rohdatensatz ab; die ",
"Mikrodaten sind über SWISSUbase / Pro Senectute erhältlich und hier **nicht** enthalten.",
"",
"---",
"",
"## S1 — Deskription nach Wohnform",
md_table(s1), anm("Gewichtet. CHF-Werte sind Mediane, übrige Zellen Anteile in %. N ungewichtet."),
"",
"## S2 — Genestete Modelle M0–M5 (SWLS)",
md_table(s2),
"",
"### S2 — Multiple Imputation (m = 20), Vermögensblock",
md_table(s2mi),
anm("Gewichtete lineare Regression auf SWLS. Sterne: * p<.05, ** p<.01, *** p<.001. CC = Complete-Case; MI = multiple Imputation (m=20) des Vermögens-/Einkommensblocks."),
"",
"## S2b — Vollständige Regressionsübersicht (alle Koeffizienten, M0–M5)",
md_table(s2b), anm("Zellen: b<Sterne> (SE). Leerzellen = Variable in diesem Modell nicht enthalten."),
"",
"## S3 — Interaktion Wohnform × Einkommensarmut",
md_table(s3), anm("Einkommensarmut = objektive Einkommensarmut (Silver-Armutsgrenze). Mit dem M4- bzw. M5-Set."),
"",
"## S4 — Innerhalb der Eigentümer: Immobilienbelastung/-vorteil",
md_table(s4), anm("Gewichtete OLS auf SWLS, Eigentümer 65+, mit dem M4-Kontrollsatz (die genannte Grösse ist der Regressor). Ein Modell je Zeile plus gemeinsames Modell."),
"",
"## S5 — PK-Vorbezug fürs Eigenheim",
"### S5a — PK-Vorbezug und finanzielle Ressourcen",
md_table(s5r), anm("Gewichtet, Eigentümer 65+. CHF = Mediane, übrige Anteile in %."),
"### S5b — PK-Vorbezug und Lebenszufriedenheit, schrittweise um Ressourcen erweitert",
md_table(s5s), anm("Der Koeffizient attenuiert, wenn die finanzielle Lage kontrolliert wird."),
"",
"## S6 — Robustheit",
md_table(s6), anm("Eigentümer-Koeffizient (bzw. Interaktion) unter alternativen Spezifikationen; inkl. Altersgruppen 65–79 / 80+ (im Manuskript nicht berichtet)."),
"",
"---",
"",
"## Abbildungen",
"Siehe `figures/` (PNG + PDF): Abbildung 1 (Eigentumsvorteil M0–M5, zwei Eigentümergruppen), ",
"Abbildung 2 (Wohnform × Einkommensarmut), Abbildung 3 (Belehnungsgrad/Hypothekenlast), ",
"Abbildung 4 (PK-Vorbezug).",
"",
"## Code",
"`code/` enthält `01_setup-data.R`, `02_analysis.R`, `02c_pk-vorbezug.R`, `03_figures_print.R` ",
"(Mikrodaten nicht enthalten; `SAS_DATA_PATH` auf die `.sav` setzen zum Reproduzieren).",
"",
paste0("Repository: ", REPO_URL, "  ·  Generiert mit `analysis/04_build_supplement.R`."),
"")
writeLines(md, file.path(SUP, "README.md"))

## --- figures: copy manuscript PNG + PDF --------------------------------------
figsrc <- "../WRITING/manuscript/figures"
for (f in c("Abbildung1","Abbildung2","Abbildung3","Abbildung4"))
  for (ext in c("png","pdf")) {
    src <- file.path(figsrc, paste0(f, ".", ext))
    if (file.exists(src)) file.copy(src, file.path(SUP, "figures", basename(src)), overwrite = TRUE)
  }

## --- code: copy scripts with a microdata header ------------------------------
hdr <- c("## ---------------------------------------------------------------------------",
         "## Supplement zu \"Wohneigentum im Pensionsalter\" (Gabriel & N.N., SAS 2022, 65+).",
         "## Die Mikrodaten (SAS 2022 ReleaseB) sind NICHT enthalten — erhältlich via",
         "## SWISSUbase / Pro Senectute. SAS_DATA_PATH auf die .sav setzen zum Reproduzieren.",
         "## Kanonische Quelle + Historie: siehe Projekt-Repository.",
         "## ---------------------------------------------------------------------------", "")
for (f in c("01_setup-data.R","00_build-silver.R","02_analysis.R","02c_pk-vorbezug.R","03_figures_print.R"))
  if (file.exists(f)) writeLines(c(hdr, readLines(f)), file.path(SUP, "code", f))

## --- .gitignore + a short note for Rainer ------------------------------------
writeLines(c("# Supplement: nur Code, Tabellen (README) und Abbildungen — keine Mikrodaten.",
             "*.sav", "*.dta", "*.rds", "*.RData", "data/", ".DS_Store", ".Rhistory"),
           file.path(SUP, ".gitignore"))
cat("Supplement gebaut ->", normalizePath(SUP), "\n")
cat("  README.md + figures/ (8 Dateien) + code/ (5 Skripte) + .gitignore\n")
cat("  Repo-Platzhalter in README.md:", REPO_URL, "  (Rainer: <user> ersetzen)\n")
