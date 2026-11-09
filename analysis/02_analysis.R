## =============================================================================
## 02  Analysis — Wohnen und Wohnbefinden (SAS 2022), 65+   [REVISION 2]
## -----------------------------------------------------------------------------
## Selina's well-being paper. Run 01_setup-data.R first (data/wohnen_working.rds).
## Weighted throughout (design weight `w`, strata `strata_22`, survey package).
## Outcome = SWLS (Diener, 5-35). Steps 2-6 of
## HANDOFF/HANDOFF_revision-2_objective-subjective_2026-11-09.md:
##   Step 2  tab1   descriptives in 3 blocks (sociodem / OBJECTIVE / SUBJECTIVE)
##   Step 3  tab2   nested M0-M5 (objective resources M4 | subjective M5), CC|MI
##           fig1   owner coef across M0-M5, objektiv|subjektiv divider
##           tab11  decomposition on the new order
##   Step 4  tab3   M4 + owner×strain (strain = objective income poverty); fig2 2×2
##   Step 5  tab4   inside owners: mortgage / LTV / burden / imputed net rent; fig3
##   Step 6  tab5   robustness; MI (mice m=20) PRIMARY for the wealth block
## KEY PRINCIPLE (Gabriel & Hümbelin 2026): endsmeet / suddenexp are SUBJECTIVE —
## they enter only M5 and never classify strain. OBJECTIVE = CHF amounts & facts.
## Paper-2 material (poverty / EL / imputed-rent poverty) is in 02b_paper2.R.
## =============================================================================

suppressWarnings(suppressMessages({
  library(survey); library(dplyr); library(ggplot2); library(tidyr)
}))
options(survey.lonely.psu = "adjust"); set.seed(20221029)
dir.create("output", showWarnings = FALSE)
BLUE <- "#2b6cb0"; GREY <- "#9fb6cf"; ORANGE <- "#dd6b20"
MI_M <- as.integer(Sys.getenv("MI_M", unset = "20"))   # mice implicates (override for smoke-tests)
d <- readRDS("data/wohnen_working.rds")

## ---- model inputs -----------------------------------------------------------
d$log_eqinc      <- log(pmax(d$eqinc, 1))
## Imputed rent is 0 for EVERY tenant by construction, so raw log1p(imp_rent_eq) is
## near-collinear with the tenure dummy and inflates the owner SE ~6× in M4/M5
## (D-IMPRENT-CENTER). Center WITHIN owners: the owner dummy then carries the mean
## rent-free benefit and this term the within-owner home-value gradient (orthogonal
## to the dummy, so the headline owner coefficient stays estimable). Tenants = 0.
ir_center <- function(ir, owner, w) {
  raw <- log1p(pmax(ir, 0)); m <- weighted.mean(raw[owner == 1], w[owner == 1], na.rm = TRUE)
  ifelse(owner == 1, raw - m, 0) }
d$log_imprent_eq <- ir_center(d$imp_rent_eq, d$owner, d$w)
d$female_f       <- factor(d$female, 0:1, c("man","woman"))
d$owner_re       <- as.integer(d$tenure3 == "owner + other RE")  # owner WITH other real estate
d$wealth_fin_miss <- as.integer(is.na(d$wealth_fin))             # item non-response on wealth

mkdes <- function(df) svydesign(ids = ~1, strata = ~strata_22, weights = ~w, data = df, nest = TRUE)
ci_tab <- function(fit, keep = NULL, label = "") {
  s <- summary(fit)$coefficients
  o <- data.frame(term = rownames(s), est = s[,1], se = s[,2],
                  lcl = s[,1] - 1.96*s[,2], ucl = s[,1] + 1.96*s[,2],
                  p = s[,4], model = label, row.names = NULL)
  if (!is.null(keep)) o <- o[grepl(keep, o$term), ]
  o
}
wmed <- function(des, v) as.numeric(svyquantile(as.formula(paste0("~", v)), des, 0.5, ci = FALSE, na.rm = TRUE)[[1]][1])

## =============================================================================
## STEP 2. Table 1 — descriptives in three labelled blocks  -> tab1
## columns: Gesamt | Miete | Eigentum ohne weitere RE | Eigentum mit weiterer RE
## =============================================================================
des_all <- mkdes(d[!is.na(d$w), ])
TENLV <- c("tenant","owner, no other RE","owner + other RE")

## weighted % of a 0/1 indicator by tenure3 + total (robust to constant subgroups —
## svymean on the numeric 0/1, NOT factor(), so an all-0 tenant group is fine)
wbin <- function(des, v, lab) {
  tot <- 100*as.numeric(svymean(as.formula(paste0("~", v)), des, na.rm = TRUE)[1])
  by  <- as.data.frame(svyby(as.formula(paste0("~", v)), ~tenure3, des, svymean, na.rm = TRUE)); rownames(by) <- by$tenure3
  data.frame(variable = lab, level = "", Gesamt = tot,
             Miete = 100*by["tenant", 2], Eig_ohne = 100*by["owner, no other RE", 2], Eig_mit = 100*by["owner + other RE", 2]) }
wmed_by <- function(des, v, lab, ownersonly = FALSE, scale = 1) {  # weighted median by tenure3 + total
  gr <- if (ownersonly) TENLV[2:3] else TENLV
  tot <- scale*wmed(des, v)
  by  <- setNames(lapply(TENLV, function(t) if (t %in% gr) scale*wmed(subset(des, tenure3 == t), v) else NA), TENLV)
  data.frame(variable = lab, level = "", Gesamt = tot,
             Miete = by[["tenant"]], Eig_ohne = by[["owner, no other RE"]], Eig_mit = by[["owner + other RE"]]) }
wmean_by <- function(des, v, lab) {                # weighted mean by tenure3 + total
  tot <- as.numeric(svymean(as.formula(paste0("~", v)), des, na.rm = TRUE)[1])
  by  <- as.data.frame(svyby(as.formula(paste0("~", v)), ~tenure3, des, svymean, na.rm = TRUE)); rownames(by) <- by$tenure3
  data.frame(variable = lab, level = "", Gesamt = tot,
             Miete = by["tenant", 2], Eig_ohne = by["owner, no other RE", 2], Eig_mit = by["owner + other RE", 2]) }

## 0/1 indicators for the specific factor levels we report as % (robust)
d$edu_tertiary    <- as.integer(d$edu3 == "tertiary")
d$endsmeet_easy   <- as.integer(d$endsmeet_f == "easily")
d$endsmeet_hard   <- as.integer(d$endsmeet_f == "great difficulty")
des_all <- mkdes(d[!is.na(d$w), ])                 # rebuild with the new indicator cols
des_ow  <- subset(des_all, owner == 1)             # owners (home/LTV/imputed rent)
des_owm <- subset(des_all, has_mortgage == 1)      # owners with a mortgage (sum/burden)

blockA <- rbind(                                              # Soziodemografie & Gesundheit
  wmean_by(des_all, "age",        "Alter (Mittel)"),
  wbin(des_all, "female",         "Frauen (%)"),
  wbin(des_all, "alone",          "allein lebend (%)"),
  wbin(des_all, "edu_tertiary",   "Tertiärbildung (%)"),
  wbin(des_all, "srh_poor",       "(sehr) schlechte Gesundheit (%)"))
blockB <- rbind(                                             # OBJEKTIVE Ressourcen
  wmed_by(des_all, "eqinc",       "Äquiv.-Einkommen Median (CHF)"),
  wmed_by(des_all, "wealth_fin",  "Finanzvermögen Median (CHF)"),
  wmed_by(des_all, "wealth_net",  "Nettovermögen Median (CHF)"),
  wbin(des_all,    "wealth_fin_miss","fehlende Vermögensangabe (%)"),
  wbin(des_all,    "has_mortgage",   "mit Hypothek (%)"),
  wmed_by(des_owm, "mortgage_amt",   "Hypothekarsumme Median (CHF; mit Hyp.)", ownersonly = TRUE),
  wmed_by(des_ow,  "ltv",            "Belehnungsgrad (LTV) Median (Eigent.)",  ownersonly = TRUE),
  wmed_by(des_owm, "mortgage_burden","Hypothekenlast % Eink. Median (mit Hyp.)", ownersonly = TRUE, scale = 100),
  wmed_by(des_ow,  "home_value",     "Hauswert Median (Eigent.; CHF)",         ownersonly = TRUE),
  wmed_by(des_ow,  "imp_rent_eq",    "imput. Miete äquiv. Median (CHF/Mt)",    ownersonly = TRUE))
blockC <- rbind(                                             # SUBJEKTIVE Einschätzung
  wbin(des_all, "endsmeet_easy",  "auskommen: leicht (%)"),
  wbin(des_all, "endsmeet_hard",  "auskommen: grosse Mühe (%)"),
  wbin(des_all, "suddenexp_subj", "kann unerwartete Ausgabe decken (%)"))
blockA$block <- "A. Soziodemografie & Gesundheit"
blockB$block <- "B. Objektive Ressourcen"
blockC$block <- "C. Subjektive Einschätzung"
tab1 <- rbind(blockA, blockB, blockC)
tab1[, c("Gesamt","Miete","Eig_ohne","Eig_mit")] <- lapply(tab1[, c("Gesamt","Miete","Eig_ohne","Eig_mit")],
                                                            function(x) round(x, 1))
n_ten <- table(factor(d$tenure3, levels = TENLV))
tab1 <- rbind(data.frame(variable = "N (ungewichtet)", level = "", Gesamt = sum(n_ten),
                         Miete = n_ten[[1]], Eig_ohne = n_ten[[2]], Eig_mit = n_ten[[3]],
                         block = "N"), tab1[, names(tab1)])
tab1 <- tab1[, c("block","variable","level","Gesamt","Miete","Eig_ohne","Eig_mit")]
write.csv(tab1, "output/tab1_descriptives.csv", row.names = FALSE)
cat("Step 2 done: tab1 (", nrow(tab1), "rows )\n")

## =============================================================================
## STEP 3. Nested models M0-M5  -> tab2 (+ CC|MI), tab2b, fig1, tab11
## =============================================================================
objblock <- "+ log_eqinc + wealth_fin_tert + log_imprent_eq + has_mortgage + ltv + pil2_home"
subblock <- "+ endsmeet_f + suddenexp_subj"
f0 <- "~ TEN"
f1 <- "~ TEN + age + female + civstat + alone + edu3 + swiss + haskids"
f2 <- paste(f1, "+ gali_any + dep_score + srh_poor")
f3 <- paste(f2, "+ lingreg + degurba")
f4 <- paste(f3, objblock)
f5 <- paste(f4, subblock)
FS <- list(M0=f0, M1=f1, M2=f2, M3=f3, M4=f4, M5=f5)

## common complete-case sample = complete on every M5 term (so M0..M5 share it)
base_vars <- c("swls","age","female","civstat","alone","edu3","swiss","haskids",
               "gali_any","dep_score","srh_poor","lingreg","degurba",
               "log_eqinc","wealth_fin_tert","log_imprent_eq","has_mortgage","ltv","pil2_home",
               "endsmeet_f","suddenexp_subj","w","strata_22")
cc  <- complete.cases(d[, base_vars])
dT  <- d[cc & !is.na(d$tenure3), ]; desT <- mkdes(dT)
dO  <- d[cc & !is.na(d$owner), ];   desO <- mkdes(dO)
N_common <- nrow(dT)
cat(sprintf("Step 3 common complete-case sample: N = %d (tenure3)\n", N_common))

## --- CC fits ---------------------------------------------------------------
fits_ten <- lapply(FS, function(f) svyglm(as.formula(paste("swls", sub("TEN","tenure3",f))), design = desT))
fits_own <- lapply(FS, function(f) svyglm(as.formula(paste("swls", sub("TEN","owner",f))),   design = desO))
tab2 <- do.call(rbind, Map(function(fit,nm) ci_tab(fit, "tenure3", nm), fits_ten, names(fits_ten)))
r2 <- sapply(fits_ten, function(fit) { y<-fit$y; mu<-fitted(fit); w<-weights(fit$survey.design)
  1 - sum(w*(y-mu)^2)/sum(w*(y-weighted.mean(y,w))^2) })
tab2$R2 <- round(r2[tab2$model], 3); tab2$N <- N_common
tab2[,c("est","se","lcl","ucl","p")] <- round(tab2[,c("est","se","lcl","ucl","p")], 3)

## --- MI (mice m=MI_M): PRIMARY for the wealth block. Impute continuous wealth/
##     income/ltv/imp-rent/pil2 (~40% item non-response); re-derive logs+tertiles
##     per implicate; pool tenure3/owner coefficients (Rubin). Reused by tab5. ---
suppressWarnings(suppressMessages(library(mice)))
mi_src <- c("swls","tenure3","owner","age","female_f","civstat","alone","edu3","swiss","haskids",
            "gali_any","dep_score","srh_poor","lingreg","degurba",
            "eqinc","wealth_fin","ltv","has_mortgage","imp_rent_eq","imp_rent_net_eq","pil2_home",
            "endsmeet_f","suddenexp_subj","strain","w","strata_22")
imp_df <- d[!is.na(d$w) & !is.na(d$tenure3) & !is.na(d$owner), mi_src]
for (b in c("female_f")) imp_df[[b]] <- factor(imp_df[[b]])
meth <- make.method(imp_df)
meth[c("swls","tenure3","owner","strain","w","strata_22")] <- ""     # never impute outcome/design/focal
pm <- make.predictorMatrix(imp_df); pm[, c("w","strata_22")] <- 0; pm[c("w","strata_22"), ] <- 0
cat(sprintf("Running mice m=%d ...\n", MI_M))
imp <- mice(imp_df, m = MI_M, method = meth, predictorMatrix = pm, printFlag = FALSE, seed = 20221029)
## derive model vars inside each completed implicate (tertiles w/o missing level)
complete_derived <- function(i) {
  ci <- complete(imp, i)
  ci$female         <- as.integer(ci$female_f == "woman")
  ci$log_eqinc      <- log(pmax(ci$eqinc, 1))
  ci$log_imprent_eq <- ir_center(ci$imp_rent_eq, ci$owner, ci$w)   # centre within owners (D-IMPRENT-CENTER)
  ci$owner_re       <- as.integer(ci$tenure3 == "owner + other RE")
  q <- quantile(ci$wealth_fin, c(0,1/3,2/3,1), na.rm = TRUE)
  ci$wealth_fin_tert <- factor(cut(ci$wealth_fin, q, labels = c("low","mid","high"), include.lowest = TRUE),
                               levels = c("mid","low","high"))      # no 'missing' level under MI
  ci
}
imp_list <- lapply(seq_len(MI_M), complete_derived)
pool_coef <- function(term, builder) {                   # Rubin pooling of one coefficient across implicates
  est <- se <- numeric(MI_M)
  for (i in seq_len(MI_M)) { fit <- svyglm(builder, design = mkdes(imp_list[[i]]))
    s <- summary(fit)$coefficients; est[i] <- s[term,1]; se[i] <- s[term,2] }
  Tvar <- mean(se^2) + (1 + 1/MI_M)*var(est)
  c(est = mean(est), se = sqrt(Tvar)) }
build <- function(f, who) as.formula(paste("swls", sub("TEN", who, f)))
## MI tenure3 coefficients for M4 & M5 (the CC|MI pair the brief asks for)
mi_tab <- do.call(rbind, lapply(c("M4","M5"), function(m)
  do.call(rbind, lapply(c("tenure3owner, no other RE","tenure3owner + other RE"), function(tm) {
    v <- pool_coef(tm, build(FS[[m]], "tenure3"))
    data.frame(model = m, term = tm, est_MI = round(v["est"],3), se_MI = round(v["se"],3)) }))))
tab2 <- merge(tab2, mi_tab, by = c("model","term"), all.x = TRUE)
tab2 <- tab2[order(match(tab2$model, names(FS)), tab2$term), ]
write.csv(tab2, "output/tab2_nested-models.csv", row.names = FALSE)

## full M5 coefficient table (all terms)
tab2b <- ci_tab(fits_ten$M5, NULL, "M5 full"); tab2b <- tab2b[tab2b$term != "(Intercept)", ]
tab2b[,c("est","se","lcl","ucl","p")] <- round(tab2b[,c("est","se","lcl","ucl","p")], 3)
write.csv(tab2b, "output/tab2b_full-controls.csv", row.names = FALSE)

## --- fig1: binary-owner coefficient across M0-M5, objektiv|subjektiv divider --
own <- do.call(rbind, Map(function(fit,nm) ci_tab(fit, "^owner$", nm), fits_own, names(fits_own)))
own$model <- factor(own$model, levels = names(FS))
own$xpos  <- as.numeric(own$model)                 # numeric x so the divider + annotation share a continuous scale
p1 <- ggplot(own, aes(xpos, est)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_vline(xintercept = 5.5, linetype = "dashed", colour = ORANGE) +
  geom_col(width = .6, fill = BLUE) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = .2) +
  geom_text(aes(label = sprintf("%+.2f", est)), vjust = -0.6, size = 3.2) +
  annotate("text", x = 5.5, y = max(own$ucl), label = "objektiv | subjektiv",
           hjust = 0.5, vjust = -0.2, size = 3.2, colour = ORANGE) +
  scale_x_continuous(breaks = 1:length(names(FS)), labels = names(FS)) +
  labs(x = NULL, y = "Eigentümer-Koeffizient auf SWLS (95%-KI)",
       title = "Der Eigentumsvorteil: objektive Ressourcen verengen ihn, die subjektive Lage schliesst ihn",
       subtitle = sprintf("SWLS (5-35), 65+, gewichtet. M4 = + objektive Ressourcen, M5 = + subjektive Einschätzung (n = %d).", N_common)) +
  theme_minimal(base_size = 11) + theme(panel.grid.major.x = element_blank())
ggsave("output/fig1_owner-coef.png", p1, width = 8, height = 5, dpi = 300)
saveRDS(p1, "output/fig1_owner-coef.rds")

## --- tab11: decomposition on the NEW order (owner coef, binary) --------------
steps <- list(
  `M3 (ohne Finanzen)` = f3,
  `+ Einkommen`        = paste(f3, "+ log_eqinc"),
  `+ Finanzvermögen`   = paste(f3, "+ log_eqinc + wealth_fin_tert"),
  `+ imput. Miete`     = paste(f3, "+ log_eqinc + wealth_fin_tert + log_imprent_eq"),
  `+ Hypothek/LTV (=M4)` = f4,
  `+ auskommen`        = paste(f4, "+ endsmeet_f"),
  `+ Liquidität (=M5)` = f5)
dec <- do.call(rbind, Map(function(f,nm){
  fit <- svyglm(build(f,"owner"), design = desO)
  c0 <- ci_tab(fit, "^owner$", nm); data.frame(schritt = nm, owner_b = round(c0$est,3), se = round(c0$se,3)) },
  steps, names(steps)))
dec$drop <- c(NA, round(-diff(dec$owner_b), 3))
write.csv(dec, "output/tab11_decomposition.csv", row.names = FALSE)
cat("Step 3 done: tab2 (CC|MI), tab2b, fig1, tab11\n")

## =============================================================================
## STEP 4. Interaction — does the house buffer?  -> tab3 + fig2
## strain = poor_silver (OBJECTIVE income poverty)
## =============================================================================
dC <- dO[!is.na(dO$strain), ]; desC <- mkdes(dC)
fC4 <- svyglm(as.formula(paste("swls ~ owner * strain", sub("~ TEN","",f4))), design = desC)
fC5 <- svyglm(as.formula(paste("swls ~ owner * strain", sub("~ TEN","",f5))), design = desC)
tab3 <- rbind(cbind(block = "M4 + owner×strain", ci_tab(fC4, "owner|strain", "M4")),
              cbind(block = "M5 + owner×strain", ci_tab(fC5, "owner|strain", "M5")))
tab3[,c("est","se","lcl","ucl","p")] <- round(tab3[,c("est","se","lcl","ucl","p")], 3)
## MI pooled interaction term (for tab2/tab5 CC|MI pair)
mi_int <- pool_coef("owner:strain", as.formula(paste("swls ~ owner * strain", sub("~ TEN","",f4))))
write.csv(tab3, "output/tab3_interaction.csv", row.names = FALSE)

## fig2: observed weighted mean SWLS in the 2×2 owner×strain cells (with 95% CI)
cell <- as.data.frame(svyby(~swls, ~owner + strain, subset(des_all, !is.na(owner) & !is.na(strain)),
                            svymean, na.rm = TRUE, vartype = "ci"))
names(cell) <- c("owner","strain","swls","lcl","ucl")
cell$owner  <- factor(cell$owner, 0:1, c("Mieter","Eigentümer"))
cell$strain <- factor(cell$strain, 0:1, c("nicht einkommensarm","einkommensarm"))
p2 <- ggplot(cell, aes(strain, swls, colour = owner, group = owner)) +
  geom_line(position = position_dodge(.2)) +
  geom_pointrange(aes(ymin = lcl, ymax = ucl), position = position_dodge(.2), size = .6) +
  scale_colour_manual(values = c(Mieter = GREY, `Eigentümer` = BLUE), name = NULL) +
  labs(x = NULL, y = "SWLS (gewichtet, 95%-KI)",
       title = "Das Eigenheim puffert die objektive Einkommensarmut nicht",
       subtitle = sprintf("Mittlere Lebenszufriedenheit nach Wohnform × Einkommensarmut, 65+ (Interaktion owner×strain p = %.2f, M4).",
                          tab3$p[tab3$block=="M4 + owner×strain" & grepl("owner:strain", tab3$term)][1])) +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
ggsave("output/fig2_owner-x-strain.png", p2, width = 7.5, height = 5, dpi = 300)
saveRDS(p2, "output/fig2_owner-x-strain.rds")
cat("Step 4 done: tab3, fig2\n")

## =============================================================================
## STEP 5. Inside the owner group — burden or advantage?  -> tab4 + fig3
## owners only, weighted OLS SWLS, M4 control set minus the owner/focal terms
## =============================================================================
dOwn <- d[which(d$owner == 1 & !is.na(d$w)), ]
d$log_imprentnet_eq <- log1p(pmax(d$imp_rent_net_eq, 0))
dOwn$log_imprentnet_eq <- log1p(pmax(dOwn$imp_rent_net_eq, 0))
ctrl <- "age + female + civstat + alone + edu3 + swiss + haskids + gali_any + dep_score + srh_poor + lingreg + degurba + log_eqinc + wealth_fin_tert"
focal <- list(
  `(a) Hypothek (ja/nein)`      = "has_mortgage",
  `(b) Belehnungsgrad (LTV-Kat)`= "ltv_cat",
  `(c) Hypothekenlast (% Eink.)`= "mortgage_burden",
  `(c2) Last > 15% Eink.`       = "mort_burden_15",
  `(c3) Last > 30% Eink.`       = "mort_burden_30",
  `(d) imput. Netto-Miete (log)`= "log_imprentnet_eq",
  `(e) PK-Bezug fürs Eigenheim` = "pil2_home",
  `(f) weitere Immobilie`       = "owner_re")
CVARS <- c("age","female","civstat","alone","edu3","swiss","haskids","gali_any","dep_score",
           "srh_poor","lingreg","degurba","log_eqinc","wealth_fin_tert")
one_model <- function(lab, term) {
  dsub <- dOwn[complete.cases(dOwn[, c("swls", CVARS, term, "w", "strata_22")]), ]
  fit <- svyglm(as.formula(paste("swls ~", term, "+", ctrl)), design = mkdes(dsub))
  r <- ci_tab(fit, paste0("^", term), lab); r$block <- lab; r$n <- nrow(dsub); r }
tab4_rows <- do.call(rbind, Map(one_model, names(focal), unlist(focal)))
## joint model: all focal predictors together (owners with all of them observed)
jf <- "swls ~ has_mortgage + ltv_cat + mortgage_burden + log_imprentnet_eq + pil2_home + owner_re +"
dJ <- dOwn[complete.cases(dOwn[, c("ltv_cat","mortgage_burden","log_imprentnet_eq","pil2_home","owner_re","has_mortgage")]), ]
fJ <- svyglm(as.formula(paste(jf, ctrl)), design = mkdes(dJ))
joint <- ci_tab(fJ, "has_mortgage|ltv_cat|mortgage_burden|log_imprentnet_eq|pil2_home|owner_re", "joint")
joint$block <- "(g) gemeinsames Modell"; joint$n <- nrow(dJ)
tab4 <- rbind(tab4_rows, joint)
tab4[,c("est","se","lcl","ucl","p")] <- round(tab4[,c("est","se","lcl","ucl","p")], 3)
tab4 <- tab4[, c("block","term","est","se","lcl","ucl","p","n")]
write.csv(tab4, "output/tab4_owner-burden.csv", row.names = FALSE)

## fig3: weighted mean SWLS by LTV category and by mortgage-burden band (owners)
dOwn$burden_band <- factor(dplyr::case_when(
  is.na(dOwn$mortgage_burden) | dOwn$has_mortgage == 0 ~ "keine Hypothek",
  dOwn$mortgage_burden <= 0.15 ~ "≤15%",
  dOwn$mortgage_burden <= 0.30 ~ "(15,30]%",
  TRUE ~ ">30%"),
  levels = c("keine Hypothek","≤15%","(15,30]%",">30%"))
desOwn <- mkdes(dOwn)
g_ltv <- as.data.frame(svyby(~swls, ~ltv_cat, subset(desOwn,!is.na(ltv_cat)), svymean, na.rm=TRUE, vartype="ci"))
g_bur <- as.data.frame(svyby(~swls, ~burden_band, subset(desOwn,!is.na(burden_band)), svymean, na.rm=TRUE, vartype="ci"))
names(g_ltv) <- names(g_bur) <- c("x","swls","lcl","ucl")
g_ltv$x <- as.character(g_ltv$x); g_bur$x <- as.character(g_bur$x)
g_ltv$panel <- "Belehnungsgrad (LTV)"; g_bur$panel <- "Hypothekenlast (% Einkommen)"
LTV_ORD <- c("0","(0,.33]","(.33,.66]",">.66"); BUR_ORD <- c("keine Hypothek","≤15%","(15,30]%",">30%")
gf3 <- rbind(g_ltv, g_bur)
gf3$x <- factor(gf3$x, levels = c(LTV_ORD, BUR_ORD))
p3 <- ggplot(gf3, aes(x, swls)) +
  geom_pointrange(aes(ymin = lcl, ymax = ucl), colour = BLUE) +
  facet_wrap(~panel, scales = "free_x") +
  labs(x = NULL, y = "SWLS (gewichtet, 95%-KI)",
       title = "Innerhalb der Eigentümer: höhere Hypothekenlast, etwas tiefere Lebenszufriedenheit",
       subtitle = "Mittlere SWLS nach Belehnungsgrad und Hypothekenlast, Eigentümer 65+ (gewichtet)") +
  theme_minimal(base_size = 11) + theme(axis.text.x = element_text(angle = 15, hjust = 1))
ggsave("output/fig3_owner-burden.png", p3, width = 9, height = 4.5, dpi = 300)
saveRDS(p3, "output/fig3_owner-burden.rds")
cat("Step 5 done: tab4, fig3\n")

## =============================================================================
## STEP 6. Robustness  -> tab5   (MI = PRIMARY for the wealth block)
## =============================================================================
rob <- list()
add <- function(spec, term, est, se) data.frame(spec=spec, term=term, est=round(est,3), se=round(se,3))
## PRIMARY: MI pooled owner coefficients (M4, M5) + interaction
mi_m4 <- pool_coef("owner", build(f4,"owner")); mi_m5 <- pool_coef("owner", build(f5,"owner"))
rob$MI_M4 <- add("MI m=20, M4 owner (PRIMARY wealth block)", "owner", mi_m4["est"], mi_m4["se"])
rob$MI_M5 <- add("MI m=20, M5 owner (PRIMARY wealth block)", "owner", mi_m5["est"], mi_m5["se"])
rob$MI_CX <- add("MI m=20, owner×strain (M4)", "owner:strain", mi_int["est"], mi_int["se"])
## (i) ordered logit on SWLS category (M4, owner)
dRo <- dO[!is.na(dO$swls_cat), ]; dRo$swls_cat <- ordered(dRo$swls_cat)
ol <- tryCatch(svyolr(as.formula(paste("swls_cat", sub("TEN","owner",f4))), design = mkdes(dRo)),
               error = function(e) NULL)
if (!is.null(ol)) { s <- summary(ol)$coefficients; rob$olr <- add("(i) ordered logit SWLS-cat (M4)","owner",s["owner",1],s["owner",2]) }
## (iii) unweighted M4
lmU <- lm(build(f4,"owner"), data = dO); cu <- summary(lmU)$coefficients["owner", ]
rob$unw <- add("(iii) unweighted M4","owner",cu[1],cu[2])
## (iv) M4 by age group
for (g in c("65-79","80+")) { dg <- dO[dO$age_grp==g, ]
  fg <- svyglm(build(f4,"owner"), design = mkdes(dg)); cg <- ci_tab(fg,"^owner$",g)
  rob[[paste0("age_",g)]] <- add(paste0("(iv) M4, ",g),"owner",cg$est,cg$se) }
## (vi) M4 with BFS-9 typology instead of DEGURBA
dB <- dO[!is.na(dO$bfs9), ]; fB <- svyglm(as.formula(paste("swls", sub("degurba","bfs9", sub("TEN","owner",f4)))), design=mkdes(dB))
cb <- ci_tab(fB,"^owner$",""); rob$bfs9 <- add("(vi) M4 BFS-9 typology","owner",cb$est,cb$se)
## (vii) M4 with collapsed BFS-9 + Bergregion
dK <- dO[!is.na(dO$bfs_coll), ]; fK <- svyglm(as.formula(paste("swls", sub("degurba","bfs_coll + mountain", sub("TEN","owner",f4)))), design=mkdes(dK))
ck <- ci_tab(fK,"^owner$",""); rob$bfs_coll <- add("(vii) M4 BFS-9(coll)+Bergregion","owner",ck$est,ck$se)
## --- sensitivity to housing constants (recompute derived vars, refit M4 owner) ---
refit_owner_M4 <- function(dd) { dd <- dd[complete.cases(dd[, base_vars]) & !is.na(dd$owner), ]
  ci_tab(svyglm(build(f4,"owner"), design = mkdes(dd)), "^owner$","") }
## NET_YIELD 0.02 / 0.04 : changes imputed rent (centred within owners) in M4
for (ny in c(0.02, 0.04)) { dd <- d
  ir <- ifelse(dd$owner==0, 0, ny*dd$home_value/12 / dd$equiv_factor)
  dd$log_imprent_eq <- ir_center(ir, dd$owner, dd$w)
  c0 <- refit_owner_M4(dd); rob[[paste0("ny",ny)]] <- add(sprintf("sens: NET_YIELD %.2f (M4 owner)",ny),"owner",c0$est,c0$se) }
## full-RE home value (D-HOMEVALUE-RE off): owner+other RE uses the FULL RE value
dd <- d
dd$ltv <- ifelse(dd$owner==1, pmin(dd$mortgage_amt/dd$home_value_full, 1.5), ifelse(dd$owner==0,0,NA))
ir <- ifelse(dd$owner==0, 0, 0.03*dd$home_value_full/12 / dd$equiv_factor)
dd$log_imprent_eq <- ir_center(ir, dd$owner, dd$w)
c0 <- refit_owner_M4(dd); rob$fullRE <- add("sens: full RE value for owner+RE (M4 owner)","owner",c0$est,c0$se)
## MORTGAGE_RATE 0.025 : affects mortgage_burden (reported for tab4 (c))
dd <- dOwn; dd$mb2 <- dd$mortgage_amt*0.025/12 / dd$hh_income
fmb <- svyglm(as.formula(paste("swls ~ mb2 +", ctrl)), design = mkdes(dd[!is.na(dd$mb2), ]))
cmb <- ci_tab(fmb,"^mb2$",""); rob$rate025 <- add("sens: MORTGAGE_RATE 0.025 (tab4 burden coef)","mortgage_burden",cmb$est,cmb$se)
tab5 <- do.call(rbind, rob); rownames(tab5) <- NULL
write.csv(tab5, "output/tab5_robustness.csv", row.names = FALSE)
cat("Step 6 done: tab5\n")

cat(sprintf("\nALL DONE (paper 1). Common N = %d. Outputs in output/.\n", N_common))
