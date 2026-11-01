## =============================================================================
## 02  Analysis — Wohnen und Wohnbefinden (SAS 2022), 65+
## -----------------------------------------------------------------------------
## Run 01_setup-data.R first (writes data/wohnen_working.rds). Weighted
## throughout (design weight `w`, strata `strata_22`, survey package). Outcome =
## SWLS (Diener, 5-35). Blocks A-E per HANDOFF/HANDOFF_analysis-run_2026-10-30.md.
## Writes CSV tables and PNG figures (+ ggplot .rds) to output/. German labels.
## =============================================================================

suppressWarnings(suppressMessages({
  library(survey); library(dplyr); library(ggplot2); library(tidyr)
}))
options(survey.lonely.psu = "adjust"); set.seed(20221029)
dir.create("output", showWarnings = FALSE)
BLUE <- "#2b6cb0"; GREY <- "#9fb6cf"
d <- readRDS("data/wohnen_working.rds")

## derived model inputs
d$log_eqinc  <- log(pmax(d$eqinc, 1))
wq <- quantile(d$wealth_net, c(0, 1/3, 2/3, 1), na.rm = TRUE)
d$wealth_tert <- cut(d$wealth_net, wq, labels = c("low","mid","high"), include.lowest = TRUE)
d$wealth_tert <- factor(ifelse(is.na(d$wealth_tert), "missing", as.character(d$wealth_tert)),
                        levels = c("mid","low","high","missing"))
d$female_f <- factor(d$female, 0:1, c("man","woman"))
## asset-rich, income-poor, outside EL flag (define before any design is built)
d$owner_poor_noel <- as.integer(d$owner == 1 & d$strain == 1 & (is.na(d$el_receipt) | d$el_receipt == 0))
mkdes <- function(df) svydesign(ids = ~1, strata = ~strata_22, weights = ~w, data = df, nest = TRUE)

ci_tab <- function(fit, keep = NULL, label = "") {
  s <- summary(fit)$coefficients
  o <- data.frame(term = rownames(s), est = s[,1], se = s[,2],
                  lcl = s[,1] - 1.96*s[,2], ucl = s[,1] + 1.96*s[,2],
                  p = s[,4], model = label, row.names = NULL)
  if (!is.null(keep)) o <- o[grepl(keep, o$term), ]
  o
}

## =============================================================================
## A. Descriptives — tenure profile  -> tab1 + fig0
## =============================================================================
des_all <- mkdes(d[!is.na(d$w), ])
cat_vars <- c("age_grp","female_f","alone","edu3","swiss","civstat","lingreg","degurba",
              "eqinc_tert","endsmeet_f","liquidity","strain","el_receipt","pil2_home","pil2_any")
num_vars <- c("age","swls","eqinc","wealth_net")

wshare <- function(des, v) {                       # weighted % by tenure3 (+ total), categorical
  f <- as.formula(paste0("~factor(", v, ")"))
  by <- svyby(f, ~tenure3, des, svymean, na.rm = TRUE)
  tot <- svymean(f, des, na.rm = TRUE)
  lv <- sub(paste0("factor\\(", v, "\\)"), "", names(tot))
  grp <- as.data.frame(by); rownames(grp) <- grp$tenure3
  out <- lapply(seq_along(lv), function(i) {
    col <- paste0("factor(", v, ")", lv[i])
    data.frame(variable = v, level = lv[i],
               tenant = 100*grp["tenant", col],
               owner_noRE = 100*grp["owner, no other RE", col],
               owner_plusRE = 100*grp["owner + other RE", col],
               total = 100*unname(tot[col]))
  })
  do.call(rbind, out)
}
wmean_by <- function(des, v) {                     # weighted mean by tenure3 (+ total), continuous
  by <- as.data.frame(svyby(as.formula(paste0("~", v)), ~tenure3, des, svymean, na.rm = TRUE))
  rownames(by) <- by$tenure3; tot <- svymean(as.formula(paste0("~", v)), des, na.rm = TRUE)
  data.frame(variable = v, level = "mean",
             tenant = by["tenant", v], owner_noRE = by["owner, no other RE", v],
             owner_plusRE = by["owner + other RE", v], total = unname(tot[1]))
}
tab1 <- rbind(do.call(rbind, lapply(cat_vars, function(v) wshare(des_all, v))),
              do.call(rbind, lapply(num_vars, function(v) wmean_by(des_all, v))))
tab1[,3:6] <- round(tab1[,3:6], 1)
n_tenure <- table(d$tenure3)
tab1 <- rbind(data.frame(variable="N (unweighted)", level="",
                         tenant=n_tenure["tenant"], owner_noRE=n_tenure["owner, no other RE"],
                         owner_plusRE=n_tenure["owner + other RE"], total=sum(n_tenure)), tab1)
write.csv(tab1, "output/tab1_tenure-profile.csv", row.names = FALSE)

## fig0: tenure composition by linguistic region x urbanisation (weighted %)
g0 <- as.data.frame(svyby(~tenure3, ~lingreg + degurba, subset(des_all, !is.na(tenure3)),
                          svymean, na.rm = TRUE))
g0l <- g0 %>% select(lingreg, degurba, starts_with("tenure3")) %>%
  pivot_longer(starts_with("tenure3"), names_to = "tenure", values_to = "share") %>%
  mutate(tenure = factor(sub("tenure3","",tenure),
                         levels = c("tenant","owner, no other RE","owner + other RE"),
                         labels = c("Mieter","Eigentümer, kein weiteres","Eigentümer + weiteres")))
p0 <- ggplot(g0l, aes(degurba, 100*share, fill = tenure)) +
  geom_col(width = .7) + facet_wrap(~lingreg) +
  scale_fill_manual(values = c("#9fb6cf","#4a7fb5",BLUE), name = NULL) +
  labs(x = NULL, y = "Gewichteter Anteil (%)",
       title = "Wohnform nach Sprachregion und Urbanisierungsgrad (65+)") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom",
       axis.text.x = element_text(angle = 25, hjust = 1))
ggsave("output/fig0_tenure-by-region.png", p0, width = 9, height = 4.5, dpi = 300)
saveRDS(p0, "output/fig0_tenure-by-region.rds")
cat("A done: tab1, fig0\n")

## =============================================================================
## B. Nested OLS on SWLS  -> tab2 + fig1  (common complete-case sample)
## =============================================================================
base_vars <- c("swls","age","female","civstat","alone","edu3","swiss","haskids",
               "gali_any","dep_score","srh_poor","lingreg","degurba","log_eqinc","endsmeet_f",
               "liquidity","w","strata_22")
cc  <- complete.cases(d[, base_vars])
dT  <- d[cc & !is.na(d$tenure3), ];  desT <- mkdes(dT)
dO  <- d[cc & !is.na(d$owner), ];    desO <- mkdes(dO)
cat("B common-sample n:", nrow(dT), "(tenure3)\n")

f0 <- "~ TEN"
f1 <- "~ TEN + age + female + civstat + alone + edu3 + swiss + haskids"
f2 <- paste(f1, "+ gali_any + dep_score + srh_poor")
f3 <- paste(f2, "+ lingreg + degurba")
f4 <- paste(f3, "+ log_eqinc + wealth_tert + endsmeet_f + liquidity")
fits_ten <- lapply(list(M0=f0,M1=f1,M2=f2,M3=f3,M4=f4), function(f)
  svyglm(as.formula(paste("swls", sub("TEN","tenure3",f))), design = desT))
tab2 <- do.call(rbind, Map(function(fit,nm) ci_tab(fit, "tenure3", nm),
                           fits_ten, names(fits_ten)))
tab2[,2:6] <- round(tab2[,2:6], 3)
write.csv(tab2, "output/tab2_nested-models.csv", row.names = FALSE)

## full M4 coefficient table (ALL controls: SRH, civil status, living alone, GALI,
## depression, education, nationality, children, region, income, wealth, ends-meet…)
tab2b <- ci_tab(fits_ten$M4, NULL, "M4 full"); tab2b <- tab2b[tab2b$term != "(Intercept)", ]
tab2b[,2:6] <- round(tab2b[,2:6], 3)
write.csv(tab2b, "output/tab2b_full-controls.csv", row.names = FALSE)

## Fig 1: owner (binary) coefficient across M0-M4 — the shrinking bar
fits_own <- lapply(list(M0=f0,M1=f1,M2=f2,M3=f3,M4=f4), function(f)
  svyglm(as.formula(paste("swls", sub("TEN","owner",f))), design = desO))
own <- do.call(rbind, Map(function(fit,nm) ci_tab(fit, "^owner$", nm), fits_own, names(fits_own)))
own$model <- factor(own$model, levels = c("M0","M1","M2","M3","M4"))
p1 <- ggplot(own, aes(model, est)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_col(width = .6, fill = BLUE) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = .2) +
  geom_text(aes(label = sprintf("%+.2f", est)), vjust = -0.6, size = 3.4) +
  labs(x = NULL, y = "Eigentümer-Koeffizient auf SWLS (95%-KI)",
       title = "Der Eigentumsvorteil schrumpft, sobald die finanzielle Lage kontrolliert wird",
       subtitle = sprintf("Lebenszufriedenheit (SWLS 5-35), 65+, gewichtet. M0 roh — M4 voll (n = %d).", nrow(dO))) +
  theme_minimal(base_size = 11) + theme(panel.grid.major.x = element_blank())
ggsave("output/fig1_owner-coef.png", p1, width = 7.5, height = 5, dpi = 300)
saveRDS(p1, "output/fig1_owner-coef.rds")
cat("B done: tab2, fig1\n")

## =============================================================================
## C. Interaction owner x financial strain  -> tab3 + fig2
## =============================================================================
dC  <- dO[!is.na(dO$strain), ]; desC <- mkdes(dC)
fC  <- svyglm(as.formula(paste("swls ~ owner * strain", sub("~ TEN","",f4))), design = desC)
tab3 <- ci_tab(fC, "owner|strain", "M4 + owner*strain")
tab3[,2:6] <- round(tab3[,2:6], 3)
write.csv(tab3, "output/tab3_interaction.csv", row.names = FALSE)

## Fig 2: weighted mean SWLS by owner x making-ends-meet cell (descriptive, with CI)
cell <- as.data.frame(svyby(~swls, ~owner + endsmeet_f, subset(des_all, !is.na(owner) & !is.na(endsmeet_f)),
                            svymean, na.rm = TRUE, vartype = "ci"))
names(cell) <- c("owner","endsmeet","swls","lcl","ucl")
cell$owner <- factor(cell$owner, 0:1, c("Mieter","Eigentümer"))
p2 <- ggplot(cell, aes(endsmeet, swls, colour = owner, group = owner)) +
  geom_line(position = position_dodge(.25)) +
  geom_pointrange(aes(ymin = lcl, ymax = ucl), position = position_dodge(.25)) +
  scale_colour_manual(values = c(Mieter = GREY, `Eigentümer` = BLUE), name = NULL) +
  labs(x = "Mit dem Einkommen auskommen", y = "SWLS (gewichtet, 95%-KI)",
       title = "Das Eigenheim puffert nicht",
       subtitle = "Mittlere Lebenszufriedenheit nach Wohnform und finanzieller Lage, 65+ (angespannte Eigentümer ≈ angespannte Mieter)") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom",
       axis.text.x = element_text(angle = 15, hjust = 1))
ggsave("output/fig2_owner-x-strain.png", p2, width = 8, height = 5, dpi = 300)
saveRDS(p2, "output/fig2_owner-x-strain.rds")
cat("C done: tab3, fig2 (interaction term p =", round(tab3$p[grepl("owner:strain|owner:", tab3$term)][1],3), ")\n")

## =============================================================================
## D. Inside the owner group  -> tab4
## =============================================================================
dOwn <- d[which(d$owner == 1 & !is.na(d$w)), ]; desOwn <- mkdes(dOwn)
## (i) logit: who is the income-strained owner?  report AMEs
fDi <- svyglm(strain ~ age_grp + female + alone + edu3 + mortgage + pil2_home + lingreg + degurba,
              design = desOwn, family = quasibinomial())
ame_logit <- function(fit, des, h = 1e-4) {
  df <- des$variables; w <- weights(des); vars <- all.vars(formula(fit))[-1]
  base <- predict(fit, newdata = df, type = "response")
  out <- lapply(vars, function(v) {
    x <- df[[v]]
    if (is.factor(x)) {
      lv <- levels(x); ref <- lv[1]
      do.call(rbind, lapply(lv[-1], function(l) {
        d1 <- df; d1[[v]] <- factor(l, levels = lv); d0 <- df; d0[[v]] <- factor(ref, levels = lv)
        a <- sum(w*(predict(fit,newdata=d1,type="response")-predict(fit,newdata=d0,type="response")),na.rm=TRUE)/sum(w)
        data.frame(term = paste0(v,"=",l), ame_pp = 100*a) }))
    } else if (all(na.omit(x) %in% 0:1)) {
      d1<-df; d1[[v]]<-1; d0<-df; d0[[v]]<-0
      a <- sum(w*(predict(fit,newdata=d1,type="response")-predict(fit,newdata=d0,type="response")),na.rm=TRUE)/sum(w)
      data.frame(term=v, ame_pp=100*a)
    } else {
      d1<-df; d1[[v]]<-x+h
      a <- sum(w*(predict(fit,newdata=d1,type="response")-base),na.rm=TRUE)/sum(w)/h
      data.frame(term=v, ame_pp=100*a)
    }
  })
  do.call(rbind, out)
}
Di_ame <- ame_logit(fDi, desOwn); Di_ame$ame_pp <- round(Di_ame$ame_pp, 2)
## (ii) OLS swls ~ mortgage + pil2_home + controls (owners)
fDii <- svyglm(swls ~ mortgage + pil2_home + age_grp + female + alone + edu3 + log_eqinc + lingreg + degurba,
               design = desOwn)
Dii <- ci_tab(fDii, "mortgage|pil2_home", "owners: SWLS"); Dii[,2:6] <- round(Dii[,2:6],3)
## (iii) asset-rich, income-poor, outside EL
poor_own  <- as.numeric(coef(svymean(~strain, desOwn, na.rm = TRUE))[1])
desPoorOwn <- subset(desOwn, strain == 1)
not_el    <- 1 - as.numeric(coef(svymean(~el_receipt, desPoorOwn, na.rm = TRUE))[1])  # share NOT receiving EL
share_ARIP<- as.numeric(coef(svymean(~owner_poor_noel, des_all, na.rm = TRUE))[1])
wq1 <- function(desub, v) as.numeric(svyquantile(as.formula(paste0("~", v)), desub, 0.5,
                                                 ci = FALSE, na.rm = TRUE)[[1]][1])
med_net_poor <- wq1(desPoorOwn, "wealth_net"); med_fin_poor <- wq1(desPoorOwn, "wealth_fin")
summ4 <- data.frame(
  metric = c("owners income-poor (silver) %","—¦of them NOT receiving EL %",
             "ALL 65+: owner & income-poor & no EL %",
             "median net wealth, income-poor owners (CHF)",
             "median financial wealth, income-poor owners (CHF)"),
  value  = round(c(100*poor_own, 100*not_el, 100*share_ARIP,
                   med_net_poor, med_fin_poor), 1))
## write the sub-tables separately (different schemas)
write.csv(Di_ame, "output/tab4a_strained-owner-AME.csv", row.names = FALSE)
write.csv(Dii,    "output/tab4b_owner-SWLS.csv",         row.names = FALSE)
write.csv(summ4,  "output/tab4c_asset-rich-income-poor.csv", row.names = FALSE)
tab4 <- list(who_is_strained_owner_AME_pp = Di_ame, owners_SWLS_mortgage_2pillar = Dii,
             summary = summ4)
saveRDS(tab4, "output/tab4_owners_list.rds")
cat("D done: tab4a/b/c\n")

## =============================================================================
## E. Robustness  -> tab5
## =============================================================================
rob <- list()
## (i) ordered logit on SWLS category
dR <- dO[!is.na(dO$swls_cat), ]; dR$swls_cat <- ordered(dR$swls_cat)
ol <- tryCatch(svyolr(swls_cat ~ owner + age + female + civstat + alone + edu3 + swiss + haskids +
                      gali_any + dep_score + lingreg + degurba + log_eqinc + wealth_tert + endsmeet_f + liquidity,
                      design = mkdes(dR)), error = function(e) NULL)
if (!is.null(ol)) { s <- summary(ol)$coefficients
  rob$ordered_logit_owner <- data.frame(spec="(i) ordered logit SWLS-cat", term="owner",
    est=round(s["owner",1],3), se=round(s["owner",2],3)) }
## (iii) unweighted M4
lmU <- lm(as.formula(paste("swls", sub("TEN","owner",f4))), data = dO)
cu <- summary(lmU)$coefficients["owner", ]
rob$unweighted_M4_owner <- data.frame(spec="(iii) unweighted M4", term="owner",
  est=round(cu[1],3), se=round(cu[2],3))
## (iv) M4 by age group
for (g in c("65-79","80+")) { dg <- dO[dO$age_grp==g, ]
  fg <- svyglm(as.formula(paste("swls", sub("TEN","owner",f4))), design = mkdes(dg))
  cg <- ci_tab(fg,"^owner$",paste0("(iv) M4, ",g))
  rob[[paste0("age_",g)]] <- data.frame(spec=cg$model, term="owner", est=round(cg$est,3), se=round(cg$se,3)) }
## (v) interaction with subjective strain
fCs <- svyglm(as.formula(paste("swls ~ owner * strain_subj", sub("~ TEN","",f4))), design = desO)
cs <- ci_tab(fCs, "owner:strain_subj", "(v) owner*strain_subj")
rob$interaction_subjective <- data.frame(spec=cs$model, term=cs$term, est=round(cs$est,3), se=round(cs$se,3))
## (vi) M4 with BFS-9 Gemeindetypologie instead of DEGURBA
dB <- dO[!is.na(dO$bfs9), ]
fB <- svyglm(as.formula(paste("swls", sub("degurba","bfs9", sub("TEN","owner",f4)))), design = mkdes(dB))
cb <- ci_tab(fB, "^owner$", "(vi) M4 with BFS-9 typology")
rob$M4_bfs9 <- data.frame(spec=cb$model, term="owner", est=round(cb$est,3), se=round(cb$se,3))
## (ii) MI (mice m=20) for M4 owner coef + interaction C  — robustness
RUN_MI <- TRUE
if (RUN_MI) {
  suppressWarnings(suppressMessages(library(mice)))
  mi_vars <- c("swls","owner","strain","age","female","civstat","alone","edu3","swiss","haskids",
               "gali_any","dep_score","srh_poor","lingreg","degurba","log_eqinc","wealth_tert","endsmeet_f",
               "liquidity","w","strata_22")
  imp_df <- d[!is.na(d$w), mi_vars]
  meth <- make.method(imp_df); meth[c("w","strata_22","swls","owner")] <- ""
  pm <- make.predictorMatrix(imp_df); pm[, c("w","strata_22")] <- 0
  imp <- mice(imp_df, m = 20, method = meth, predictorMatrix = pm, printFlag = FALSE, seed = 20221029)
  pool_coef <- function(term, builder) {
    est <- se <- numeric(20)
    for (i in 1:20) { ci <- complete(imp, i)
      fit <- svyglm(builder, design = mkdes(ci)); s <- summary(fit)$coefficients
      est[i] <- s[term,1]; se[i] <- s[term,2] }
    Tvar <- mean(se^2) + (1+1/20)*var(est)
    data.frame(est = round(mean(est),3), se = round(sqrt(Tvar),3)) }
  mi_m4 <- pool_coef("owner", as.formula(paste("swls", sub("TEN","owner",f4))))
  mi_cx <- pool_coef("owner:strain", as.formula(paste("swls ~ owner * strain", sub("~ TEN","",f4))))
  rob$MI_M4_owner       <- data.frame(spec="(ii) MI m=20, M4", term="owner", est=mi_m4$est, se=mi_m4$se)
  rob$MI_interaction    <- data.frame(spec="(ii) MI m=20, owner*strain", term="owner:strain", est=mi_cx$est, se=mi_cx$se)
}
tab5 <- do.call(rbind, rob); rownames(tab5) <- NULL
write.csv(tab5, "output/tab5_robustness.csv", row.names = FALSE)
cat("E done: tab5\n")

## =============================================================================
## F. BFS-9 geography + deep dive into the vulnerable (income-poor) owners
## =============================================================================
## extra flags
d$edu_comp <- as.integer(d$edu3 == "compulsory")
d$italian  <- as.integer(d$lingreg == "Italian")
d$rural    <- as.integer(d$bfs3 == "ländlich")
d$widowed  <- as.integer(d$civstat == "widowed")
d$lowliq   <- as.integer(d$wealth_fin < 10000)           # <10k liquid (financial) wealth
d$cashpoor_owner <- as.integer(d$owner == 1 & d$strain == 1 & d$wealth_fin < 10000)
desF  <- mkdes(d[!is.na(d$w), ])
desFO <- subset(desF, owner == 1)

## F1. BFS-9: owner share and (within owners) income-poor share, by Gemeindetyp
own_by_bfs9 <- as.data.frame(svyby(~owner,  ~bfs9, subset(desF, !is.na(owner)),  svymean, na.rm = TRUE))
poor_by_bfs9<- as.data.frame(svyby(~strain, ~bfs9, subset(desFO,!is.na(strain)), svymean, na.rm = TRUE))
tab6 <- data.frame(bfs9 = own_by_bfs9$bfs9,
                   owner_pct = round(100*own_by_bfs9$owner,1),
                   poor_owner_pct = round(100*poor_by_bfs9$strain[match(own_by_bfs9$bfs9, poor_by_bfs9$bfs9)],1))
write.csv(tab6, "output/tab6_bfs9.csv", row.names = FALSE)

## F2. 2nd-pillar-for-home link: did income-poor owners finance via 2nd pillar?
pil_by_poor <- as.data.frame(svyby(~pil2_home, ~strain, subset(desFO,!is.na(strain)&!is.na(pil2_home)),
                                   svymean, na.rm = TRUE))
pil_link <- data.frame(group = c("non-poor owners","income-poor owners"),
                       pil2_home_pct = round(100*pil_by_poor$pil2_home,1))

## F3. profile of income-poor vs non-poor owners (weighted %)
prof_vars <- c("female","alone","widowed","edu_comp","italian","rural","mortgage","pil2_home","lowliq")
wmean_sub <- function(des, v) 100*as.numeric(coef(svymean(as.formula(paste0("~",v)), des, na.rm=TRUE))[1])
desP <- subset(desFO, strain==1); desN <- subset(desFO, strain==0)
tab7 <- data.frame(merkmal = prof_vars,
                   poor_owner = round(sapply(prof_vars, function(v) wmean_sub(desP,v)),1),
                   nonpoor_owner = round(sapply(prof_vars, function(v) wmean_sub(desN,v)),1))
write.csv(tab7, "output/tab7_vulnerable-owner-profile.csv", row.names = FALSE)
write.csv(pil_link, "output/tab7b_2pillar-link.csv", row.names = FALSE)

## F4. cash-poor / house-rich CORE: income-poor owner with <10k liquid; also subjective
cp_all   <- wmean_sub(desF, "cashpoor_owner")                      # % of all 65+
d$cashpoor_subj <- as.integer(d$owner==1 & d$strain==1 & (!is.na(d$liquidity) & d$liquidity==0))
desF2 <- mkdes(d[!is.na(d$w), ])
cp_subj  <- 100*as.numeric(coef(svymean(~cashpoor_subj, desF2, na.rm=TRUE))[1])
lowliq_poorown <- wmean_sub(desP, "lowliq")                        # % of income-poor owners with <10k liquid
mort_poorown   <- wmean_sub(desP, "mortgage")                      # % of income-poor owners still with mortgage
tab8 <- data.frame(
  metric = c("cash-poor house-rich core: % of ALL 65+ (owner, income-poor, <10k liquid)",
             "subjective variant: % of ALL 65+ (owner, income-poor, no buffer)",
             "% of income-poor owners with <10k liquid wealth",
             "% of income-poor owners still carrying a mortgage",
             "% of income-poor owners who used 2nd pillar for the home",
             "% of NON-poor owners who used 2nd pillar for the home"),
  value = round(c(cp_all, cp_subj, lowliq_poorown, mort_poorown,
                  pil_link$pil2_home_pct[2], pil_link$pil2_home_pct[1]), 1))
write.csv(tab8, "output/tab8_cashpoor-core.csv", row.names = FALSE)

## F5. figure: profile of income-poor vs non-poor owners
lbl <- c(female="Frau", alone="allein lebend", widowed="verwitwet",
         edu_comp="oblig. Schulbildung", italian="ital. Sprachregion", rural="ländliche Gemeinde",
         mortgage="mit Hypothek", pil2_home="PK-Bezug fürs Eigenheim", lowliq="<10k liquid")
gp <- tidyr::pivot_longer(tab7, c(poor_owner,nonpoor_owner), names_to="grp", values_to="pct")
gp$merkmal <- factor(lbl[gp$merkmal], levels = rev(lbl[prof_vars]))
gp$grp <- factor(gp$grp, c("poor_owner","nonpoor_owner"),
                 c("einkommensarme Eigentümer","übrige Eigentümer"))
p3 <- ggplot(gp, aes(pct, merkmal, fill = grp)) +
  geom_col(position = position_dodge(.7), width = .65) +
  scale_fill_manual(values = c("einkommensarme Eigentümer"=BLUE, "übrige Eigentümer"=GREY), name=NULL) +
  labs(x = "Gewichteter Anteil (%)", y = NULL,
       title = "Wer sind die einkommensarmen Eigentümer?",
       subtitle = "Merkmalsanteile: einkommensarme vs. übrige Eigentümer, 65+ (gewichtet)") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")
ggsave("output/fig3_vulnerable-owner-profile.png", p3, width = 8, height = 5, dpi = 300)
saveRDS(p3, "output/fig3_vulnerable-owner-profile.rds")
cat("F done: tab6, tab7(+b), tab8, fig3\n")

cat("\nALL DONE. Outputs in output/.\n")
