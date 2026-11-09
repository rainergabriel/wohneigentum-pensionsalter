## =============================================================================
## 01  Setup data — Wohnen und Wohnbefinden (SAS 2022), 65+
## -----------------------------------------------------------------------------
## Builds data/wohnen_working.rds for the housing / residential-well-being paper
## (see WRITING/analysis-plan_2026-10-30.md and HANDOFF/).
##
## SOURCE — a SINGLE raw file (OUTSIDE this repo; microdata is never committed):
##   SAS 2022 raw release matching codebook 2025-08-05:
##   data_sas22_ReleaseB_2025-09-16.sav  (SAS_DATA_PATH)
##
## SILVER variables (equivalised income, net/financial wealth, real-estate value,
## mortgage sum, income-poverty flag) are DERIVED IN-REPO from that raw file by
## `00_build-silver.R` (build_silver_sas) — the warehouse silver logic vendored
## verbatim (helpers + prep merge + build_core_sas, incl. hh_wealth_re/hh_mortgage).
## So the whole pipeline is 100% reproducible from the raw `.sav` alone, with NO
## dependency on the (separate, local-only) altersdata warehouse. The in-repo build
## was verified value-for-value against the warehouse `sas_2026-11-09.rds`.
##
## NOTE: SAS has no dwelling-type / years-in-dwelling / rent-burden / move-intent
## items — those parts of the plan cannot be done with this survey (logged in
## HANDOFF). Sample restricted to 65+.
## =============================================================================

suppressWarnings(suppressMessages({library(haven); library(dplyr)}))
source("00_build-silver.R")   # in-repo silver derivation (build_silver_sas); see its header

RAW <- Sys.getenv("SAS_DATA_PATH", unset =
  "/Users/gabn/Library/CloudStorage/GoogleDrive-rainer.gabriel@gmail.com/My Drive/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/4_Erhebung/datenbereinigung/data_sas22_ReleaseB_2025-09-16.sav")

dir.create("data", showWarnings = FALSE)
na_neg <- function(x) { x <- suppressWarnings(as.numeric(zap_labels(x))); x[x < 0] <- NA; x }

## ---- _run.log: tee key decisions / counts to a log the brief asks for --------
LOG <- "_run.log"
cat(sprintf("=== 01_setup-data.R run %s ===\n", Sys.time()), file = LOG)
logline <- function(...) { msg <- paste0(...); cat(msg, "\n"); cat(msg, "\n", file = LOG, append = TRUE) }

## weighted median (design weight w); no survey pkg dependency in setup
wmedian <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  x <- x[ok]; w <- w[ok]; o <- order(x); x <- x[o]; w <- w[o]
  cw <- cumsum(w) / sum(w)
  x[which(cw >= 0.5)[1]]
}

## housing-finance constants (no magic numbers)
MORTGAGE_RATE <- 0.0125   # hypothek. Referenzzinssatz BWO 2022 (open-question: level/source); sensitivity 0.025
NET_YIELD     <- 0.03     # net imputed rental yield (gross ~4% minus ~1% upkeep); sensitivity 0.02/0.04

raw <- read_sav(RAW); names(raw) <- trimws(names(raw))
cat("Raw release:", nrow(raw), "respondents.\n")

## ---- derive from raw --------------------------------------------------------
d <- raw %>% transmute(
  id        = as.character(Respondent_ID),
  age       = na_neg(pi_ageinterview_22),
  age_grp   = factor(ifelse(age >= 80, "80+", "65-79"), levels = c("65-79","80+")),
  female    = case_when(na_neg(pi_sex_22) == 2 ~ 1L, na_neg(pi_sex_22) == 1 ~ 0L),
  civstat   = factor(case_when(na_neg(pi_civstat_22) %in% c(0,1) ~ "married/partnered",
                               na_neg(pi_civstat_22) == 2 ~ "single",
                               na_neg(pi_civstat_22) == 3 ~ "divorced",
                               na_neg(pi_civstat_22) == 4 ~ "widowed"),
                     levels = c("married/partnered","widowed","divorced","single")),
  alone     = as.integer(na_neg(pi_livingalone_22) == 1),
  swiss     = as.integer(na_neg(pi_swissnational_22) == 1),
  nchild    = na_neg(pi_nbchildren_22),
  haskids   = as.integer(na_neg(pi_nbchildren_22) >= 1),
  edu3      = factor(case_when(na_neg(se_edu_22) <= 3 ~ "compulsory",
                               na_neg(se_edu_22) <= 7 ~ "upper secondary",
                               na_neg(se_edu_22) <= 12 ~ "tertiary"),
                     levels = c("upper secondary","compulsory","tertiary")),
  lingreg   = factor(case_when(na_neg(env_lingregion_22) %in% c(1,4) ~ "German",
                               na_neg(env_lingregion_22) == 2 ~ "French",
                               na_neg(env_lingregion_22) == 3 ~ "Italian"),
                     levels = c("German","French","Italian")),
  degurba   = factor(na_neg(env_degurba_22), levels = 1:3,
                     labels = c("urban","peri-urban","rural")),
  ## BFS-Gemeindetypologie 2012, 9 Typen (env_BFStypo9_22, codes 11..33) + 3er-Gruppe
  bfs9      = factor(na_neg(env_BFStypo9_22),
                     levels = c(11,12,13,21,22,23,31,32,33),
                     labels = c("städt. Grossagglo","städt. mittl. Agglo","städt. klein/ausserh.",
                                "periurban hohe Dichte","periurban mittl. Dichte","periurban geringe Dichte",
                                "ländl. Zentrum","ländl. zentral gelegen","ländl. peripher")),
  bfs3      = factor(substr(sprintf("%02d", na_neg(env_BFStypo9_22)), 1, 1),
                     levels = c("1","2","3"), labels = c("städtisch","periurban","ländlich")),
  ## tenure (decision 2): tenant = not self-occupied owner; split owners by other RE
  homeowner = na_neg(fi_homeowner_22),
  realest   = na_neg(fi_realest_22),
  owner     = as.integer(na_neg(fi_homeowner_22) == 1),
  tenure3   = factor(case_when(na_neg(fi_homeowner_22) == 0 ~ "tenant",
                               na_neg(fi_homeowner_22) == 1 & na_neg(fi_realest_22) == 0 ~ "owner, no other RE",
                               na_neg(fi_homeowner_22) == 1 & na_neg(fi_realest_22) == 1 ~ "owner + other RE"),
                     levels = c("tenant","owner, no other RE","owner + other RE")),
  mortgage  = as.integer(na_neg(fi_morgage_22) == 1),            # use within owners only
  pil2_home = as.integer(na_neg(fi_2pil4home_22) == 1),          # 2nd-pillar capital for the home
  pil2_any  = as.integer(na_neg(fi_cashout2pil_22) == 1),        # any 2nd-pillar withdrawal
  el_receipt= as.integer(na_neg(fi_incsourc_22_el) == 1),        # Ergänzungsleistungen receipt
  knowel    = as.integer(na_neg(fi_knowel_22) == 1),             # knows about EL (filtered item)
  considerel= as.integer(na_neg(fi_considerel_22) == 1),         # ever applied/considered EL (filtered)
  ## care / aging in place
  infcare   = as.integer(na_neg(cs_getinfcare_22) == 1),         # received informal care, last 12 mo
  comcare   = as.integer(na_neg(cs_getcomcare_22) == 1),         # received formal/community care
  careinsur = as.integer(na_neg(mu_careinsur_22) == 1),          # supplementary care insurance (filtered)
  ## geography extras
  mountain  = as.integer(na_neg(env_ismountainregion_22) == 1),  # mountain region
  bfs_coll  = factor(dplyr::case_when(
                 na_neg(env_BFStypo9_22) == 11 ~ "Grosszentrum",
                 na_neg(env_BFStypo9_22) %in% c(12,13) ~ "übrige städtisch",
                 na_neg(env_BFStypo9_22) %in% c(21,22,23) ~ "periurban",
                 na_neg(env_BFStypo9_22) %in% c(31,32) ~ "ländlich zentral",
                 na_neg(env_BFStypo9_22) == 33 ~ "ländlich peripher"),
               levels = c("Grosszentrum","übrige städtisch","periurban","ländlich zentral","ländlich peripher")),
  ## home value (for imputed rent): reported amount, fallback to category midpoint
  realest_amt = na_neg(fi_valrealest_22),
  realest_cat = dplyr::case_when(na_neg(fi_valrestestcat_22)==0~125000, na_neg(fi_valrestestcat_22)==1~375000,
                                 na_neg(fi_valrestestcat_22)==2~750000, na_neg(fi_valrestestcat_22)==3~1100000,
                                 TRUE~NA_real_),
  ## health
  srh       = na_neg(ph_srh_22),                                 # 0 poor .. 4 excellent
  srh_poor  = as.integer(na_neg(ph_srh_22) <= 1),
  gali_any  = as.integer(na_neg(ph_gali_22) <= 1),               # any activity limitation
  dep_score = 4 - na_neg(mh_depression_22),                      # 0 never .. 4 always
  ## financial situation (subjective)
  endsmeet  = na_neg(fi_endsmeet_22),                            # 0 great diff .. 3 no problem
  endsmeet_f= factor(na_neg(fi_endsmeet_22), levels = 0:3,
                     labels = c("great difficulty","difficulty","fairly easily","easily")),
  liquidity = as.integer(na_neg(fi_suddenexp_22) == 1),          # can cover unexpected expense
  ## outcome + weights/design
  swls      = na_neg(ls_genlifesatdiener_numscore_22),           # Diener SWLS 5-35
  swls_cat  = na_neg(ls_genlifesatdiener_cat_22),
  w         = as.numeric(`cross.design.weights.sample_22`),
  strata_22 = as.character(strata_22)                            # canton-stratum code, e.g. "ZH-1"
)

## ---- SILVER income / wealth / poverty — DERIVED IN-REPO from the same raw -----
## build_silver_sas() (00_build-silver.R) reproduces the warehouse silver variables
## from the raw release, so the pipeline is self-contained. Includes hh_wealth_re
## (all real estate, CHF) and hh_mortgage (sum of all mortgages, CHF) — the amounts
## the burden/LTV/imputed-net-rent analyses need. hh_wealth_net = fin + re − mortgage.
sil <- build_silver_sas(raw) %>%
  transmute(id = as.character(pid),
            eqinc = income_eq, hh_income = hh_income,
            wealth_net = hh_wealth_net, wealth_fin = hh_wealth_fin,
            wealth_re = hh_wealth_re, mortgage_silver = hh_mortgage,
            poor_silver = poor)
d <- left_join(d, sil, by = "id")
match_rate <- mean(d$id %in% sil$id)
logline(sprintf("Silver (in-repo build) join match rate: %.1f%% (%d of %d)", 100*match_rate, sum(d$id %in% sil$id), nrow(d)))
stopifnot("Silver join match rate < 99% — check 00_build-silver.R / IDs" = match_rate > 0.99)

## =============================================================================
## OBJECTIVE financial resources (CHF amounts & yes/no facts) — D-* in brief
## =============================================================================
d <- d %>% mutate(
  ## MAIN strain = silver income-poverty flag (OBJECTIVE). endsmeet / suddenexp are
  ## SUBJECTIVE and never define strain (brief, Gabriel & Hümbelin 2026).
  strain      = poor_silver,
  eqinc_tert  = cut(eqinc, quantile(eqinc, c(0,1/3,2/3,1), na.rm = TRUE),
                    labels = c("low","mid","high"), include.lowest = TRUE),
  equiv_factor= ifelse(!is.na(eqinc) & eqinc > 0, hh_income / eqinc, NA_real_),  # hh_income / eq income
  ## --- rename: these two are SUBJECTIVE assessments (M5 only; never classify) --
  suddenexp_subj = liquidity,                                   # can cover an unexpected expense (was `liquidity`)
  ## --- wealth transforms (objective), explicit "missing" tertile level --------
  wealth_fin_log = log1p(pmax(wealth_fin, 0)),
  wealth_net_log = log1p(pmax(wealth_net, 0))
)
wq_fin <- quantile(d$wealth_fin, c(0,1/3,2/3,1), na.rm = TRUE)
d$wealth_fin_tert <- cut(d$wealth_fin, wq_fin, labels = c("low","mid","high"), include.lowest = TRUE)
d$wealth_fin_tert <- factor(ifelse(is.na(d$wealth_fin_tert), "missing", as.character(d$wealth_fin_tert)),
                            levels = c("mid","low","high","missing"))
wq_net <- quantile(d$wealth_net, c(0,1/3,2/3,1), na.rm = TRUE)
d$wealth_net_tert <- cut(d$wealth_net, wq_net, labels = c("low","mid","high"), include.lowest = TRUE)
d$wealth_net_tert <- factor(ifelse(is.na(d$wealth_net_tert), "missing", as.character(d$wealth_net_tert)),
                            levels = c("mid","low","high","missing"))

## --- HOME VALUE (owners): amount -> category -> median-of-no-other-RE owners --
## D-HOMEVALUE-RE: owner, no other RE -> hh_wealth_re (= the home for them);
## owner + other RE -> weighted median hh_wealth_re of the no-other-RE owners
## (hh_wealth_re overstates the home when other RE exists). Non-owners -> 0.
own_no_re <- d$tenure3 == "owner, no other RE" & !is.na(d$tenure3)
own_re    <- d$tenure3 == "owner + other RE"  & !is.na(d$tenure3)
home_src <- dplyr::case_when(
  d$owner == 0                     ~ NA_character_,     # non-owner
  own_re                           ~ "median",         # owner + other RE -> always median
  !is.na(d$realest_amt)            ~ "amount",          # no-other-RE owner, reported amount
  !is.na(d$realest_cat)            ~ "category",        # no-other-RE owner, category only
  TRUE                             ~ "median")          # no-other-RE owner, neither reported
wmed_home <- wmedian(d$wealth_re[own_no_re & d$wealth_re > 0], d$w[own_no_re & d$wealth_re > 0])
d$home_value <- dplyr::case_when(
  d$owner == 0            ~ 0,
  home_src == "amount"    ~ d$realest_amt,
  home_src == "category"  ~ d$realest_cat,
  TRUE                    ~ wmed_home)                 # median fallback (owner+RE, or no report)
## sensitivity variant (D-HOMEVALUE-RE off): owner + other RE keeps the FULL RE value
d$home_value_full <- ifelse(own_re, d$wealth_re, d$home_value)
logline(sprintf("home_value source among owners (n=%d): amount=%d, category=%d, median=%d (weighted median home = %s CHF)",
        sum(d$owner == 1, na.rm = TRUE),
        sum(home_src == "amount", na.rm = TRUE), sum(home_src == "category", na.rm = TRUE),
        sum(home_src == "median", na.rm = TRUE), format(round(wmed_home), big.mark = "'")))

## --- MORTGAGE amount, LTV, cost, burden (owners) -----------------------------
## mortgage_amt = hh_mortgage; 0 for mortgage-free owners (fi_morgage_22 == 0).
d$mortgage_amt <- dplyr::case_when(
  d$owner == 0                                     ~ NA_real_,  # owner-only construct
  d$mortgage == 0                                  ~ 0,          # owner, no mortgage
  d$mortgage == 1 & !is.na(d$mortgage_silver)      ~ d$mortgage_silver,
  TRUE                                             ~ NA_real_)   # owner, mortgage but amount unknown
d$has_mortgage <- as.integer(d$mortgage_amt > 0)
d$has_mortgage[d$owner == 0 & !is.na(d$owner)] <- 0L   # tenants: 0-for-tenants (model use)
## cross-check vs the fi_morgage_22 yes/no flag; report disagreements
disagree <- with(d, owner == 1 & !is.na(mortgage) & !is.na(has_mortgage) & mortgage != has_mortgage)
unknown_amt <- with(d, owner == 1 & mortgage == 1 & is.na(mortgage_amt))
logline(sprintf("mortgage: owners with flag==1 but unknown amount = %d (-> has_mortgage NA); flag vs amount disagreements = %d",
        sum(unknown_amt, na.rm = TRUE), sum(disagree, na.rm = TRUE)))
## loan-to-value (Belehnungsgrad), winsorised at 1.5; home_value>0 for all owners.
## ltv = 0 for tenants (model 0-for-tenants, brief Step 3); NA for owners w/ unknown amount.
d$ltv <- ifelse(d$owner == 1, pmin(d$mortgage_amt / d$home_value, 1.5),
                ifelse(d$owner == 0 & !is.na(d$owner), 0, NA_real_))
## ltv_cat is OWNER-ONLY (descriptive tab4/fig3): tenants NA
d$ltv_cat <- factor(dplyr::case_when(
  d$owner != 1 | is.na(d$owner) | is.na(d$ltv) ~ NA_character_,
  d$ltv == 0        ~ "0",
  d$ltv <= 0.33     ~ "(0,.33]",
  d$ltv <= 0.66     ~ "(.33,.66]",
  TRUE              ~ ">.66"),
  levels = c("0","(0,.33]","(.33,.66]",">.66"))
## monthly mortgage interest cost and burden (share of monthly hh income)
d$mortgage_cost_m  <- d$mortgage_amt * MORTGAGE_RATE / 12
d$mortgage_burden  <- ifelse(d$owner == 1 & !is.na(d$hh_income) & d$hh_income > 0,
                             d$mortgage_cost_m / d$hh_income, NA_real_)
d$mort_burden_15 <- as.integer(d$mortgage_burden > 0.15)
d$mort_burden_30 <- as.integer(d$mortgage_burden > 0.30)

## --- IMPUTED RENT (owners live rent-free): gross and net of mortgage cost ----
d$imp_rent_m      <- NET_YIELD * d$home_value / 12
d$imp_rent_net_m  <- pmax(d$imp_rent_m - ifelse(is.na(d$mortgage_cost_m), 0, d$mortgage_cost_m), 0)
d$imp_rent_eq     <- ifelse(!is.na(d$equiv_factor) & d$equiv_factor > 0, d$imp_rent_m     / d$equiv_factor, NA_real_)
d$imp_rent_net_eq <- ifelse(!is.na(d$equiv_factor) & d$equiv_factor > 0, d$imp_rent_net_m / d$equiv_factor, NA_real_)
## non-owners: imputed rent is 0 (they pay market rent); keep eq versions 0 too
d$imp_rent_eq[d$owner == 0]     <- 0
d$imp_rent_net_eq[d$owner == 0] <- 0

## ---- restrict to 65+ and save ----------------------------------------------
d65 <- d %>% filter(!is.na(age) & age >= 65)
saveRDS(d65, "data/wohnen_working.rds")

## ---- n-table of analysis variables -----------------------------------------
logline(sprintf("analysis frame 65+: n = %d (weighted design var `w`, strata `strata_22`)", nrow(d65)))
logline("tenure3: " , paste(names(table(d65$tenure3)), table(d65$tenure3), sep="=", collapse=", "))
logline(sprintf("owners: has_mortgage=1 %d, mortgage-free %d, unknown %d",
        sum(d65$has_mortgage == 1, na.rm = TRUE), sum(d65$has_mortgage == 0, na.rm = TRUE),
        sum(d65$owner == 1 & is.na(d65$has_mortgage), na.rm = TRUE)))
logline(sprintf("strain (silver income-poverty) among 65+: %d poor / %d total",
        sum(d65$strain == 1, na.rm = TRUE), sum(!is.na(d65$strain))))
logline(sprintf("home_value (owners, CHF): median %s | mortgage_amt (owners w/ mortgage): median %s | ltv (owners): median %.2f",
        format(round(wmedian(d65$home_value[d65$owner==1], d65$w[d65$owner==1])), big.mark="'"),
        format(round(wmedian(d65$mortgage_amt[d65$has_mortgage==1], d65$w[d65$has_mortgage==1])), big.mark="'"),
        wmedian(d65$ltv[d65$owner==1], d65$w[d65$owner==1])))
miss <- sapply(d65, function(x) sum(is.na(x)))
cat("\nmissing per variable:\n"); print(miss)
cat("\ntenure3:\n");   print(table(d65$tenure3, useNA = "ifany"))
cat("ltv_cat (owners):\n"); print(table(d65$ltv_cat, useNA = "ifany"))
cat("SWLS summary:\n");print(summary(d65$swls))
logline("Saved data/wohnen_working.rds")
