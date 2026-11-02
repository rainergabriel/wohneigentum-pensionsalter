## =============================================================================
## 01  Setup data — Wohnen und Wohnbefinden (SAS 2022), 65+
## -----------------------------------------------------------------------------
## Builds data/wohnen_working.rds for the housing / residential-well-being paper
## (see WRITING/analysis-plan_2026-10-30.md and HANDOFF/).
##
## SOURCES (both OUTSIDE this repo — microdata is never committed):
##  (A) SAS 2022 raw release matching codebook 2025-08-05:
##      data_sas22_ReleaseB_2025-09-16.sav  (SAS_DATA_PATH)
##  (B) SAS SILVER rebuilt on that same release (Step 0):
##      silver/sas/sas_release2025-09-16.rds  (SAS_SILVER)
##      → equivalised income, net/financial wealth, income-poverty flag. NOT
##        re-derived here, so the paper shares Rainer's canonical SAS definitions.
##      Joined on Respondent_ID; the join match rate is asserted ~100 %.
##
## NOTE: SAS has no dwelling-type / years-in-dwelling / rent-burden / move-intent
## items — those parts of the plan cannot be done with this survey (logged in
## HANDOFF). Sample restricted to 65+.
## =============================================================================

suppressWarnings(suppressMessages({library(haven); library(dplyr)}))

RAW <- Sys.getenv("SAS_DATA_PATH", unset =
  "/Users/gabn/Library/CloudStorage/GoogleDrive-rainer.gabriel@gmail.com/My Drive/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/4_Erhebung/datenbereinigung/data_sas22_ReleaseB_2025-09-16.sav")
SILVER <- Sys.getenv("SAS_SILVER", unset =
  "/Users/gabn/Library/CloudStorage/GoogleDrive-rainer.gabriel@gmail.com/My Drive/Documents/Work/ACTIVE/ZHAW/3.1 DATA WAREHOUSE/altersdata/silver/sas/sas_release2025-09-16.rds")

dir.create("data", showWarnings = FALSE)
na_neg <- function(x) { x <- suppressWarnings(as.numeric(zap_labels(x))); x[x < 0] <- NA; x }

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

## ---- join SILVER income / wealth / poverty (on Respondent_ID) ---------------
sil <- readRDS(SILVER) %>%
  transmute(id = as.character(pid),
            eqinc = income_eq, hh_income = hh_income,
            wealth_net = hh_wealth_net, wealth_fin = hh_wealth_fin, poor_silver = poor)
d <- left_join(d, sil, by = "id")
match_rate <- mean(d$id %in% sil$id)
cat(sprintf("Silver join match rate: %.1f%% (%d of %d)\n", 100*match_rate, sum(d$id %in% sil$id), nrow(d)))
stopifnot("Silver join match rate < 99% — check Step 0 / IDs" = match_rate > 0.99)

## income-based financial strain (MAIN) = silver income-poverty flag;
## subjective strain (sensitivity) = (great) difficulty making ends meet OR no buffer
d <- d %>% mutate(
  strain      = poor_silver,
  strain_subj = as.integer((!is.na(endsmeet) & endsmeet <= 1) | (!is.na(liquidity) & liquidity == 0)),
  eqinc_tert  = cut(eqinc, quantile(eqinc, c(0,1/3,2/3,1), na.rm = TRUE),
                    labels = c("low","mid","high"), include.lowest = TRUE),
  ## home value for imputed rent: amount -> category midpoint; owners w/o value -> owner median; non-owners 0
  home_value  = ifelse(!is.na(realest_amt), realest_amt, realest_cat),
  equiv_factor= ifelse(!is.na(eqinc) & eqinc > 0, hh_income / eqinc, NA_real_)  # hh_income / equiv income
)

## finalise home value: owners with still-missing value -> weighted owner median;
## non-owners -> 0 (imputed rent applies to the self-occupied home of owners)
own_med <- median(d$home_value[d$owner == 1], na.rm = TRUE)
d$home_value <- ifelse(d$owner == 1 & is.na(d$home_value), own_med,
                       ifelse(d$owner == 0, 0, d$home_value))

## ---- restrict to 65+ and save ----------------------------------------------
d65 <- d %>% filter(!is.na(age) & age >= 65)
saveRDS(d65, "data/wohnen_working.rds")

## ---- n-table of analysis variables -----------------------------------------
cat("\n=== analysis frame: 65+ ===  n =", nrow(d65), " (weighted design var `w`, strata `strata_22`)\n")
miss <- sapply(d65, function(x) sum(is.na(x)))
cat("\nmissing per variable:\n"); print(miss)
cat("\ntenure3:\n");   print(table(d65$tenure3, useNA = "ifany"))
cat("owner:\n");       print(table(d65$owner, useNA = "ifany"))
cat("strain (silver poverty):\n"); print(table(d65$strain, useNA = "ifany"))
cat("strain_subj:\n"); print(table(d65$strain_subj, useNA = "ifany"))
cat("SWLS summary:\n");print(summary(d65$swls))
cat("\nSaved data/wohnen_working.rds\n")
