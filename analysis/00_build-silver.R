## =============================================================================
## 00  Build the "silver" SAS variables — SELF-CONTAINED, for 100% reproducibility
## -----------------------------------------------------------------------------
## This reproduces the altersdata data-warehouse "silver" variables (equivalised
## income, net/financial wealth, real-estate value, mortgage sum, income-poverty
## flag) DIRECTLY from the raw SAS 2022 ReleaseB `.sav`, so this repository runs
## end-to-end from the raw microdata WITHOUT the (separate, local-only) warehouse.
## Previously `01_setup-data.R` only `readRDS()`-ed a pre-built silver `.rds`; a
## reader could not see HOW those variables were derived. Now they are derived here.
##
## VENDORED — logic copied verbatim from the altersdata warehouse:
##   silver/helpers.R              — modified-OECD equivalisation, poverty flag
##   silver/sas/prep_sas_release.R — income/RE/assets/mortgage amount<->band merge
##                                   + the one cleaning step (NA household size -> 1)   [D-SASBASE, 2026-11-07]
##   silver/sas/core_sas.R         — build_core_sas(); incl. hh_wealth_re + hh_mortgage [D-HOUSINGWEALTH, 2026-11-09]
## Band midpoints come verbatim from the authoritative legacy setup
##   (script_NTU_setting-up_2024-08-19_PUBLISHED.R). The CANONICAL source remains
## the warehouse; keep this file in sync if the silver definitions change.
##
## Verified: build_silver_sas() on the ReleaseB `.sav` reproduces
## silver/sas/sas_2026-11-09.rds value-for-value (the 2022-11-22 and 2025-09-16
## ReleaseB exports are byte-identical, ids 1..4457). 4457 rows; the 56
## married-living-alone cases are INCLUDED (the old EL-study base wrongly dropped
## them — see warehouse D-SASBASE).
##
## Defines build_silver_sas(x) where x is a path to the `.sav` OR an already-loaded
## raw data.frame; returns a data.frame keyed on `pid` (= Respondent_ID).
## =============================================================================

## ---- shared definitions (from silver/helpers.R) -----------------------------
## Modified OECD equivalence: first adult 1.0, each further adult 0.5, each child 0.3.
.oecd_equiv_factor <- function(hhsize, n_children = 0,
                               first_adult = 1.0, add_adult = 0.5, child = 0.3) {
  hhsize   <- pmax(as.numeric(hhsize), 1)
  n_adults <- pmax(hhsize - n_children, 1)
  first_adult + (n_adults - 1) * add_adult + n_children * child
}
.equivalize  <- function(income, hhsize, n_children = 0) income / .oecd_equiv_factor(hhsize, n_children)
.poor_flag   <- function(resources, line) as.integer(resources < line)
.poverty_gap <- function(resources, line) (resources - line) / line

SAS_POVERTY_LINE <- 2279   # CHF/person/month, absolute SKOS/BFS 2020 (silver poverty line)
SAS_AT_RISK_LINE <- 2506   # 60% median, SILC/BFS 2020

## ---- prep: ReleaseB amount<->band merge + NA-hhsize->1 (prep_sas_release.R) --
## Reproduces the single legacy derivation core needs (merge exact amounts with
## banded-category midpoints) plus the one cleaning step the old set-up applied.
.prep_sas_release <- function(sas.22) {
  nn <- function(x) { x <- suppressWarnings(as.numeric(haven::zap_labels(x))); x[x < 0] <- NA; x }

  ## cleaning: missing household size -> 1 (single-person); else NA equiv. income (D-SASBASE)
  raw_hh <- nn(sas.22$pi_nbhoushold_22)
  sas.22$pi_nbhoushold_22[is.na(raw_hh)] <- 1

  ## INCOME: exact amount, else banded midpoint
  inccat <- nn(sas.22$fi_income_22_category)
  sas.22$virtual.amount.income.cat <- dplyr::case_when(
    inccat == 0 ~ median(0:2000),    inccat == 1 ~ median(2000:2300),
    inccat == 2 ~ median(2300:3000), inccat == 3 ~ median(3000:4000),
    inccat == 4 ~ median(4000:4800), inccat == 5 ~ median(4800:6000),
    inccat == 6 ~ median(6000:8000), inccat == 7 ~ median(8000:15000), TRUE ~ NA_real_)
  sas.22$income.amount.merged <- nn(sas.22$fi_income_22_amount)
  f <- is.na(sas.22$income.amount.merged); sas.22$income.amount.merged[f] <- sas.22$virtual.amount.income.cat[f]

  ## REAL ESTATE value
  recat <- nn(sas.22$fi_valrestestcat_22)
  sas.22$virtual.realest.amount <- dplyr::case_when(
    recat == 0 ~ median(0:250000),       recat == 1 ~ median(250000:500000),
    recat == 2 ~ median(500000:1000000), recat == 3 ~ median(1100000), TRUE ~ NA_real_)
  sas.22$realest.merged.val <- nn(sas.22$fi_valrealest_22)
  f <- is.na(sas.22$realest.merged.val); sas.22$realest.merged.val[f] <- sas.22$virtual.realest.amount[f]
  sas.22$realest.merged.val[is.na(sas.22$realest.merged.val)] <- 0   # no value reported -> 0

  ## ASSETS (financial wealth)
  ascat <- nn(sas.22$fi_assetscat_22)
  sas.22$virtual.asset.amount <- dplyr::case_when(
    ascat == 0 ~ median(0:4000),        ascat == 1 ~ median(4000:10000),
    ascat == 2 ~ median(10000:37500),   ascat == 3 ~ median(37500:60000),
    ascat == 4 ~ median(60000:100000),  ascat == 5 ~ median(100000:112500),
    ascat == 6 ~ median(112500:300000), ascat == 7 ~ median(300000:500000),
    ascat == 8 ~ median(500000:1000000),ascat == 9 ~ median(1000000:1500000), TRUE ~ NA_real_)
  sas.22$assetamount.merged <- nn(sas.22$fi_assetsamount_22)
  f <- is.na(sas.22$assetamount.merged); sas.22$assetamount.merged[f] <- sas.22$virtual.asset.amount[f]

  ## MORTGAGE sum
  mocat <- nn(sas.22$fi_summorgagecat_22)
  sas.22$virtual.morgage.amount <- dplyr::case_when(
    mocat == 0 ~ median(0:250000),       mocat == 1 ~ median(250000:500000),
    mocat == 2 ~ median(500000:1000000), mocat == 3 ~ median(1000001), TRUE ~ NA_real_)
  sas.22$morgage.merged.amount <- nn(sas.22$fi_summorgage_22)
  f <- which(nn(sas.22$fi_morgage_22) == 1 & is.na(sas.22$morgage.merged.amount))
  sas.22$morgage.merged.amount[f] <- sas.22$virtual.morgage.amount[f]
  f <- which(sas.22$realest.merged.val == 0 & is.na(sas.22$morgage.merged.amount))
  sas.22$morgage.merged.amount[f] <- 0

  sas.22
}

## ---- core: the main SAS variables every paper shares (core_sas.R) ------------
## One respondent per household (no household id). Income is monthly CHF.
.build_core_sas <- function(raw) {
  d <- data.frame(
    pid        = as.character(raw$Respondent_ID),
    age        = as.numeric(raw$pi_ageinterview_22),
    hhsize     = as.integer(raw$pi_nbhoushold_22),
    n_children = as.integer(ifelse(is.na(raw$pi_nbhoushold_22_child), 0, raw$pi_nbhoushold_22_child)),
    homeowner  = raw$fi_homeowner_22 %in% 1,
    hh_income  = as.numeric(raw$income.amount.merged),                       # monthly CHF
    hh_wealth_net = as.numeric(raw$assetamount.merged) +
                    as.numeric(raw$realest.merged.val) -
                    as.numeric(raw$morgage.merged.amount),                   # negatives kept
    hh_wealth_fin = as.numeric(raw$assetamount.merged),
    hh_wealth_re  = as.numeric(raw$realest.merged.val),                      # all real estate, CHF (0 if none)
    hh_mortgage   = as.numeric(raw$morgage.merged.amount),                   # sum of all mortgages, CHF (0 if none)
    weight     = as.numeric(raw$cross.design.weights.sample_22),
    income_band = raw$fi_income_22_category,
    stringsAsFactors = FALSE)
  d$income_eq    <- .equivalize(d$hh_income, d$hhsize, d$n_children)         # modified OECD
  d$poverty_line <- SAS_POVERTY_LINE
  d$poor         <- .poor_flag(d$income_eq, SAS_POVERTY_LINE)
  d$poor_atrisk  <- .poor_flag(d$income_eq, SAS_AT_RISK_LINE)
  d$poverty_gap  <- .poverty_gap(d$income_eq, SAS_POVERTY_LINE)
  d
}

## ---- public entry point -----------------------------------------------------
build_silver_sas <- function(x) {
  raw <- if (is.character(x)) {
    r <- as.data.frame(haven::read_sav(x)); names(r) <- trimws(names(r)); r
  } else as.data.frame(x)
  .build_core_sas(.prep_sas_release(raw))
}
