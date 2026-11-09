## =============================================================================
## 02b  Paper-2 material — Wohnen (SAS 2022, 65+)   [parked, NOT Selina's paper]
## -----------------------------------------------------------------------------
## Revision 2 (2026-11-09) scoped the WELL-BEING paper (01 + 02) and PARKED the
## poverty / Eigenmietwert / EL / geography-of-poverty material here, writing to
## output/_paper2/. Most of those tables (tab4c, tab6-8, tab10, tab13, tab14) do
## NOT depend on any variable that changed, so they were simply MOVED from output/.
## Only `home_value` changed (D-HOMEVALUE-RE: owner+other-RE now gets the median of
## no-other-RE owners instead of the full RE value), which feeds the imputed-rent
## poverty table — so tab9 + fig4 are RE-RUN here against the new home_value.
## =============================================================================

suppressWarnings(suppressMessages({ library(survey); library(dplyr); library(ggplot2) }))
options(survey.lonely.psu = "adjust"); set.seed(20221029)
P2 <- "output/_paper2"; dir.create(P2, showWarnings = FALSE, recursive = TRUE)
BLUE <- "#2b6cb0"; GREY <- "#9fb6cf"
d <- readRDS("data/wohnen_working.rds")
mkdes <- function(df) svydesign(ids = ~1, strata = ~strata_22, weights = ~w, data = df, nest = TRUE)
SAS_POVERTY_LINE <- 2279   # CHF/person/month (absolute SKOS/BFS 2020; silver definition)

## ---- imputed-rent poverty (dim 1), recomputed on the NEW home_value ----------
NET_YIELD <- 0.03
imp_rent <- function(yield) {
  ir_m  <- yield * d$home_value / 12                                   # monthly hh imputed rent (NEW home_value)
  ir_eq <- ifelse(!is.na(d$equiv_factor) & d$equiv_factor > 0, ir_m / d$equiv_factor, NA_real_)
  eqir  <- d$eqinc + ifelse(is.na(ir_eq), 0, ir_eq)
  list(eqinc_ir = eqir, poor_ir = as.integer(eqir < SAS_POVERTY_LINE)) }
ir <- imp_rent(NET_YIELD); d$poor_ir <- ir$poor_ir
desG <- mkdes(d[!is.na(d$w), ])
wp <- function(des, v) 100*as.numeric(coef(svymean(as.formula(paste0("~", v)), des, na.rm = TRUE))[1])
pov_own_before <- wp(subset(desG, owner==1), "strain"); pov_own_after <- wp(subset(desG, owner==1), "poor_ir")
pov_ten_before <- wp(subset(desG, owner==0), "strain"); pov_ten_after <- wp(subset(desG, owner==0), "poor_ir")
exit <- 100*as.numeric(coef(svymean(~I(poor_ir==0), subset(desG, owner==1 & strain==1), na.rm=TRUE))[2])
exit_sens <- sapply(c(0.02,0.04), function(y){ p <- imp_rent(y)$poor_ir
  dd <- d; dd$poor_ir <- p; dss <- subset(mkdes(dd[!is.na(dd$w),]), owner==1 & strain==1)
  100*as.numeric(coef(svymean(~I(poor_ir==0), dss, na.rm=TRUE))[2]) })
tab9 <- data.frame(
  metric = c("Armutsquote Eigentümer vor imput. Miete (%)","Armutsquote Eigentümer nach imput. Miete (%)",
             "Armutsquote Mieter vor (%)","Armutsquote Mieter nach (%)",
             "einkommensarme Eigentümer, die mit imput. Miete NICHT mehr arm sind (%)",
             "   dasselbe bei 2% Rendite (%)","   dasselbe bei 4% Rendite (%)"),
  value = round(c(pov_own_before, pov_own_after, pov_ten_before, pov_ten_after, exit, exit_sens[1], exit_sens[2]), 2))
write.csv(tab9, file.path(P2, "tab9_imputed-rent.csv"), row.names = FALSE)

pgir <- data.frame(Wohnform=rep(c("Eigentümer","Mieter"),each=2),
                   Einkommen=factor(rep(c("Bareinkommen","+ imput. Miete"),2), levels=c("Bareinkommen","+ imput. Miete")),
                   Armut=c(pov_own_before,pov_own_after,pov_ten_before,pov_ten_after))
p4 <- ggplot(pgir, aes(Wohnform, Armut, fill=Einkommen)) +
  geom_col(position=position_dodge(.7), width=.6) +
  geom_text(aes(label=sprintf("%.1f%%",Armut)), position=position_dodge(.7), vjust=-0.4, size=3.3) +
  scale_fill_manual(values=c(Bareinkommen=GREY, `+ imput. Miete`=BLUE), name=NULL) +
  labs(x=NULL, y="Einkommensarmutsquote (%)",
       title="Eigenmietwert: Eigentümer-Armut ist grösstenteils Papier-Armut",
       subtitle=sprintf("Armutsquote vor/nach imputierter Netto-Miete (%.0f%% des Hauswerts, NEUE Hauswert-Definition), 65+, gewichtet", 100*NET_YIELD)) +
  theme_minimal(base_size=11) + theme(legend.position="bottom", panel.grid.major.x=element_blank())
ggsave(file.path(P2, "fig4_imputed-rent.png"), p4, width=7.5, height=5, dpi=300)
saveRDS(p4, file.path(P2, "fig4_imputed-rent.rds"))
cat("02b done: re-ran tab9 + fig4 on new home_value ->", P2, "\n")
cat(sprintf("  owner poverty %.1f%% -> %.1f%% after imputed rent (exit %.1f%%)\n", pov_own_before, pov_own_after, exit))
