# Wohneigentum im Pensionsalter — Online-Supplement

**Sonnen- und Schattenseiten des Eigenheims im Rentenalter: Eigentum, objektive Ressourcen und subjektives Wohlbefinden (Swiss Aging Survey 2022, 65+).**

Rainer Gabriel & Selina N.N. (ZHAW). Supplement zum Manuskript; ersetzt den Anhang.

## Daten & Methoden (Kurz)

Swiss Aging Survey 2022 (SAS, ReleaseB), Personen ab 65 (analytische n = 3'313; gemeinsame Complete-Case-Stichprobe der genesteten Modelle n = 2502). Gewichtet (Designgewicht, kantonale Schichtung). Zielgrösse: Diener-SWLS (5–35). Die finanzielle Lage wird getrennt in einen **objektiven** Block (äquivalisiertes Einkommen, Finanzvermögens-Terzile mit einer „fehlend"-Kategorie; M4) und einen **subjektiven** Block (mit dem Einkommen auskommen, unerwartete Ausgabe decken; M5) — Letzterer erklärt nur, klassiert aber nie (Gabriel & Hümbelin 2026). Owner-spezifische Grössen (Hypothek, Belehnungsgrad, PK-Vorbezug, imputierte Miete) werden separat innerhalb der Eigentümer untersucht (S4/S5). Vermögens-Item-Nonresponse wird zusätzlich per multipler Imputation (mice, m = 20) adressiert. Keine Mikrodaten im Supplement.

Reproduzierbar: der Code (`code/`) leitet alle Silver-Variablen direkt aus dem Rohdatensatz ab; die 
Mikrodaten sind über SWISSUbase / Pro Senectute erhältlich und hier **nicht** enthalten.

---

## S1 — Deskription nach Wohnform
| Block | Merkmal (gewichtet) | Gesamt | Miete | Eigentum o. w. RE | Eigentum m. w. RE |
| --- | --- | --- | --- | --- | --- |
| N | N (ungewichtet) | 3204 | 1459 | 1153 | 592 |
| A. Soziodemografie & Gesundheit | Alter (Mittel) | 75.5 | 76.7 | 74.9 | 74 |
| A. Soziodemografie & Gesundheit | Frauen (%) | 53.7 | 59.1 | 50.9 | 45.6 |
| A. Soziodemografie & Gesundheit | allein lebend (%) | 33.6 | 49.3 | 23.9 | 15.7 |
| A. Soziodemografie & Gesundheit | Tertiärbildung (%) | 36.6 | 28.7 | 37.2 | 52.7 |
| A. Soziodemografie & Gesundheit | (sehr) schlechte Gesundheit (%) | 30.3 | 34.1 | 28.4 | 25 |
| B. Objektive Ressourcen | Äquiv.-Einkommen Median (CHF) | 4000 | 3400 | 4333.3 | 5000 |
| B. Objektive Ressourcen | Finanzvermögen Median (CHF) | 150000 | 50000 | 206250 | 360000 |
| B. Objektive Ressourcen | Nettovermögen Median (CHF) | 4e+05 | 50000 | 696250 | 1257000 |
| B. Objektive Ressourcen | fehlende Vermögensangabe (%) | 30 | 30.4 | 27.1 | 24.3 |
| B. Objektive Ressourcen | mit Hypothek (%) | 39.4 | 0 | 69.7 | 74.4 |
| B. Objektive Ressourcen | Hypothekarsumme Median (CHF; mit Hyp.) | 350000 |  | 3e+05 | 5e+05 |
| B. Objektive Ressourcen | Belehnungsgrad (LTV) Median (Eigent.) | 0.3 |  | 0.3 | 0.5 |
| B. Objektive Ressourcen | Hypothekenlast % Eink. Median (mit Hyp.) | 5.5 |  | 4.8 | 6.8 |
| B. Objektive Ressourcen | Hauswert Median (Eigent.; CHF) | 750000 |  | 750000 | 750000 |
| B. Objektive Ressourcen | imput. Miete äquiv. Median (CHF/Mt) | 1250 |  | 1304.3 | 1250 |
| B. Objektive Ressourcen | PK-Vorbezug fürs Eigenheim (%) | 11.2 | 3.7 | 15.8 | 19.3 |
| B. Objektive Ressourcen | PK-Bezug 2. Säule gesamt (%) | 27.9 | 20.6 | 29 | 41.4 |
| C. Subjektive Einschätzung | auskommen: leicht (%) | 49.9 | 37.9 | 52.9 | 71.2 |
| C. Subjektive Einschätzung | auskommen: grosse Mühe (%) | 2.6 | 4.5 | 1.8 | 0.3 |
| C. Subjektive Einschätzung | kann unerwartete Ausgabe decken (%) | 86.4 | 74.1 | 93.8 | 98.7 |

*Anmerkung: Gewichtet. CHF-Werte sind Mediane, übrige Zellen Anteile in %. N ungewichtet.*


## S2 — Genestete Modelle M0–M5 (SWLS)
| Spezifikation | Modell | Wohnform (vs. Mietende) | b (SE) | 95%-KI | R2 | N |
| --- | --- | --- | --- | --- | --- | --- |
| tenure3 | M0 | owner, no other RE | 1.33*** (0.30) | [0.75; 1.91] | 0.024 | 2502 |
| tenure3 | M0 | owner + other RE | 2.22*** (0.36) | [1.51; 2.94] | 0.024 | 2502 |
| tenure3 | M1 | owner, no other RE | 0.95** (0.31) | [0.34; 1.56] | 0.051 | 2502 |
| tenure3 | M1 | owner + other RE | 1.64*** (0.39) | [0.87; 2.41] | 0.051 | 2502 |
| tenure3 | M2 | owner, no other RE | 0.88** (0.28) | [0.33; 1.43] | 0.217 | 2502 |
| tenure3 | M2 | owner + other RE | 1.53*** (0.36) | [0.82; 2.24] | 0.217 | 2502 |
| tenure3 | M3 | owner, no other RE | 0.83** (0.28) | [0.29; 1.38] | 0.224 | 2502 |
| tenure3 | M3 | owner + other RE | 1.58*** (0.37) | [0.86; 2.31] | 0.224 | 2502 |
| tenure3 | M4 | owner, no other RE | 0.58* (0.27) | [0.04; 1.11] | 0.232 | 2502 |
| tenure3 | M4 | owner + other RE | 1.21** (0.36) | [0.51; 1.91] | 0.232 | 2502 |
| tenure3 | M5 | owner, no other RE | 0.30 (0.27) | [-0.23; 0.82] | 0.267 | 2502 |
| tenure3 | M5 | owner + other RE | 0.72* (0.35) | [0.03; 1.41] | 0.267 | 2502 |
| owner (binär) | M0 | owner | 1.64*** (0.27) | [1.11; 2.17] | 0.021 | 2502 |
| owner (binär) | M1 | owner | 1.16*** (0.29) | [0.59; 1.73] | 0.048 | 2502 |
| owner (binär) | M2 | owner | 1.07*** (0.26) | [0.56; 1.59] | 0.216 | 2502 |
| owner (binär) | M3 | owner | 1.06*** (0.26) | [0.54; 1.58] | 0.223 | 2502 |
| owner (binär) | M4 | owner | 0.75** (0.26) | [0.25; 1.25] | 0.231 | 2502 |
| owner (binär) | M5 | owner | 0.40 (0.25) | [-0.09; 0.89] | 0.267 | 2502 |

### S2 — Multiple Imputation (m = 20), Vermögensblock
| Modell | Wohnform (vs. Mietende) | b (SE), MI m=20 |
| --- | --- | --- |
| M4 | owner, no other RE | 0.58 (0.27) |
| M4 | owner + other RE | 1.16 (0.36) |
| M5 | owner, no other RE | 0.29 (0.27) |
| M5 | owner + other RE | 0.69 (0.36) |

*Anmerkung: Gewichtete lineare Regression auf SWLS. Sterne: * p<.05, ** p<.01, *** p<.001. CC = Complete-Case; MI = multiple Imputation (m=20) des Vermögens-/Einkommensblocks.*


## S2b — Vollständige Regressionsübersicht (alle Koeffizienten, M0–M5)
| Term | M0 | M1 | M2 | M3 | M4 | M5 |
| --- | --- | --- | --- | --- | --- | --- |
| tenure3owner, no other RE | 1.33*** (0.30) | 0.95** (0.31) | 0.88** (0.28) | 0.83** (0.28) | 0.58* (0.27) | 0.29 (0.27) |
| tenure3owner + other RE | 2.22*** (0.36) | 1.64*** (0.39) | 1.53*** (0.36) | 1.58*** (0.37) | 1.21*** (0.36) | 0.72* (0.35) |
| age |  | 0.04* (0.02) | 0.07*** (0.02) | 0.07*** (0.02) | 0.07*** (0.02) | 0.04* (0.02) |
| female |  | 0.44 (0.28) | 1.14*** (0.27) | 1.16*** (0.27) | 1.21*** (0.27) | 1.04*** (0.27) |
| civstatwidowed |  | 1.42** (0.52) | 0.97* (0.48) | 1.00* (0.47) | 1.07* (0.47) | 0.82 (0.44) |
| civstatdivorced |  | -0.17 (0.54) | -0.33 (0.51) | -0.29 (0.51) | -0.12 (0.50) | -0.12 (0.47) |
| civstatsingle |  | 0.69 (0.67) | 0.44 (0.61) | 0.46 (0.61) | 0.61 (0.60) | 0.57 (0.58) |
| alone |  | -1.89*** (0.50) | -1.48** (0.45) | -1.45** (0.45) | -1.42** (0.45) | -1.16** (0.42) |
| edu3compulsory |  | -0.76 (0.41) | -0.59 (0.37) | -0.54 (0.37) | -0.42 (0.37) | -0.40 (0.36) |
| edu3tertiary |  | 0.66* (0.30) | 0.60* (0.27) | 0.59* (0.27) | 0.39 (0.28) | 0.30 (0.28) |
| swiss |  | 0.87 (0.67) | 0.30 (0.61) | 0.16 (0.61) | 0.02 (0.60) | -0.33 (0.56) |
| haskids |  | -0.20 (0.44) | -0.62 (0.40) | -0.66 (0.39) | -0.54 (0.39) | -0.34 (0.39) |
| gali_any |  |  | -0.30 (0.25) | -0.35 (0.25) | -0.34 (0.25) | -0.20 (0.25) |
| dep_score |  |  | -2.28*** (0.17) | -2.27*** (0.17) | -2.24*** (0.16) | -1.99*** (0.16) |
| srh_poor |  |  | -1.58*** (0.29) | -1.58*** (0.29) | -1.56*** (0.29) | -1.46*** (0.28) |
| lingregFrench |  |  |  | -0.22 (0.28) | -0.09 (0.28) | 0.17 (0.28) |
| lingregItalian |  |  |  | -2.32** (0.71) | -2.19** (0.71) | -1.87** (0.70) |
| degurbaperi-urban |  |  |  | 0.29 (0.29) | 0.39 (0.29) | 0.28 (0.28) |
| degurbarural |  |  |  | 0.52 (0.39) | 0.68 (0.40) | 0.58 (0.39) |
| log_eqinc |  |  |  |  | 0.27 (0.17) | -0.04 (0.16) |
| wealth_fin_tertlow |  |  |  |  | -0.93* (0.36) | -0.32 (0.35) |
| wealth_fin_terthigh |  |  |  |  | 0.38 (0.33) | 0.05 (0.33) |
| wealth_fin_tertmissing |  |  |  |  | 0.12 (0.34) | 0.04 (0.33) |
| endsmeet_fdifficulty |  |  |  |  |  | 3.14** (1.05) |
| endsmeet_ffairly easily |  |  |  |  |  | 4.69*** (1.02) |
| endsmeet_feasily |  |  |  |  |  | 5.98*** (1.02) |
| suddenexp_subj |  |  |  |  |  | 0.04 (0.42) |

*Anmerkung: Zellen: b<Sterne> (SE). Leerzellen = Variable in diesem Modell nicht enthalten.*


## S3 — Interaktion Wohnform × Einkommensarmut
| Block | Term | b (SE) | 95%-KI | p |
| --- | --- | --- | --- | --- |
| M4 + owner×strain | owner | 0.64* (0.27) | [0.11; 1.18] | 0.019 |
| M4 + owner×strain | strain | -0.69 (0.57) | [-1.81; 0.43] | 0.226 |
| M4 + owner×strain | owner:strain | 0.78 (0.70) | [-0.59; 2.14] | 0.264 |
| M5 + owner×strain | owner | 0.33 (0.27) | [-0.20; 0.86] | 0.224 |
| M5 + owner×strain | strain | -0.22 (0.54) | [-1.28; 0.83] | 0.677 |
| M5 + owner×strain | owner:strain | 0.56 (0.66) | [-0.73; 1.85] | 0.395 |

*Anmerkung: Einkommensarmut = objektive Einkommensarmut (Silver-Armutsgrenze). Mit dem M4- bzw. M5-Set.*


## S4 — Innerhalb der Eigentümer: Immobilienbelastung/-vorteil
| Fokus | Term | b (SE) | p | n | Anmerkung |
| --- | --- | --- | --- | --- | --- |
| (a) Hypothek (ja/nein) | has_mortgage | -0.38 (0.39) | 0.332 | 1395 |  |
| (b) Belehnungsgrad (LTV-Kat) | ltv_cat(0,.33] | -0.19 (0.45) | 0.671 | 1393 |  |
| (b) Belehnungsgrad (LTV-Kat) | ltv_cat(.33,.66] | -0.69 (0.45) | 0.123 | 1393 |  |
| (b) Belehnungsgrad (LTV-Kat) | ltv_cat>.66 | -0.11 (0.51) | 0.826 | 1393 |  |
| (c) Hypothekenlast (% Eink.) | mortgage_burden | 2.41 (1.66) | 0.145 | 1393 |  |
| (c2) Last > 15% Eink. | mort_burden_15 | 0.34 (0.55) | 0.536 | 1393 |  |
| (c3) Last > 30% Eink. | mort_burden_30 | 2.25** (0.82) | 0.006 | 1393 | n(Last>30%)=31 ungew. |
| (d) imput. Netto-Miete (log) | log_imprentnet_eq | -0.06 (0.08) | 0.452 | 1439 |  |
| (e) PK-Bezug fürs Eigenheim | pil2_home | -0.45 (0.46) | 0.319 | 1420 |  |
| (f) weitere Immobilie | owner_re | 0.72* (0.34) | 0.035 | 1436 |  |
| (g) gemeinsames Modell | has_mortgage | -0.87 (0.62) | 0.159 | 1480 |  |
| (g) gemeinsames Modell | ltv_cat(0,.33] | 0.45 (0.60) | 0.455 | 1480 |  |
| (g) gemeinsames Modell | ltv_cat(.33,.66] | -0.11 (0.55) | 0.848 | 1480 |  |
| (g) gemeinsames Modell | mortgage_burden | 3.68 (1.96) | 0.060 | 1480 |  |
| (g) gemeinsames Modell | log_imprentnet_eq | -0.04 (0.10) | 0.724 | 1480 |  |
| (g) gemeinsames Modell | pil2_home | -0.46 (0.46) | 0.311 | 1480 |  |
| (g) gemeinsames Modell | owner_re | 0.58 (0.36) | 0.115 | 1480 |  |
| (c-sens) Hypothekenlast @ Zins 2.5% | mb_25 | 1.21 (0.83) | 0.145 | 1393 | Sensitivität: Referenzzins 2.5% statt 1.25% |

*Anmerkung: Gewichtete OLS auf SWLS, Eigentümer 65+, mit dem M4-Kontrollsatz (die genannte Grösse ist der Regressor). Ein Modell je Zeile plus gemeinsames Modell.*


## S5 — PK-Vorbezug fürs Eigenheim
### S5a — PK-Vorbezug und finanzielle Ressourcen
| Kennzahl (Eigentümer 65+) | kein PK-Vorbezug | PK-Vorbezug | p |
| --- | --- | --- | --- |
| Äquiv.-Einkommen Median (CHF) | 4667 | 4400 | 0.326 |
| Finanzvermögen Median (CHF) | 206250 | 2e+05 | 0.005 |
| Nettovermögen Median (CHF) | 810000 | 710000 | 0.299 |
| Hauswert Median (CHF) | 750000 | 750000 | 0.612 |
| Einkommensarm (%) | 8.98 | 9.85 | 0.713 |
| kann unerwartete Ausgabe decken (%) | 95.4 | 96.72 | 0.296 |
| auskommen: leicht (%) | 61.95 | 47.55 | 0.000 |
| noch mit Hypothek (%) | 69.44 | 85.59 | 0.000 |
| Belehnungsgrad (LTV) Median | 0.27 | 0.5 | 0.000 |
| Hypothekenlast % Eink. Median | 3.65 | 6.16 | 0.000 |

*Anmerkung: Gewichtet, Eigentümer 65+. CHF = Mediane, übrige Anteile in %.*

### S5b — PK-Vorbezug und Lebenszufriedenheit, schrittweise um Ressourcen erweitert
| Modell | b (PK-Vorbezug) | SE | p | n |
| --- | --- | --- | --- | --- |
| mA: roh (nur PK) | -0.82 | 0.48 | 0.086 | 1595 |
| mB: + Soziodem./Gesundh./Reg. | -0.67 | 0.45 | 0.132 | 1559 |
| mC: + objektive Finanzen | -0.45 | 0.46 | 0.319 | 1420 |
| mD: + subjektive Lage (=voll) | -0.38 | 0.45 | 0.389 | 1394 |

*Anmerkung: Der Koeffizient attenuiert, wenn die finanzielle Lage kontrolliert wird.*


## S6 — Robustheit
| Spezifikation | Term | b | SE |
| --- | --- | --- | --- |
| MI m=20, M4 owner (PRIMARY wealth block) | owner | 0.75 | 0.26 |
| MI m=20, M5 owner (PRIMARY wealth block) | owner | 0.40 | 0.25 |
| MI m=20, owner×strain (M4) | owner:strain | 0.83 | 0.70 |
| (i) ordered logit SWLS-cat (M4) | owner | 0.35 | 0.10 |
| (iii) unweighted M4 | owner | 0.66 | 0.22 |
| (iv) M4, 65-79 | owner | 0.78 | 0.32 |
| (iv) M4, 80+ | owner | 0.65 | 0.42 |
| (vi) M4 BFS-9 typology | owner | 0.79 | 0.25 |
| (vii) M4 BFS-9(coll)+Bergregion | owner | 0.79 | 0.25 |

*Anmerkung: Eigentümer-Koeffizient (bzw. Interaktion) unter alternativen Spezifikationen; inkl. Altersgruppen 65–79 / 80+ (im Manuskript nicht berichtet).*


---

## Abbildungen
Siehe `figures/` (PNG + PDF): Abbildung 1 (Eigentumsvorteil M0–M5, zwei Eigentümergruppen), 
Abbildung 2 (Wohnform × Einkommensarmut), Abbildung 3 (Belehnungsgrad/Hypothekenlast), 
Abbildung 4 (PK-Vorbezug).

## Code
`code/` enthält `01_setup-data.R`, `02_analysis.R`, `02c_pk-vorbezug.R`, `03_figures_print.R` 
(Mikrodaten nicht enthalten; `SAS_DATA_PATH` auf die `.sav` setzen zum Reproduzieren).

Repository: https://github.com/rainergabriel/wohneigentum-pensionsalter  ·  Generiert mit `analysis/04_build_supplement.R`.

