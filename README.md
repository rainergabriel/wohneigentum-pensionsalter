# Wohneigentum im Pensionsalter — Code & Online-Supplement

Reproduktions-Code und Online-Supplement zur Studie

> **Sonnen- und Schattenseiten des Eigenheims im Rentenalter: Eigentum, objektive
> Ressourcen und subjektives Wohlbefinden im Alter** (Swiss Aging Survey 2022, 65+).
> Rainer Gabriel & Selina N.N., ZHAW.

Analysiert wird der Zusammenhang von **Wohnform** (Mietende / Eigentum ohne weitere
Immobilie / Eigentum mit weiterer Immobilie) und **Lebenszufriedenheit** (Diener-SWLS)
bei den über 65-Jährigen in der Schweiz — und insbesondere, wie weit ein roher
Eigentumsvorteil auf **objektive Ressourcen** (Einkommen, Vermögen) gegenüber der
**subjektiven** finanziellen Einschätzung zurückgeht.

## Daten (nicht im Repository)

Die Analysen beruhen auf dem **Swiss Aging Survey 2022 (SAS), ReleaseB** (Pro Senectute).
Die **Mikrodaten sind urheberrechtlich geschützt und NICHT Teil dieses Repositories** —
sie sind über **SWISSUbase** bzw. Pro Senectute erhältlich. Zum Reproduzieren die
Release-Datei lokal ablegen und den Pfad setzen:

```r
Sys.setenv(SAS_DATA_PATH = "/pfad/zu/data_sas22_ReleaseB_2025-09-16.sav")
```

Alle abgeleiteten „Silver"-Variablen (äquivalisiertes Einkommen, Netto-/Finanzvermögen,
Immobilienwert, Hypothekarsumme, Armutsgrenze) werden **im Repository selbst** aus der
Rohdatei abgeleitet (`analysis/00_build-silver.R`) — es wird kein externer Data-Warehouse
benötigt.

## Reproduktion

R ≥ 4.2; Pakete: `haven`, `dplyr`, `tidyr`, `survey`, `mice`, `ggplot2`, `knitr`,
`rmarkdown`. Aus dem `analysis/`-Verzeichnis ausführen:

```r
source("01_setup-data.R")       # Rohdaten -> Silver (via 00_build-silver.R) -> data/wohnen_working.rds
source("02_analysis.R")         # Haupttabellen + Figuren (gewichtet; MI mice m=20)
source("02c_pk-vorbezug.R")     # PK-Vorbezug: Ressourcen & Zufriedenheit
source("03_figures_print.R")    # Manuskript-Abbildungen 1-4 (PNG + PDF)
source("04_build_supplement.R") # baut ../supplement/ (Tabellen S1-S6 + Figuren) neu
# rmarkdown::render("report.Rmd")   # Kurz-Report "Hauptergebnisse" (PDF)
```

## Struktur

| Pfad | Inhalt |
|---|---|
| `analysis/00_build-silver.R` | Leitet die Silver-Variablen (Einkommen/Vermögen/Armut, Hauswert, Hypothek) direkt aus der SAS-Rohdatei ab — macht das Projekt ohne Data-Warehouse 100 % reproduzierbar. |
| `analysis/01_setup-data.R` | Baut die Analysevariablen (Wohnform, finanzielle Lage, Gesundheit, Region/BFS-9, SWLS), 65+. |
| `analysis/02_analysis.R` | Deskription, geschachtelte gewichtete OLS M0–M5 (objektiver Block M4, subjektiver Block M5), Interaktion, Analysen innerhalb der Eigentümer, Robustheit (inkl. MI m=20). |
| `analysis/02c_pk-vorbezug.R` | Pensionskassen-Vorbezug fürs Eigenheim: Zusammenhang mit finanziellen Ressourcen und Zufriedenheit. |
| `analysis/03_figures_print.R` | Manuskript-Abbildungen 1–4 (PNG + PDF). |
| `analysis/04_build_supplement.R` | Erzeugt das Online-Supplement (`supplement/`) aus den Ergebnis-Tabellen. |
| `analysis/report.Rmd` | Kurz-Report der Hauptergebnisse (PDF). |
| **`supplement/`** | **Online-Supplement** (ersetzt den Anhang): `README.md` mit allen Tabellen **S1–S6** (Markdown), `figures/` (PNG + PDF), `code/`. |

## Methode (kurz)

Gewichtet (Designgewicht, kantonale Schichtung); 65+ (n = 3'313; gemeinsame
Complete-Case-Stichprobe der genesteten Modelle n = 2'502). Die finanzielle Lage wird
getrennt in einen **objektiven** Block (äquivalisiertes Einkommen + Finanzvermögens-Terzile
mit „fehlend"-Kategorie; M4) und einen **subjektiven** Block (mit dem Einkommen auskommen,
unerwartete Ausgabe decken; M5) — Letzterer erklärt, klassiert aber nie (Gabriel & Hümbelin
2026). Owner-spezifische Grössen (Hypothek, Belehnungsgrad, PK-Vorbezug, imputierte Miete)
werden separat innerhalb der Eigentümer untersucht. Vermögens-Item-Nonresponse wird per
multipler Imputation (mice, m = 20) adressiert.

**→ Alle Ergebnistabellen: [`supplement/README.md`](supplement/README.md).**

## Zitation

> Gabriel, R., & N.N., S. (in Vorbereitung). *Sonnen- und Schattenseiten des Eigenheims
> im Rentenalter.* ZHAW. Code & Supplement: dieses Repository.

## Lizenz

Code: MIT (sofern nicht anders vermerkt). Die SAS-Mikrodaten unterliegen den
Nutzungsbedingungen von Pro Senectute / SWISSUbase und sind hier nicht enthalten.
