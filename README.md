# Wohnen und Wohnbefinden — SAS (R-Analysen)

R-Code für die Analyse von Wohnform und Lebenszufriedenheit im Alter (Swiss Aging
Survey 2022, 65+). **Nur die Analyse-Skripte sind hier versioniert** — Daten,
Abbildungen, Tabellen, der Report und die Projektunterlagen liegen lokal und werden
nicht committet (SAS-Mikrodaten sind Eigentum von Pro Senectute).

## Skripte (`analysis/`)
- `01_setup-data.R` — lädt die SAS-Release, verknüpft den altersdata-Silver-Layer
  (Einkommen/Vermögen/Armut) auf `Respondent_ID` und bildet die Analysevariablen
  (Wohnform, finanzielle Lage, Gesundheit, Region/BFS-9, SWLS, …), 65+.
- `02_analysis.R` — gewichtete Analysen (survey): Deskriptiva, geschachtelte
  OLS-Modelle, Interaktion Eigentum × finanzielle Anspannung, Vertiefung
  einkommensarme Eigentümer, Robustheit (inkl. MI).

## Voraussetzungen
R ≥ 4.2; Pakete: haven, dplyr, tidyr, survey, mice, ggplot2. Die SAS-Release-Datei
ausserhalb des Repos ablegen und `SAS_DATA_PATH` setzen; der Silver-Layer wird über
`SAS_SILVER` referenziert. Skripte aus dem `analysis/`-Verzeichnis ausführen.
