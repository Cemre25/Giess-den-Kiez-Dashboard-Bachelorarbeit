# Giess-den-Kiez-Dashboard-Bachelorarbeit

Ein interaktives R Shiny Dashboard zur Analyse der Bewässerungsdaten des Berliner Projekts [Gieß den Kiez](https://www.giessdenkiez.de/). Entwickelt im Rahmen einer Bachelorarbeit an der **Technischen Hochschule Wildau**.

---

## 📊 Features

- **Interaktive Karte** – Visualisierung aller Berliner Straßenbäume und Handpumpen nach Bezirk und LOR-Planungsraum
- **Bewässerungsstatistiken** – Analyse der Gießmengen nach Zeit, Bezirk, Baumart und Baumalter
- **Wetterdaten** – Monatliche Niederschlags- und Temperaturdaten (2020–2024) in Relation zur Bewässerungsaktivität
- **Sozialindex** – Verknüpfung von Bewässerungsverhalten mit dem Berliner Sozialindex (GESIx 2022)

---

## 🛠️ Technologien

| Paket | Zweck |
|---|---|
| `shiny` + `shinydashboard` | Framework & Layout |
| `leaflet` + `leaflet.extras` + `leafgl` | Interaktive Karten |
| `ggplot2` + `plotly` | Diagramme & Visualisierungen |
| `dplyr` + `tidyr` + `data.table` + `purrr` | Datentransformation |
| `sf` | Geodatenverarbeitung |
| `viridis` | Barrierefreie Farbpaletten |
| `shinyBS` | UI-Elemente (Tooltips, Modaldialoge) |
| `lubridate` + `stringr` | Datum & Text-Verarbeitung |

---

## 📁 Projektstruktur

```
├── app.R                  # Führt alle Teile zusammen
├── server.R               # Logik, reaktive Datenpipelines
├── global.R               # Bibliotheken, Konstanten, WFS-Integration 
├── ui.R                   # Benutzeroberfläche & Layout 
├── data/
│   ├── *.csv              # Aufbereitete Datensätze
│   └── *.geojson          # Geodaten (Bezirke, LOR, Pumpen)
└── README.md
```

---

## 🚀 Installation & Ausführen

### Voraussetzungen

- R (≥ 4.0)
- RStudio (empfohlen)

### Pakete installieren

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyBS",
  "leaflet", "leaflet.extras", "leafgl",
  "ggplot2", "plotly",
  "dplyr", "tidyr", "data.table", "purrr",
  "sf", "viridis",
  "lubridate", "stringr", "memoise"
))
```

### App starten

```r
shiny::runApp()
```

---

## 📦 Datenquellen

### 1. 🌿 Gießdaten – Gieß den Kiez (GovData)
Bewässerungsaktionen Berliner Bürger von 2020–2024, inkl. Koordinaten, Baumzuordnung, Zeitstempel und Gießmenge in Litern.
👉 [GovData – Gieß den Kiez Nutzungsdaten](https://www.govdata.de/suche/daten/giess-den-kiez-nutzungsdaten)

### 2. 🌳 Baumbestandsdaten – Berlin Open Data (WFS)
Offizielle Berliner Baumkatasterdaten mit Standort, Baumart, Gattung, Pflanzjahr und Koordinaten.
👉 [Baumbestand Berlin – WFS](https://daten.berlin.de/datensaetze/baumbestand-berlin-wfs-48ad3a23)

### 3. 💧 Öffentliche Wasserpumpen – OpenStreetMap (Overpass API)
Standorte öffentlicher Wasserpumpen in Berlin mit Informationen zu Zugänglichkeit, Zustand und Bedienbarkeit.
👉 [Overpass Turbo](https://overpass-turbo.eu/)

### 4. 🌦️ Wetterdaten – Deutscher Wetterdienst (DWD)
Monatliche Temperatur- und Niederschlagsdaten der Station Berlin-Tempelhof (2020–2024).
👉 [DWD Open Data](https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/monthly/kl/)

### 5. 🗺️ Administrative Raumdaten
- **Bezirksgrenzen** (GeoJSON) – [ODIS Berlin](https://daten.odis-berlin.de/de/dataset/bezirksgrenzen/)
- **LOR-Planungsräume** – [GovData](https://www.govdata.de/suche/daten/lebensweltlich-orientierte-raume-lor-01-01-2019)

### 6. 📊 Gesundheits- und Sozialindex
Sozialindex (GESIx 2022) auf LOR-Ebene zur Untersuchung sozioökonomischer Einflüsse auf das Gießverhalten.
👉 [Berlin Open Data – GESIx 2022](https://daten.berlin.de/datensaetze/gesundheits-und-sozialstrukturatlas-berlin-2022-indexwerte-auf-ebene-der-bezirksregionen-119115)

### 7. 👥 Einwohnerdaten – Statistik Berlin Brandenburg
Gesamtbevölkerungsdaten Berlin 2020–2024 zur Analyse der Bevölkerungsdichte.
👉 [Statistik Berlin Brandenburg](https://www.statistik-berlin-brandenburg.de/a-i-5-hj)
---
## Demo
[![Dashboard Demo](https://img.youtube.com/vi/2cw5hLDTULg/maxresdefault.jpg)](https://youtu.be/2cw5hLDTULg)

---

## 📜 Lizenz

Dieses Projekt basiert teilweise auf [quadriga-dk/Tabelle-Fallstudie-3](https://github.com/quadriga-dk/Tabelle-Fallstudie-3), lizenziert unter [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

Weiterentwicklung und neuer Code: Cemre, 2025

---

## 🎓 Bachelorarbeit

Dieses Dashboard wurde im Rahmen einer Bachelorarbeit an der **Technischen Hochschule Wildau** entwickelt.