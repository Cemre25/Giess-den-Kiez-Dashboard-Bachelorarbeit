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

| Tool | Zweck |
|---|---|
| R + Shiny | Dashboard Framework |
| data.table / dplyr | Datentransformation |
| sf / leaflet | Geodatenverarbeitung & Karte |
| ggplot2 | Visualisierungen |

---

## 📁 Projektstruktur

```
├── app.R                  # Haupt-App (UI + Server)
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
  "shiny", "data.table", "dplyr", "sf",
  "leaflet", "ggplot2", "lubridate", "stringr"
))
```

### App starten

```r
shiny::runApp()
```

---

## 📦 Datenquellen

- **Bewässerungsdaten**: [Gieß den Kiez – Open Data](https://www.giessdenkiez.de/)
- **Berliner Straßenbäume**: [Geoportal Berlin](https://www.berlin.de/sen/uvk/natur-und-gruen/stadtgruen/daten-und-fakten/)
- **Wetterdaten**: Deutscher Wetterdienst (DWD)
- **Sozialindex**: Senatsverwaltung Berlin (GESIx 2022)
- **LOR-Planungsräume**: Amt für Statistik Berlin-Brandenburg

> ⚠️ Die großen CSV-Datensätze sind aufgrund der GitHub-Größenbeschränkung nicht im Repository enthalten. Die Rohdaten können über die oben genannten Quellen bezogen werden.

---
## Demo
[![Dashboard Demo](https://img.youtube.com/vi/2cw5hLDTULg/maxresdefault.jpg)](https://youtu.be/2cw5hLDTULg)

---

## 📜 Lizenz

Dieses Projekt basiert teilweise auf [quadriga-dk/Tabelle-Fallstudie-3](https://github.com/quadriga-dk/Tabelle-Fallstudie-3), lizenziert unter [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

Weiterentwicklung und neuer Code: Cemre Bingöl, 2024–2025

---

## 🎓 Bachelorarbeit

Dieses Dashboard wurde im Rahmen einer Bachelorarbeit an der **Technischen Hochschule Wildau** entwickelt.