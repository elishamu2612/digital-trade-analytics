# Digital Trade Analytics

Interactive [Shiny](https://shiny.posit.co/) dashboard for comparing countries on two digital-trade series:

- **INDIGO** — a normalized digital-trade readiness index (higher means stronger digital-trade capacity). Source file: `indigo.xlsx` (OECD-style country panel, about 2015–2024).
- **Digitally delivered services (DDS)** — exports of services that can be delivered digitally, in US$ million. Source file: `dds.xlsx` (WTO/OECD-style reporter panel, about 2005–2024, including regional aggregate rows).

Switch datasets from the sidebar. India is the default country when it is present in the file.

## What the dashboard shows

| Tab | What it does |
| --- | --- |
| Overview | World-average change, latest average, top economy, and how many economies report. Charts for the global trend, top economies, and the gap between the highest and lowest values. |
| World choropleth | Country map for a chosen year (ISO-3). Darker shades are higher values. |
| Regional trends | Lines for region rows stored in the spreadsheet (World, Africa, Asia, and so on), plus unweighted continent averages for Africa, Americas, Asia, Europe, and Oceania. |
| Trend explorer | Overlay selected countries against the world average. |
| Comparative lens | Distance from the world average, and “catch-up” economies that started in the lower half and gained the most in absolute terms. |
| Quadrant map | Latest level versus total change, both centered on the sample mean: Leaders, Catch-up, Mature/Slowing, Lagging. |
| Country explorer | One economy versus the world average, with rank, peer group (Leader / Strong / Mid-tier / Emerging), growth, and gap to the world average. |
| Data table | Sortable table with copy / CSV / Excel export. |

Regional aggregates such as World, Africa, and the European Union are kept out of country rankings. Continent averages are a simple mean of countries with data that year (small and large economies count equally).

## Run locally

Place `comparative_file.R`, `indigo.xlsx`, and `dds.xlsx` in the same folder, then from that folder:

```r
# install once if needed: shiny, shinydashboard, readxl, dplyr, tidyr,
# ggplot2, forcats, stringr, scales, DT, plotly, countrycode, htmltools
shiny::runApp("comparative_file.R")
```

The script installs any missing packages the first time it runs, then reads both workbooks and opens the dashboard.

## Data notes

- `indigo.xlsx`: column 1 is the country; remaining columns are years. The 2025 column is empty, so the app uses the last year that actually has data.
- `dds.xlsx`: column 1 is the reporter name; remaining columns are years in US$ million. Region rows in the sheet are plotted as official aggregates and are excluded from country ranks.
