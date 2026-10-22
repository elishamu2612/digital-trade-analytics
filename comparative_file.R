# ============================================================
# DIGITAL TRADE ANALYTICS — INDIGO Index & Digitally delivered services
# Required files (same folder as this script): indigo.xlsx, dds.xlsx
# Run: setwd() to this folder, then source or shiny::runApp
# ============================================================

# ===========================
# 0. PACKAGES
# ===========================
required_packages <- c(
  "shiny",
  "shinydashboard",
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "forcats",
  "stringr",
  "scales",
  "DT",
  "plotly",
  "countrycode"
)

installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!(pkg %in% installed)) install.packages(pkg, dependencies = TRUE)
}

library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(stringr)
library(scales)
library(DT)
library(plotly)
library(countrycode)
library(htmltools)

# ===========================
# 1. PATHS
# ===========================
path_indigo <- "indigo.xlsx"
path_dds <- "dds.xlsx"

# ===========================
# 2. CONSTANTS & HELPERS
# ===========================
# Rows that are regions / aggregates in trade spreadsheets (not mappable countries)
REGION_AGGREGATE_NAMES <- c(
  "World",
  "Africa",
  "Asia",
  "Europe",
  "North America",
  "Middle East",
  "South and Central America and the Caribbean",
  "European Union",
  "European Union (evolving composition)",
  "Commonwealth of Independent States (CIS), including certain associate and former member States"
)

ISO3_CUSTOM <- c(
  "Korea, Republic of" = "KOR",
  "Hong Kong, China" = "HKG",
  "Macao, China" = "MAC",
  "Chinese Taipei" = "TWN",
  "Viet Nam" = "VNM",
  "United States of America" = "USA",
  "Russian Federation" = "RUS",
  "Türkiye" = "TUR",
  "China (People's Republic of)" = "CHN",
  "Korea" = "KOR",
  "Hong Kong (China)" = "HKG",
  "Macau (China)" = "MAC",
  "Russia" = "RUS",
  "United States" = "USA",
  "The Gambia" = "GMB",
  "Democratic Republic of the Congo" = "COD",
  "Congo" = "COG",
  "Côte d'Ivoire" = "CIV",
  "Bolivia, Plurinational State of" = "BOL",
  "Venezuela, Bolivarian Republic of" = "VEN",
  "Moldova, Republic of" = "MDA",
  "Sao Tomé and Principe" = "STP",
  "Sao Tome and Principe" = "STP",
  "Lebanese Republic" = "LBN",
  "Bahrain, Kingdom of" = "BHR",
  "Kuwait, the State of" = "KWT",
  "Saudi Arabia, Kingdom of" = "SAU",
  "Sint Maarten" = "SXM",
  "Curaçao" = "CUW",
  "Faeroe Islands" = "FRO",
  "Kyrgyz Republic" = "KGZ",
  "Lao People's Democratic Republic" = "LAO",
  "Iran" = "IRN",
  "Tanzania" = "TZA",
  "Palestinian Authority or West Bank and Gaza Strip" = "PSE",
  "UNMIK/Kosovo" = "XKX"
)

clean_country_names <- function(x) {
  x |>
    str_replace_all("&", "and") |>
    str_squish()
}

# Short axis labels for coord_flip bar charts (full name kept in tooltips)
country_axis_label <- function(x, width = 22) {
  str_wrap(x, width = width)
}

fmt_index <- function(x) {
  number(x, accuracy = 0.001)
}

fmt_trade_musd <- function(x) {
  number(x, accuracy = 1, big.mark = ",")
}

theme_dash <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(
        face = "bold", size = base_size + 3, color = "grey10",
        hjust = 0.5
      ),
      plot.subtitle = element_text(size = base_size, color = "grey35", hjust = 0.5),
      axis.title = element_text(face = "bold", color = "grey15"),
      axis.text = element_text(color = "grey20"),
      axis.text.y = element_text(color = "grey20", lineheight = 0.88, size = rel(0.92)),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey86", linewidth = 0.35),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.margin = margin(8, 10, 8, 10)
    )
}

# Plotly ignores ggplot title hjust; center explicitly + leave room for wrapped titles
ggplotly_centered <- function(p) {
  g <- suppressWarnings(ggplotly(p, tooltip = "text"))
  plotly::layout(
    g,
    title = list(x = 0.5, xanchor = "center"),
    margin = list(t = 100, b = 52, l = 94, r = 32)
  )
}

col_main <- "#1e3a5f"
col_accent <- "#c2410c"
col_good <- "#0d9488"
col_warn <- "#b91c1c"
col_soft <- "#64748b"
pal_lines <- c("#1e3a5f", "#c2410c", "#0d9488", "#b91c1c", "#6366f1", "#ea580c", "#0e7490", "#ca8a04")

add_iso3 <- function(countries) {
  countrycode(
    countries,
    origin = "country.name",
    destination = "iso3c",
    warn = FALSE,
    custom_match = ISO3_CUSTOM
  )
}

is_aggregate_row <- function(name) {
  name %in% REGION_AGGREGATE_NAMES |
    str_detect(name, regex("^Commonwealth of Independent States", ignore_case = TRUE)) |
    str_detect(name, regex("^European Union", ignore_case = TRUE))
}

# ===========================
# 3. BUILD ONE DATASET (INDIGO or DDS)
# ===========================
prepare_dashboard_data <- function(path, dataset_id) {
  df_full <- read_excel(path)
  names(df_full)[1] <- "Country"
  df_full$Country <- clean_country_names(df_full$Country)

  year_cols <- setdiff(names(df_full), "Country")
  df_full[year_cols] <- lapply(df_full[year_cols], as.numeric)

  valid_years <- year_cols[colSums(!is.na(df_full[year_cols])) > 0]
  df_full <- df_full |> select(Country, all_of(valid_years))

  # Country-level rows only for rankings / world averages (exclude World, regions, etc.)
  df <- df_full |> filter(!is_aggregate_row(Country))

  df_long_full <- df_full |>
    pivot_longer(cols = -Country, names_to = "Year", values_to = "Index") |>
    mutate(Year = as.integer(Year), Index = as.numeric(Index))

  df_long <- df_long_full |> filter(!is_aggregate_row(Country))

  years <- sort(unique(df_long$Year[!is.na(df_long$Index)]))
  start_year <- min(years)
  end_year <- max(years)

  if (mean(is.na(df[[as.character(end_year)]])) > 0.8 && length(years) > 1) {
    end_year <- years[length(years) - 1]
  }
  years <- years[years <= end_year]

  global_trend <- df_long |>
    filter(Year <= end_year) |>
    group_by(Year) |>
    summarise(
      mean_index = mean(Index, na.rm = TRUE),
      median_index = median(Index, na.rm = TRUE),
      max_index = max(Index, na.rm = TRUE),
      min_index = min(Index, na.rm = TRUE),
      gap_max_min = max_index - min_index,
      n_countries = sum(!is.na(Index)),
      .groups = "drop"
    )

  first_value <- global_trend |> filter(Year == start_year) |> pull(mean_index)
  last_value <- global_trend |> filter(Year == end_year) |> pull(mean_index)
  global_growth_pct <- ((last_value - first_value) / first_value) * 100

  latest_data <- df_long |>
    filter(Year == end_year, !is.na(Index)) |>
    arrange(desc(Index)) |>
    mutate(rank_latest = row_number())

  growth <- df |>
    select(Country, all_of(as.character(start_year)), all_of(as.character(end_year))) |>
    rename(
      start_value = all_of(as.character(start_year)),
      end_value = all_of(as.character(end_year))
    ) |>
    mutate(
      absolute_change = end_value - start_value,
      pct_growth = ifelse(
        !is.na(start_value) & start_value > 0,
        ((end_value - start_value) / start_value) * 100,
        NA_real_
      )
    )

  relative_position <- df_long |>
    filter(Year <= end_year) |>
    group_by(Year) |>
    mutate(
      world_avg = mean(Index, na.rm = TRUE),
      gap_to_world = Index - world_avg,
      ratio_to_world = Index / world_avg
    ) |>
    ungroup()

  cthr <- if (identical(dataset_id, "dds")) 1 else 0.01
  catchup_table <- growth |>
    filter(!is.na(start_value), !is.na(end_value), start_value > cthr) |>
    mutate(start_half = ntile(start_value, 2)) |>
    filter(start_half == 1) |>
    arrange(desc(absolute_change))

  dashboard_table <- latest_data |>
    left_join(growth, by = "Country") |>
    left_join(
      relative_position |>
        filter(Year == end_year) |>
        select(Country, gap_to_world, ratio_to_world),
      by = "Country"
    ) |>
    mutate(
      comparative_group = case_when(
        Index >= quantile(Index, 0.75, na.rm = TRUE) ~ "Leader",
        Index >= quantile(Index, 0.50, na.rm = TRUE) ~ "Strong",
        Index >= quantile(Index, 0.25, na.rm = TRUE) ~ "Mid-tier",
        TRUE ~ "Emerging"
      )
    )

  quadrant_data <- dashboard_table |>
    filter(!is.na(Index), !is.na(absolute_change)) |>
    mutate(
      level_centered = Index - mean(Index, na.rm = TRUE),
      growth_centered = absolute_change - mean(absolute_change, na.rm = TRUE),
      quadrant = case_when(
        level_centered >= 0 & growth_centered >= 0 ~ "Leaders",
        level_centered < 0 & growth_centered >= 0 ~ "Catch-up",
        level_centered >= 0 & growth_centered < 0 ~ "Mature/Slowing",
        TRUE ~ "Lagging"
      )
    )

  # --- Regional series from spreadsheet (WTO-style regions when present)
  region_long <- df_long_full |> filter(is_aggregate_row(Country))

  # --- UN macro-region averages (country rows only)
  country_only <- df |>
    filter(!is_aggregate_row(Country)) |>
    pull(Country) |>
    unique()
  # Continent from ISO3 is more reliable than country-name → continent (avoids bogus "regions" in legends)
  df_country <- df_long |>
    filter(Country %in% country_only) |>
    mutate(
      iso3 = add_iso3(Country),
      macro_region = countrycode(
        iso3, "iso3c", "continent",
        warn = FALSE,
        custom_match = ISO3_CUSTOM
      )
    ) |>
    filter(!is.na(iso3))

  un_region_trend <- df_country |>
    filter(Year <= end_year, !is.na(macro_region), !is.na(Index)) |>
    group_by(Year, macro_region) |>
    summarise(mean_index = mean(Index, na.rm = TRUE), n = n(), .groups = "drop")

  list(
    df = df,
    df_full = df_full,
    df_long = df_long,
    global_trend = global_trend,
    first_value = first_value,
    last_value = last_value,
    global_growth_pct = global_growth_pct,
    latest_data = latest_data,
    growth = growth,
    relative_position = relative_position,
    catchup_table = catchup_table,
    dashboard_table = dashboard_table,
    quadrant_data = quadrant_data,
    region_long = region_long,
    un_region_trend = un_region_trend,
    years = years,
    start_year = start_year,
    end_year = end_year,
    dataset_id = dataset_id
  )
}

# ===========================
# 4. LOAD BOTH DATASETS
# ===========================
if (!file.exists(path_indigo)) stop("Missing file: ", path_indigo, " — place it next to this script.")
if (!file.exists(path_dds)) stop("Missing file: ", path_dds, " — place it next to this script.")

prep_indigo <- prepare_dashboard_data(path_indigo, "indigo")
prep_dds <- prepare_dashboard_data(path_dds, "dds")

meta <- list(
  indigo = list(
    title = "INDIGO — Digital trade index",
    subtitle = "Normalized index of digital trade readiness.\nHigher = stronger digital trade capacity.",
    fmt = fmt_index,
    ylab_global = "Average index",
    ylab_country = "Index",
    map_title = "INDIGO index by country"
  ),
  dds = list(
    title = "Digitally delivered services — exports",
    subtitle = "Digitally delivered services exports (US$ million).\nWTO/OECD-style series — compare levels and growth.",
    fmt = fmt_trade_musd,
    ylab_global = "Average exports (US$ m)",
    ylab_country = "Exports (US$ m)",
    map_title = "Digitally delivered services exports (latest year)"
  )
)

# Default sidebar choices (INDIGO) — updated when switching dataset
ch_init <- sort(unique(prep_indigo$df_long$Country))
sel_init <- if ("India" %in% ch_init) "India" else ch_init[1]
top5_init <- prep_indigo$latest_data$Country[seq_len(min(5, nrow(prep_indigo$latest_data)))]
trend_init <- unique(c(if ("India" %in% ch_init) "India", top5_init))
trend_init <- intersect(trend_init, ch_init)
if (length(trend_init) < 6) {
  trend_init <- c(trend_init, head(setdiff(ch_init, trend_init), 6 - length(trend_init)))
}

# ===========================
# 5. UI
# ===========================
ui <- dashboardPage(
  skin = "blue",
  title = "Digital Trade Analytics",
  dashboardHeader(
    title = span(
      icon("chart-area"),
      " Digital Trade Analytics "
    ),
    titleWidth = 278
  ),

  dashboardSidebar(
    width = 278,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("World choropleth", tabName = "map", icon = icon("globe-americas")),
      menuItem("Regional trends", tabName = "regional", icon = icon("layer-group")),
      menuItem("Trend explorer", tabName = "trend", icon = icon("chart-line")),
      menuItem("Comparative lens", tabName = "comparative", icon = icon("balance-scale")),
      menuItem("Quadrant map", tabName = "quadrant", icon = icon("braille")),
      menuItem("Country explorer", tabName = "country", icon = icon("search")),
      menuItem("Data table", tabName = "table", icon = icon("table")),
      hr(),
      selectInput(
        "dataset",
        tags$span(style = "font-weight:600;", "Active dataset"),
        choices = c(
          "INDIGO index" = "indigo",
          "Digitally delivered services" = "dds"
        ),
        selected = "indigo"
      ),
      uiOutput("dataset_blurb"),
      hr(),
      selectInput("selected_country", "Country (explorer)", choices = ch_init, selected = sel_init),
      sliderInput("top_n", "Top N (bar charts)", min = 5, max = 25, value = 12, step = 1),
      sliderInput(
        "map_year",
        "Map year (choropleth)",
        min = min(prep_indigo$years),
        max = max(prep_indigo$years),
        value = prep_indigo$end_year,
        step = 1,
        sep = ""
      ),
      selectInput(
        "trend_countries",
        "Trend explorer — compare countries",
        choices = ch_init,
        selected = trend_init,
        multiple = TRUE,
        selectize = TRUE
      )
    )
  ),

  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=DM+Sans:ital,opsz,wght@0,9..40,400;0,9..40,600;0,9..40,700;1,9..40,400&display=swap"),
      tags$style(HTML("
        body, .main-sidebar { font-family: 'DM Sans', 'Segoe UI', system-ui, sans-serif; }
        body { background: #f8fafc; color: #334155; }
        .content-wrapper, .right-side { background: #f1f5f9 !important; }
        .skin-blue .main-header .logo {
          background: #ffffff !important;
          color: #0f172a !important;
          font-weight: 700;
          letter-spacing: 0.02em;
          border-bottom: 1px solid #e2e8f0 !important;
        }
        .skin-blue .main-header .navbar {
          background: #ffffff !important;
          border-bottom: 1px solid #e2e8f0 !important;
        }
        .skin-blue .main-header .navbar-custom-menu .navbar-nav > li > a {
          color: #475569 !important;
        }
        .skin-blue .main-sidebar {
          background: #f8fafc !important;
          border-right: 1px solid #e2e8f0 !important;
          overflow-x: hidden !important;
          box-shadow: 2px 0 12px rgba(15, 23, 42, 0.04);
        }
        .main-sidebar .sidebar-menu { padding-top: 8px; padding-bottom: 16px; }
        .sidebar-menu > li > a {
          border-radius: 10px;
          margin: 3px 10px;
          color: #475569 !important;
          font-size: 13px;
        }
        .sidebar-menu > li > a:hover { background-color: #e2e8f0 !important; color: #0f172a !important; }
        .sidebar-menu > li.active > a {
          background: linear-gradient(90deg, #e0f2fe 0%, #f0f9ff 100%) !important;
          border-left: 3px solid #0ea5e9;
          color: #0c4a6e !important;
          font-weight: 600;
        }
        .main-sidebar .form-group, .main-sidebar .shiny-input-container {
          max-width: 100%;
          padding-left: 2px;
          padding-right: 2px;
          box-sizing: border-box;
        }
        .main-sidebar .selectize-input, .main-sidebar select {
          max-width: 100%;
          box-sizing: border-box;
          background: #fff !important;
          border: 1px solid #cbd5e1 !important;
          border-radius: 8px;
        }
        .main-sidebar label { color: #475569 !important; font-weight: 600; font-size: 12px; }
        .main-sidebar .shiny-html-output {
          width: 100% !important;
          max-width: 100% !important;
          box-sizing: border-box;
          min-width: 0;
        }
        .sidebar-dataset-blurb {
          display: block;
          width: 100%;
          max-width: 100%;
          box-sizing: border-box;
          padding: 4px 10px 12px 10px;
          margin: 0;
          overflow: visible;
        }
        .sidebar-dataset-blurb p {
          margin: 0;
          font-size: 11px;
          line-height: 1.45;
          color: #64748b;
          overflow-wrap: anywhere;
          word-wrap: break-word;
          word-break: normal;
          hyphens: manual;
          max-width: 100%;
          white-space: normal;
        }
        .box {
          border-radius: 14px;
          border: 1px solid #e2e8f0;
          box-shadow: 0 2px 16px rgba(15, 23, 42, 0.05);
          overflow: hidden;
          background: #fff;
        }
        .box .box-body { background: #fff; }
        .box.box-solid.box-primary > .box-header {
          background: linear-gradient(90deg, #f8fafc 0%, #f1f5f9 100%) !important;
          border-radius: 13px 13px 0 0;
          border-bottom: 1px solid #e2e8f0 !important;
          color: #0f172a !important;
        }
        .box.box-solid.box-info > .box-header {
          background: linear-gradient(90deg, #eff6ff 0%, #dbeafe 100%) !important;
          color: #1e3a8a !important;
          border-bottom: 1px solid #bfdbfe !important;
        }
        .box.box-solid.box-success > .box-header {
          background: linear-gradient(90deg, #ecfdf5 0%, #d1fae5 100%) !important;
          color: #065f46 !important;
          border-bottom: 1px solid #a7f3d0 !important;
        }
        .box.box-solid.box-warning > .box-header {
          background: linear-gradient(90deg, #fffbeb 0%, #fef3c7 100%) !important;
          color: #92400e !important;
          border-bottom: 1px solid #fde68a !important;
        }
        .nav-tabs-custom > .nav-tabs > li.active { border-top-color: #0ea5e9 !important; }
        .nav-tabs-custom > .nav-tabs > li > a {
          color: #64748b !important;
          border-radius: 8px 8px 0 0;
        }
        .nav-tabs-custom > .nav-tabs > li.active > a {
          color: #0c4a6e !important;
          font-weight: 600;
        }
        .nav-tabs-custom { border-radius: 14px; border: 1px solid #e2e8f0; box-shadow: 0 2px 16px rgba(15,23,42,0.05); }
        .nav-tabs-custom > .tab-content { background: #fff; padding: 16px; border-radius: 0 0 12px 12px; }
        .small-box {
          border-radius: 14px;
          box-shadow: 0 2px 12px rgba(15, 23, 42, 0.06);
        }
        .insight-callout {
          background: #f8fafc;
          border-left: 4px solid #0ea5e9;
          padding: 12px 16px;
          border-radius: 10px;
          margin-bottom: 12px;
          color: #334155;
        }
        .insight-callout h4 { margin-top: 0; color: #0c4a6e; font-size: 15px; font-weight: 700; }
        .continent-insight { margin-top: 8px; }
      "))
    ),

    tabItems(
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("vb_growth", width = 3),
          valueBoxOutput("vb_avg", width = 3),
          valueBoxOutput("vb_top", width = 3),
          valueBoxOutput("vb_n", width = 3)
        ),
        fluidRow(
          box(
            width = 8,
            title = "Global trend",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("plot_global", height = 380)
          ),
          box(
            width = 4,
            title = "Insights",
            status = "primary",
            solidHeader = TRUE,
            uiOutput("overview_insights")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Top economies",
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("plot_top", height = 380)
          ),
          box(
            width = 6,
            title = "Dispersion (max − min)",
            status = "warning",
            solidHeader = TRUE,
            plotlyOutput("plot_divide", height = 380)
          )
        )
      ),

      tabItem(
        tabName = "map",
        fluidRow(
          box(
            width = 12,
            title = "World choropleth",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("plot_choropleth", height = 520),
            uiOutput("map_insights")
          )
        )
      ),

      tabItem(
        tabName = "regional",
        fluidRow(
          box(
            width = 6,
            title = "Regional aggregates (from source table)",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("plot_region_source", height = 400),
            uiOutput("region_source_note")
          ),
          box(
            width = 6,
            title = "Continent series (how to read)",
            status = "info",
            solidHeader = TRUE,
            uiOutput("region_continent_intro")
          )
        ),
        fluidRow(
          tabBox(
            id = "continent_subtabs",
            width = 12,
            tabPanel(
              title = "Africa",
              plotlyOutput("plot_ct_africa", height = 380),
              uiOutput("insight_ct_africa")
            ),
            tabPanel(
              title = "Americas",
              plotlyOutput("plot_ct_americas", height = 380),
              uiOutput("insight_ct_americas")
            ),
            tabPanel(
              title = "Asia",
              plotlyOutput("plot_ct_asia", height = 380),
              uiOutput("insight_ct_asia")
            ),
            tabPanel(
              title = "Europe",
              plotlyOutput("plot_ct_europe", height = 380),
              uiOutput("insight_ct_europe")
            ),
            tabPanel(
              title = "Oceania",
              plotlyOutput("plot_ct_oceania", height = 380),
              uiOutput("insight_ct_oceania")
            )
          )
        )
      ),

      tabItem(
        tabName = "trend",
        fluidRow(
          box(
            width = 12,
            title = "Multi-country trend comparison",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("plot_trend_compare", height = 450),
            uiOutput("trend_insights")
          )
        )
      ),

      tabItem(
        tabName = "comparative",
        fluidRow(
          box(
            width = 6,
            title = "Above / below world average",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("plot_relative", height = 380)
          ),
          box(
            width = 6,
            title = "Catch-up (started lower, gained most)",
            status = "success",
            solidHeader = TRUE,
            plotlyOutput("plot_catchup", height = 380)
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "How to read this tab",
            status = "success",
            solidHeader = TRUE,
            uiOutput("comparative_note")
          )
        )
      ),

      tabItem(
        tabName = "quadrant",
        fluidRow(
          box(
            width = 12,
            title = "Level vs growth (centered)",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("plot_quadrant", height = 540),
            uiOutput("quadrant_insight")
          )
        )
      ),

      tabItem(
        tabName = "country",
        fluidRow(
          valueBoxOutput("vb_country_index", width = 4),
          valueBoxOutput("vb_country_gap", width = 4),
          valueBoxOutput("vb_country_growth", width = 4)
        ),
        fluidRow(
          box(
            width = 8,
            title = "Country vs world average",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("plot_country", height = 400)
          ),
          box(
            width = 4,
            title = "Profile",
            status = "primary",
            solidHeader = TRUE,
            tableOutput("country_profile")
          )
        )
      ),

      tabItem(
        tabName = "table",
        fluidRow(
          box(
            width = 12,
            title = "Dashboard table",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("table_dashboard")
          )
        )
      )
    )
  )
)

# ===========================
# 6. SERVER
# ===========================
server <- function(input, output, session) {
  d <- reactive({
    if (is.null(input$dataset)) return(prep_indigo)
    if (input$dataset == "dds") prep_dds else prep_indigo
  })

  m <- reactive({
    meta[[d()$dataset_id]]
  })

  observeEvent(input$dataset, {
    pr <- d()
    ch <- sort(unique(pr$df_long$Country))
    sel <- if ("India" %in% ch) "India" else ch[1]
    updateSelectInput(session, "selected_country", choices = ch, selected = sel)

    yrs <- pr$years
    updateSliderInput(
      session,
      "map_year",
      min = min(yrs),
      max = max(yrs),
      value = pr$end_year
    )

    # Default trend comparison: top 5 + India if present
    top5 <- pr$latest_data$Country[seq_len(min(5, nrow(pr$latest_data)))]
    tc <- unique(c(if ("India" %in% ch) "India", top5))
    tc <- intersect(tc, ch)
    updateSelectInput(session, "trend_countries", choices = ch, selected = tc[seq_len(min(6, length(tc)))])
  }, ignoreInit = TRUE)

  output$dataset_blurb <- renderUI({
    sub <- m()$subtitle
    lines <- strsplit(sub, "\n", fixed = TRUE)[[1]]
    html <- paste(htmlEscape(lines), collapse = "<br/>")
    tags$div(
      class = "sidebar-dataset-blurb",
      tags$p(HTML(html))
    )
  })

  output$vb_growth <- renderValueBox({
    pr <- d()
    fmt <- m()$fmt
    valueBox(
      value = paste0(round(pr$global_growth_pct, 1), "%"),
      subtitle = paste("Change in world average,", pr$start_year, "→", pr$end_year),
      icon = icon("chart-line"),
      color = "aqua"
    )
  })

  output$vb_avg <- renderValueBox({
    pr <- d()
    fmt <- m()$fmt
    valueBox(
      value = fmt(pr$last_value),
      subtitle = paste("World average,", pr$end_year),
      icon = icon("globe"),
      color = "green"
    )
  })

  output$vb_top <- renderValueBox({
    pr <- d()
    top_country <- pr$latest_data |> slice(1)
    valueBox(
      value = top_country$Country,
      subtitle = paste("Highest in", pr$end_year),
      icon = icon("trophy"),
      color = "yellow"
    )
  })

  output$vb_n <- renderValueBox({
    pr <- d()
    valueBox(
      value = nrow(pr$latest_data),
      subtitle = "Economies in latest year",
      icon = icon("flag"),
      color = "purple"
    )
  })

  output$overview_insights <- renderUI({
    pr <- d()
    fmt <- m()$fmt
    div(
      class = "insight-callout",
      tags$h4("Interpretation"),
      tags$p(
        "The world average moves from ",
        tags$b(fmt(pr$first_value)), " (", pr$start_year, ") to ",
        tags$b(fmt(pr$last_value)), " (", pr$end_year, "). ",
        if (pr$global_growth_pct > 0) {
          "On average, values increased over the sample — consistent with expanding digital trade and digitally delivered services."
        } else {
          "The average declined over the sample — worth checking crisis years, data revisions, or compositional effects."
        }
      ),
      tags$p(
        "The dispersion chart (max − min) shows whether gaps between leading and lagging economies are widening or narrowing. ",
        "Rising dispersion often signals uneven diffusion of digital capability or scale economies in services exports."
      )
    )
  })

  output$plot_global <- renderPlotly({
    pr <- d()
    fmt <- m()$fmt
    ylab <- m()$ylab_global
    p <- ggplot(
      pr$global_trend,
      aes(
        x = Year,
        y = mean_index,
        text = paste0("Year: ", Year, "<br>Average: ", fmt(mean_index))
      )
    ) +
      geom_line(linewidth = 1.2, color = col_main) +
      geom_point(size = 3, color = col_main) +
      geom_smooth(
        method = "lm", se = FALSE, linetype = "dashed",
        linewidth = 0.8, color = col_accent
      ) +
      scale_x_continuous(breaks = pr$years) +
      scale_y_continuous(labels = ~ fmt(.)) +
      labs(
        title = "World average over time",
        subtitle = m()$title,
        x = NULL,
        y = ylab
      ) +
      theme_dash()
    ggplotly_centered(p)
  })

  output$plot_top <- renderPlotly({
    pr <- d()
    fmt <- m()$fmt
    p <- pr$latest_data |>
      slice(1:input$top_n) |>
      mutate(Country = fct_reorder(Country, Index)) |>
      mutate(Country_lbl = country_axis_label(as.character(Country))) |>
      ggplot(
        aes(
          x = Country_lbl,
          y = Index,
          text = paste0("Economy: ", Country, "<br>Value: ", fmt(Index))
        )
      ) +
      geom_col(fill = col_main, width = 0.72) +
      coord_flip() +
      scale_y_continuous(labels = ~ fmt(.), expand = expansion(mult = c(0, 0.12))) +
      labs(
        title = paste("Top", input$top_n, "—", pr$end_year),
        x = NULL,
        y = m()$ylab_country
      ) +
      theme_dash()
    ggplotly_centered(p)
  })

  output$plot_divide <- renderPlotly({
    pr <- d()
    fmt <- m()$fmt
    p <- ggplot(
      pr$global_trend,
      aes(
        x = Year,
        y = gap_max_min,
        text = paste0("Year: ", Year, "<br>Range: ", fmt(gap_max_min))
      )
    ) +
      geom_line(linewidth = 1.2, color = col_warn) +
      geom_point(size = 3, color = col_warn) +
      scale_x_continuous(breaks = pr$years) +
      scale_y_continuous(labels = ~ fmt(.)) +
      labs(
        title = "Gap between highest and lowest",
        subtitle = "Scale of cross-country inequality in the chosen series",
        x = NULL,
        y = "Max − min"
      ) +
      theme_dash()
    ggplotly_centered(p)
  })

  # --- Choropleth for selected year
  map_df <- reactive({
    pr <- d()
    req(input$map_year)
    y <- as.integer(input$map_year)
    iso3 <- add_iso3(pr$df$Country)
    country_map <- pr$df$Country
    val_col <- pr$df[[as.character(y)]]
    tibble(Country = country_map, iso3 = iso3, value = val_col) |>
      filter(!is_aggregate_row(Country), !is.na(iso3), !is.na(value))
  })

  output$plot_choropleth <- renderPlotly({
    md <- map_df()
    fmt <- m()$fmt
    y <- as.integer(input$map_year)
    md$hover <- paste0(md$Country, "<br>", y, ": ", fmt(md$value))
    plot_ly(
      data = md,
      type = "choropleth",
      locations = ~iso3,
      locationmode = "ISO-3",
      z = ~value,
      colorscale = list(c(0, "#f1f5f9"), c(0.5, "#38bdf8"), c(1, "#1e3a5f")),
      colorbar = list(title = ""),
      text = ~hover,
      hoverinfo = "text"
    ) |>
      layout(
        title = list(text = paste0(m()$map_title, " (", y, ")")),
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          projection = list(type = "natural earth"),
          bgcolor = "rgba(0,0,0,0)"
        ),
        margin = list(l = 0, r = 0, t = 50, b = 0)
      )
  })

  output$map_insights <- renderUI({
    md <- map_df()
    fmt <- m()$fmt
    y <- input$map_year
    if (nrow(md) < 5) {
      return(tags$p(class = "insight-callout", "Not enough mapped countries for this year — try another year."))
    }
    hi <- md |> filter(value == max(value, na.rm = TRUE)) |> slice(1)
    lo <- md |> filter(value == min(value, na.rm = TRUE)) |> slice(1)
    div(
      class = "insight-callout",
      tags$h4("Map reading guide"),
      tags$p(
        "Darker shades indicate higher values in ", tags$b(y), ". ",
        "Roughly: highest mapped economy is ", tags$b(hi$Country), " (", fmt(hi$value), "); ",
        "lowest is ", tags$b(lo$Country), " (", fmt(lo$value), "). ",
        "Small economies and offshore centers can rank high in digitally delivered services due to scale and specialization."
      )
    )
  })

  output$plot_region_source <- renderPlotly({
    pr <- d()
    fmt <- m()$fmt
    rl <- pr$region_long
    if (nrow(rl) == 0) {
      return(
        plotly_empty() |>
          layout(title = "No regional aggregate rows in this file", annotations = list(
            text = "See continent tabs below →",
            xref = "paper", yref = "paper", x = 0.5, y = 0.5, showarrow = FALSE
          ))
      )
    }
    p <- ggplot(
      rl,
      aes(x = Year, y = Index, color = Country, group = Country,
          text = paste0(Country, " — ", Year, ": ", fmt(Index)))
    ) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.8) +
      scale_x_continuous(breaks = pr$years) +
      scale_y_continuous(labels = ~ fmt(.)) +
      scale_color_manual(values = rep(pal_lines, length.out = length(unique(rl$Country)))) +
      labs(
        title = "Regional series",
        x = NULL,
        y = m()$ylab_country
      ) +
      theme_dash()
    ggplotly_centered(p)
  })

  output$region_source_note <- renderUI({
    tags$p(
      style = "font-size:13px; color:#444;",
      "These lines use region rows embedded in the spreadsheet (e.g. World, Africa, Asia). ",
      "They are not recomputed from country data — use them as official aggregates when present."
    )
  })

  output$region_continent_intro <- renderUI({
    tags$div(
      tags$p(
        style = "font-size:14px; line-height:1.55; color:#334155;",
        tags$b("Continent averages"),
        " use the same country series as elsewhere. Each economy is mapped to a continent via ",
        tags$code("ISO3"), " (",
        tags$code("countrycode"),
        " → continent), then we take a ",
        tags$b("simple average"),
        " of countries with data in that year — not GDP- or population-weighted."
      ),
      tags$p(
        style = "font-size:13px; color:#64748b; margin-top:12px;",
        "Open the tabs below for ",
        tags$b("Africa, Americas, Asia, Europe, Oceania"),
        " — one clean line chart and a short readout per region."
      )
    )
  })

  continent_plot_for <- function(continent) {
    pr <- d()
    fmt <- m()$fmt
    ylab <- paste("Mean", m()$ylab_country)
    ur <- pr$un_region_trend |>
      filter(.data$macro_region == continent) |>
      arrange(.data$Year)
    if (nrow(ur) == 0) {
      return(
        plotly_empty() |>
          layout(
            title = list(text = paste("No data for", continent), x = 0.5, xanchor = "center"),
            margin = list(t = 72)
          )
      )
    }
    p <- ggplot(
      ur,
      aes(
        x = .data$Year,
        y = .data$mean_index,
        text = paste0(
          continent, ", ", .data$Year, ": ", fmt(.data$mean_index),
          "<br>Countries (n): ", .data$n
        )
      )
    ) +
      geom_line(linewidth = 1.15, color = col_main) +
      geom_point(size = 2.5, color = col_accent) +
      scale_x_continuous(breaks = pr$years) +
      scale_y_continuous(labels = ~ fmt(.)) +
      labs(
        title = str_wrap(paste(continent), width = 40),
        subtitle = str_wrap("One series per tab; scroll years on the x-axis.", width = 48),
        x = NULL,
        y = ylab
      ) +
      theme_dash()
    ggplotly_centered(p)
  }

  continent_insight_for <- function(continent) {
    pr <- d()
    fmt <- m()$fmt
    ur <- pr$un_region_trend |>
      filter(.data$macro_region == continent) |>
      arrange(.data$Year)
    if (nrow(ur) == 0) {
      return(tags$p(style = "color:#64748b;", paste("No observations mapped to", continent, ".")))
    }
    y1 <- min(ur$Year)
    y2 <- max(ur$Year)
    v1 <- ur$mean_index[ur$Year == y1][1]
    v2 <- ur$mean_index[ur$Year == y2][1]
    n1 <- ur$n[ur$Year == y1][1]
    n2 <- ur$n[ur$Year == y2][1]
    chg <- if (is.finite(v1) && abs(v1) > 1e-12) (v2 - v1) / abs(v1) * 100 else NA_real_
    trend <- if (is.na(chg)) {
      "The regional mean is flat or undefined over this window."
    } else if (chg > 3) {
      "The average for this continent is trending up: typical members show higher values over time (subject to coverage)."
    } else if (chg < -3) {
      "The average is trending down: check crises, data revisions, or fewer reporting countries in later years."
    } else {
      "The average is fairly stable — no dramatic shift for the mean country in this sample."
    }
    tags$div(
      class = "insight-callout continent-insight",
      tags$h4(paste(continent, "— readout")),
      tags$p(
        tags$b(paste(y1, "→", y2, ": ")),
        "mean moves from ", tags$b(fmt(v1)), " to ", tags$b(fmt(v2)),
        if (is.na(chg)) "." else paste0(" (", sprintf("%+.1f", chg), "% vs ", y1, ").")
      ),
      tags$p(tags$b("Countries in the average: "), "about ", n1, " (", y1, ") and ", n2, " (", y2, ")."),
      tags$p(trend),
      tags$p(
        style = "font-size:12px; color:#64748b; margin-bottom:0;",
        "Unweighted mean: small and large economies count equally."
      )
    )
  }

  output$plot_ct_africa <- renderPlotly({ continent_plot_for("Africa") })
  output$plot_ct_americas <- renderPlotly({ continent_plot_for("Americas") })
  output$plot_ct_asia <- renderPlotly({ continent_plot_for("Asia") })
  output$plot_ct_europe <- renderPlotly({ continent_plot_for("Europe") })
  output$plot_ct_oceania <- renderPlotly({ continent_plot_for("Oceania") })

  output$insight_ct_africa <- renderUI({ continent_insight_for("Africa") })
  output$insight_ct_americas <- renderUI({ continent_insight_for("Americas") })
  output$insight_ct_asia <- renderUI({ continent_insight_for("Asia") })
  output$insight_ct_europe <- renderUI({ continent_insight_for("Europe") })
  output$insight_ct_oceania <- renderUI({ continent_insight_for("Oceania") })

  output$plot_trend_compare <- renderPlotly({
    pr <- d()
    fmt <- m()$fmt
    req(input$trend_countries)
    tc <- input$trend_countries
    if (length(tc) == 0) {
      return(plotly_empty() |> layout(title = "Select at least one country"))
    }
    dl <- pr$df_long |>
      filter(Country %in% tc, Year <= pr$end_year) |>
      select(Year, Country, Index)
    world <- pr$global_trend |>
      select(Year, mean_index) |>
      mutate(Country = "World average", Index = mean_index) |>
      select(Year, Country, Index)
    dl2 <- bind_rows(dl, world)

    p <- ggplot(
      dl2,
      aes(
        x = Year,
        y = Index,
        color = Country,
        group = Country,
        text = paste0(Country, " — ", Year, ": ", fmt(Index))
      )
    ) +
      geom_line(linewidth = 1.05) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = pr$years) +
      scale_y_continuous(labels = ~ fmt(.)) +
      scale_color_manual(values = rep(pal_lines, length.out = length(unique(dl2$Country)))) +
      labs(
        title = "Interactive comparison",
        subtitle = "Selected economies and world average",
        x = NULL,
        y = m()$ylab_country
      ) +
      theme_dash()
    ggplotly_centered(p)
  })

  output$trend_insights <- renderUI({
    pr <- d()
    fmt <- m()$fmt
    div(
      class = "insight-callout",
      tags$h4("How to use this chart"),
      tags$p(
        "Select multiple countries to see convergence or divergence. ",
        "Compare against the world average line: economies above it are outperforming the global mean for this metric."
      ),
      tags$p(
        "For digitally delivered services, steep upward slopes often reflect ",
        tags$b("offshoring, ICT-enabled exports, and business services"), "; flat or declining lines may reflect ",
        "price effects, exchange rates, or structural shifts — not necessarily lower digital adoption."
      )
    )
  })

  output$plot_relative <- renderPlotly({
    pr <- d()
    fmt <- m()$fmt
    p <- pr$relative_position |>
      filter(Year == pr$end_year) |>
      arrange(desc(gap_to_world)) |>
      slice(1:input$top_n) |>
      mutate(Country = fct_reorder(Country, gap_to_world)) |>
      mutate(Country_lbl = country_axis_label(as.character(Country))) |>
      ggplot(
        aes(
          x = Country_lbl,
          y = gap_to_world,
          text = paste0(Country, "<br>Gap to world: ", fmt(gap_to_world))
        )
      ) +
      geom_col(fill = col_accent, width = 0.72) +
      geom_hline(yintercept = 0, linewidth = 0.7, color = "black") +
      coord_flip() +
      scale_y_continuous(labels = ~ fmt(.)) +
      labs(
        title = str_wrap(paste("Distance from world average,", pr$end_year), width = 32),
        subtitle = str_wrap(
          "Each bar: economy value minus world average (same year).",
          width = 42
        ),
        x = NULL,
        y = "Value − world average"
      ) +
      theme_dash()
    ggplotly_centered(p)
  })

  output$plot_catchup <- renderPlotly({
    pr <- d()
    fmt <- m()$fmt
    ct <- pr$catchup_table |> slice(1:input$top_n)
    p <- ct |>
      mutate(Country = fct_reorder(Country, absolute_change)) |>
      mutate(Country_lbl = country_axis_label(as.character(Country))) |>
      ggplot(
        aes(
          x = Country_lbl,
          y = absolute_change,
          text = paste0(Country, "<br>Change: ", fmt(absolute_change))
        )
      ) +
      geom_col(fill = col_good, width = 0.72) +
      coord_flip() +
      scale_y_continuous(labels = ~ fmt(.)) +
      labs(
        title = str_wrap("Catch-up candidates", width = 40),
        subtitle = str_wrap(
          "Lower starting level, largest absolute gains to end year.",
          width = 42
        ),
        x = NULL,
        y = "Absolute change"
      ) +
      theme_dash()
    ggplotly_centered(p)
  })

  output$comparative_note <- renderUI({
    if (is.null(input$dataset) || input$dataset == "indigo") {
      return(HTML(paste0(
        "<p><b>Comparative lens (INDIGO).</b> The charts summarize ",
        "<b>level vs the sample mean</b> (gap to world average) and <b>momentum</b> (catch-up among lower starters).</p>",
        "<p>Higher INDIGO values proxy stronger digital trade capability across multiple dimensions.</p>"
      )))
    }
    HTML(paste0(
      "<p><b>Comparative lens (DDS).</b> The charts compare each economy to the <b>world average</b> in ",
      "digitally delivered services exports and highlight <b>catch-up</b> (large gains from a lower starting level). ",
      "Values are in US$ million as in the source table.</p>"
    ))
  })

  output$plot_quadrant <- renderPlotly({
    pr <- d()
    fmt <- m()$fmt
    p <- ggplot(
      pr$quadrant_data,
      aes(
        x = level_centered,
        y = growth_centered,
        color = quadrant,
        text = paste0(
          Country, "<br>Latest: ", fmt(Index),
          "<br>Δ: ", fmt(absolute_change), "<br>", quadrant
        )
      )
    ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
      geom_point(size = 3, alpha = 0.88) +
      scale_color_manual(values = c(
        "Leaders" = col_main,
        "Catch-up" = col_good,
        "Mature/Slowing" = col_accent,
        "Lagging" = col_warn
      )) +
      labs(
        title = "Quadrants: level vs growth (both demeaned)",
        subtitle = "Right = above sample mean level; Top = above sample mean growth",
        x = "Latest value (centered)",
        y = "Total change (centered)"
      ) +
      theme_dash()
    ggplotly_centered(p)
  })

  output$quadrant_insight <- renderUI({
    tags$p(
      style = "font-size:13px;",
      tags$b("Leaders: "), "high level and strong growth. ",
      tags$b("Catch-up: "), "lower starting level but strong gains. ",
      tags$b("Mature/Slowing: "), "high level but weaker marginal growth. ",
      tags$b("Lagging: "), "below average on both dimensions in this sample."
    )
  })

  selected_country_row <- reactive({
    pr <- d()
    pr$dashboard_table |> filter(Country == input$selected_country)
  })

  selected_country_series <- reactive({
    pr <- d()
    pr$df_long |> filter(Country == input$selected_country, Year <= pr$end_year)
  })

  output$vb_country_index <- renderValueBox({
    row <- selected_country_row()
    fmt <- m()$fmt
    pr <- d()
    valueBox(
      value = fmt(row$Index),
      subtitle = paste(input$selected_country, "—", pr$end_year),
      icon = icon("signal"),
      color = "aqua"
    )
  })

  output$vb_country_gap <- renderValueBox({
    row <- selected_country_row()
    fmt <- m()$fmt
    valueBox(
      value = fmt(row$gap_to_world),
      subtitle = "Vs world average",
      icon = icon("balance-scale"),
      color = ifelse(row$gap_to_world >= 0, "green", "red")
    )
  })

  output$vb_country_growth <- renderValueBox({
    row <- selected_country_row()
    valueBox(
      value = ifelse(is.na(row$pct_growth), "NA", paste0(round(row$pct_growth, 1), "%")),
      subtitle = paste("Growth", d()$start_year, "→", d()$end_year),
      icon = icon("arrow-up"),
      color = "yellow"
    )
  })

  output$plot_country <- renderPlotly({
    pr <- d()
    fmt <- m()$fmt
    country_df <- selected_country_series()
    world_line <- pr$global_trend |> select(Year, mean_index)
    p <- ggplot() +
      geom_line(
        data = world_line,
        aes(
          x = Year,
          y = mean_index,
          text = paste0("World avg — ", Year, ": ", fmt(mean_index))
        ),
        linewidth = 1,
        linetype = "dashed",
        color = col_warn
      ) +
      geom_line(
        data = country_df,
        aes(
          x = Year,
          y = Index,
          text = paste0(input$selected_country, " — ", Year, ": ", fmt(Index))
        ),
        linewidth = 1.2,
        color = col_main
      ) +
      geom_point(
        data = country_df,
        aes(x = Year, y = Index, text = paste0(input$selected_country, " — ", Year, ": ", fmt(Index))),
        size = 3,
        color = col_main
      ) +
      scale_x_continuous(breaks = pr$years) +
      scale_y_continuous(labels = ~ fmt(.)) +
      labs(
        title = paste(input$selected_country, "vs world average"),
        subtitle = "Interactive — hover points for values",
        x = NULL,
        y = m()$ylab_country
      ) +
      theme_dash()
    ggplotly_centered(p)
  })

  output$country_profile <- renderTable({
    row <- selected_country_row()
    fmt <- m()$fmt
    data.frame(
      Metric = c(
        "Rank (latest)",
        "Group",
        "Latest value",
        "Start",
        "End",
        "Absolute change",
        "% change",
        "Gap to world",
        "Ratio to world"
      ),
      Value = c(
        row$rank_latest,
        row$comparative_group,
        as.character(fmt(row$Index)),
        as.character(fmt(row$start_value)),
        as.character(fmt(row$end_value)),
        as.character(fmt(row$absolute_change)),
        ifelse(is.na(row$pct_growth), NA, paste0(round(row$pct_growth, 2), "%")),
        as.character(fmt(row$gap_to_world)),
        round(row$ratio_to_world, 3)
      )
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "m")

  output$table_dashboard <- renderDT({
    pr <- d()
    fmt_num <- function(x) round(x, 4)
    datatable(
      pr$dashboard_table |>
        mutate(
          Index = fmt_num(Index),
          start_value = fmt_num(start_value),
          end_value = fmt_num(end_value),
          absolute_change = fmt_num(absolute_change),
          pct_growth = round(pct_growth, 2),
          gap_to_world = fmt_num(gap_to_world),
          ratio_to_world = round(ratio_to_world, 3)
        ) |>
        select(
          Country, rank_latest, comparative_group, Index,
          start_value, end_value, absolute_change, pct_growth,
          gap_to_world, ratio_to_world
        ),
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        pageLength = 12,
        scrollX = TRUE
      )
    )
  })
}

# ===========================
# 7. RUN
# ===========================
shinyApp(ui, server)
