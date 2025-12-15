library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(countrycode)

# -----------------------------
# STANDARDISATION ISO
# -----------------------------
standardize_iso <- function(x){
  case_when(
    x == "FR" ~ "FRA",
    x == "US" ~ "USA",
    x == "GB" ~ "GBR",
    TRUE ~ x
  )
}

# -----------------------------
# DONNÉES
# -----------------------------
cropland <- read_csv("cropland.csv", show_col_types = FALSE) %>%
  rename(cropland_ha = `Land use: Cropland`) %>%
  mutate(Code = standardize_iso(Code)) %>%
  filter(!is.na(Code))

population <- read_csv("population.csv", show_col_types = FALSE) %>%
  rename(
    Pop_proj = `Population (projections)`,
    Pop_hist = `Population (historical)`
  ) %>%
  mutate(
    Population = ifelse(!is.na(Pop_hist), Pop_hist, Pop_proj),
    Code = standardize_iso(Code)
  ) %>%
  select(Code, Year, Population)

wheat_real <- read_csv("wheat_production.csv", show_col_types = FALSE) %>%
  rename(wheat_t_real = `Wheat | 00000015 || Production | 005510 || tonnes`) %>%
  mutate(Code = standardize_iso(Code)) %>%
  filter(Year >= 1961)

# -----------------------------
# DICTIONNAIRE ISO → NOM
# -----------------------------
all_codes <- sort(unique(c(cropland$Code, population$Code, wheat_real$Code)))

world_full <- tibble(iso_a3 = all_codes) %>%
  mutate(name = countrycode(iso_a3, origin = "iso3c", destination = "country.name.en")) %>%
  mutate(name = ifelse(is.na(name), iso_a3, name))  # fallback

menu_countries <- world_full %>% arrange(name)

# -----------------------------
# POPULATION COMPLÈTE
# -----------------------------
years <- seq(min(cropland$Year), max(cropland$Year))

population_full <- population %>%
  group_by(Code) %>%
  complete(Year = years) %>%
  fill(Population, .direction = "downup") %>%
  ungroup()

# -----------------------------
# RENDEMENTS HISTORIQUES
# -----------------------------
yield_historical <- function(year){
  case_when(
    year < -3000 ~ 0.4,
    year < 0     ~ 0.6,
    year < 1500  ~ 0.7,
    year < 1900  ~ 0.8,
    year < 1961  ~ 1.0,
    TRUE ~ NA_real_
  )
}

a_opt <- 1
b_opt <- 0.02

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  tags$style(HTML("
    .rounded-box {
      border-radius: 15px;
      overflow: hidden;
      margin-bottom: 20px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.15);
    }
  ")),
  
  titlePanel("Grains d’Histoire : production et dépendance au blé"),
  
  fluidRow(
    column(12,
           wellPanel(
             h4("Introduction"),
             p("Cette application permet d’explorer l’évolution historique de la production de blé,
          de comparer des données estimées et observées, et d’analyser le poids relatif des pays
          dans l’approvisionnement mondial en blé.")
           )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        "selected_country",
        "Sélectionner un ou plusieurs pays (indice)",
        choices = setNames(menu_countries$iso_a3, menu_countries$name),
        selected = menu_countries$iso_a3,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      ),
      
      sliderInput(
        "share_wheat",
        "Part du blé dans les cultures",
        min = 0.1, max = 0.5, value = 0.28, step = 0.01
      )
    ),
    
    mainPanel(
      # CARTES
      div(class = "rounded-box", plotlyOutput("map_total", height = "400px")),
      div(class = "rounded-box", plotlyOutput("map_percap", height = "400px")),
      
      # METHODOLOGIE
      wellPanel(
        h4("Méthodologie – données estimées"),
        p("Les productions de blé sont estimées à partir des surfaces cultivées,
          d’une part moyenne du blé dans les cultures et de rendements historiques stylisés.
          À partir de 1961, les données observées issues de la FAO permettent de comparer
          estimations et données réelles.")
      ),
      
      # COMPARAISON ESTIMÉE vs RÉELLE
      plotlyOutput("comparison_plot", height = "350px"),
      
      # INDICE DE DEPENDANCE
      wellPanel(
        h4("Indice de dépendance au blé"),
        p("Indice = production nationale ÷ production mondiale.
          Il mesure le poids relatif des pays sélectionnés dans l’offre mondiale.")
      ),
      plotlyOutput("dependance_plot", height = "350px"),
      
      # SOURCES
      wellPanel(
        h4("Sources et bibliographie"),
        tags$ul(
          tags$li("FAO – FAOSTAT : production de blé"),
          tags$li("ONU – Données de population"),
          tags$li("Our World in Data – Surfaces agricoles"),
          tags$li("France Culture – Le Cours de l’Histoire, « Graines d’empires »")
        ),
        p(em("Application développée à des fins pédagogiques."))
      )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output){
  
  base_data <- reactive({
    cropland %>%
      group_by(Code, Year) %>%
      summarise(cropland_ha = sum(cropland_ha), .groups="drop") %>%
      left_join(population_full, by = c("Code", "Year")) %>%
      mutate(
        yield_t_ha = ifelse(Year < 1961, yield_historical(Year), a_opt + b_opt*(Year-1961)),
        wheat_t = cropland_ha * input$share_wheat * yield_t_ha,
        per_cap = wheat_t / Population
      ) %>%
      left_join(wheat_real, by = c("Code","Year")) %>%
      left_join(world_full, by = c("Code" = "iso_a3"))
  })
  
  world_totals <- reactive({
    base_data() %>%
      group_by(Year) %>%
      summarise(prod_mondiale_ble = sum(wheat_t, na.rm = TRUE), .groups = "drop")
  })
  
  country_data <- reactive({
    req(input$selected_country)
    base_data() %>%
      filter(Code %in% input$selected_country) %>%
      left_join(world_totals(), by = "Year") %>%
      mutate(indice_dependance_ble = wheat_t / prod_mondiale_ble)
  })
  
  # Carte production totale
  output$map_total <- renderPlotly({
    d <- base_data()
    z_range <- range(log(d$wheat_t+1), na.rm=TRUE)
    plot_ly(d, type="choropleth", locations=~Code, locationmode="ISO-3",
            z=~log(wheat_t+1), zmin=z_range[1], zmax=z_range[2],
            colorscale=list(c(0,"#f7fcf5"),c(1,"#00441b")),
            frame=~Year,
            text=~paste0(name,"<br>",round(wheat_t/1e6,1)," Mt"), hoverinfo="text") %>%
      layout(title="Production mondiale de blé", geo=list(showframe=FALSE))
  })
  
  # Carte production/habitant
  output$map_percap <- renderPlotly({
    d <- base_data()
    z_range <- range(log(d$per_cap+1), na.rm=TRUE)
    plot_ly(d, type="choropleth", locations=~Code, locationmode="ISO-3",
            z=~log(per_cap+1), zmin=z_range[1], zmax=z_range[2],
            colorscale=list(c(0,"#fff5f0"),c(1,"#67000d")),
            frame=~Year,
            text=~paste0(name,"<br>",round(per_cap,3)," t/hab"), hoverinfo="text") %>%
      layout(title="Production par habitant", geo=list(showframe=FALSE))
  })
  
  # Comparaison estimée vs réelle
  output$comparison_plot <- renderPlotly({
    d <- base_data() %>% filter(Year >= 1961, !is.na(wheat_t_real))
    plot_ly(d, x=~wheat_t_real, y=~wheat_t, type="scatter", mode="markers") %>%
      layout(title="Production estimée vs observée (1961–2023)",
             xaxis=list(title="Production réelle (t)"),
             yaxis=list(title="Production estimée (t)"))
  })
  
  # Indice de dépendance
  output$dependance_plot <- renderPlotly({
    d <- country_data()
    plot_ly(d, x=~Year, y=~indice_dependance_ble, color=~name, type="scatter", mode="lines") %>%
      layout(title="Indice de dépendance au blé", xaxis=list(title="Année"), yaxis=list(title="Indice"))
  })
}

# -----------------------------
# LANCEMENT
# -----------------------------
shinyApp(ui, server)
















