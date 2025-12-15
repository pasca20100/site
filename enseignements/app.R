# -----------------------------
# LIBRAIRIES
# -----------------------------
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(rnaturalearthdata)

# -----------------------------
# DONNÉES
# -----------------------------
cropland <- read_csv("cropland.csv") %>%
  rename(cropland_ha = `Land use: Cropland`) %>%
  mutate(Code = case_when(
    Code=="FR" ~ "FRA",
    Code=="US" ~ "USA",
    TRUE ~ Code
  )) %>%
  filter(!is.na(Code))

population <- read_csv("population.csv") %>%
  rename(Pop_proj=`Population (projections)`,
         Pop_hist=`Population (historical)`) %>%
  mutate(Population = ifelse(!is.na(Pop_hist), Pop_hist, Pop_proj),
         Code = case_when(
           Code=="FR" ~ "FRA",
           Code=="US" ~ "USA",
           TRUE ~ Code
         )) %>%
  select(Code, Year, Population)

wheat_real <- read_csv("wheat_production.csv") %>%
  rename(wheat_t_real = `Wheat | 00000015 || Production | 005510 || tonnes`) %>%
  filter(Year >= 1961)

years <- seq(min(cropland$Year), max(cropland$Year), 1)

population_full <- population %>%
  group_by(Code) %>%
  complete(Year = years) %>%
  fill(Population, .direction="downup") %>%
  ungroup()

yield_historical <- function(year){
  case_when(
    year < -3000 ~ 0.4,
    year < 0     ~ 0.6,
    year < 1500  ~ 0.7,
    year < 1900  ~ 0.8,
    year < 1961  ~ 1.0
  )
}

a_opt <- 1
b_opt <- 0.02

world <- rnaturalearthdata::countries110 %>%
  as.data.frame() %>%
  select(iso_a3, name)

colors_green <- brewer.pal(9,"Greens")
colors_red   <- brewer.pal(9,"Reds")

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Grains d'Histoire : production et tensions sur le blé"),
  
  fluidRow(
    column(12,
           wellPanel(
             h4("Introduction"),
             p("Explorez l'évolution historique de la production de blé, comparez les données estimées et réelles, 
               et suivez l'indice de tension par pays : rapport production nationale / production mondiale.")
           )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(width=3,
                 pickerInput("selected_countries", "Sélectionner pays",
                             choices = sort(world$name),
                             selected = sort(world$name)[1:10],
                             multiple = TRUE,
                             options = list(`actions-box`=TRUE, `live-search`=TRUE)),
                 sliderInput("share_wheat", "Part du blé dans les cultures",
                             min=0.1, max=0.5, value=0.28, step=0.01)
    ),
    
    mainPanel(width=9,
              wellPanel(
                h4("Paramètres choisis"),
                p("Rendements historiques et part du blé dans les cultures sont utilisés pour estimer la production. 
                  Paramètres par défaut : a_opt = 1, b_opt = 0.02. Vous pouvez ajuster la part de blé.")
              ),
              plotlyOutput("map_total", height="400px"),
              plotlyOutput("map_percap", height="400px"),
              plotlyOutput("global_prod_plot", height="300px"),
              wellPanel(
                h4("Méthodologie : données estimées"),
                p("Les productions estimées sont calculées à partir des surfaces cultivées, 
                  de la part de blé dans les cultures et des rendements historiques. 
                  Pour 1961-2023, les données réelles sont intégrées et comparées aux estimations.")
              ),
              plotlyOutput("comparison_plot", height="350px"),
              wellPanel(
                h4("Indice de tension par pays"),
                p("Indice = production nationale / production mondiale pour chaque année. 
                  Valeur proche de 1 : situation normale. 
                  Supérieure à 1 : tension sur l'approvisionnement. Inférieure à 1 : abondance.")
              ),
              plotlyOutput("tension_plot", height="350px"),
              wellPanel(
                h5("Sources et bibliographie"),
                p("- FAO : base de données production de blé"),
                p("- Données historiques de population et surfaces cultivées"),
                p("- Auteur : application pédagogique Grains d'Histoire")
              )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session){
  
  reactive_data <- reactive({
    d <- cropland %>%
      group_by(Code, Year) %>%
      summarise(cropland_ha=sum(cropland_ha), .groups="drop") %>%
      left_join(population_full, by=c("Code","Year")) %>%
      mutate(
        yield_t_ha = ifelse(Year<1961, yield_historical(Year), a_opt + b_opt*(Year-1961)),
        wheat_t = cropland_ha * input$share_wheat * yield_t_ha,
        ratio_t_per_cap = wheat_t / Population
      ) %>%
      left_join(wheat_real, by=c("Code","Year")) %>%
      left_join(world, by=c("Code"="iso_a3")) %>%
      filter(name %in% input$selected_countries)
    
    d$name[is.na(d$name)] <- d$Code[is.na(d$name)]
    
    # Indice de tension = prod nationale / prod mondiale
    d <- d %>%
      group_by(Year) %>%
      mutate(global_prod = sum(wheat_t, na.rm=TRUE),
             tension = wheat_t / global_prod) %>%
      ungroup()
    
    d
  })
  
  # Carte production totale (verts)
  output$map_total <- renderPlotly({
    d <- reactive_data()
    plot_ly(d, type="choropleth", locations=~Code, locationmode="ISO-3",
            z=~wheat_t, frame=~Year,
            colorscale=colors_green,
            text=~paste0(name,"<br>Production: ", round(wheat_t/1e6,2)," Mt"))) %>%
    layout(title="Production totale de blé (verts)",
           geo=list(showland=TRUE, landcolor="rgb(245,245,245)",
                    projection=list(type="orthographic")))
  })

# Carte production/habitant (rouges)
output$map_percap <- renderPlotly({
  d <- reactive_data()
  plot_ly(d, type="choropleth", locations=~Code, locationmode="ISO-3",
          z=~ratio_t_per_cap, frame=~Year,
          colorscale=colors_red,
          text=~paste0(name,"<br>t/hab: ", round(ratio_t_per_cap,3)))) %>%
  layout(title="Production par habitant (rouges)",
         geo=list(showland=TRUE, landcolor="rgb(245,245,245)",
                  projection=list(type="orthographic")))
})

# Graph production mondiale globale
output$global_prod_plot <- renderPlotly({
  d <- reactive_data() %>%
    group_by(Year) %>%
    summarise(global_prod = sum(wheat_t, na.rm=TRUE))
  plot_ly(d, x=~Year, y=~global_prod, type="scatter", mode="lines",
          line=list(color="darkgreen")) %>%
    layout(title="Production mondiale globale de blé",
           yaxis=list(title="Production totale (t)"),
           xaxis=list(title="Année"))
})

# Comparaison estimée / réelle
output$comparison_plot <- renderPlotly({
  d <- reactive_data() %>% filter(Year>=1961, !is.na(wheat_t_real))
  plot_ly(d, x=~wheat_t_real, y=~wheat_t,
          type="scatter", mode="markers") %>%
    layout(title="Production estimée vs réelle (1961–2023)",
           xaxis=list(title="Réelle (t)"),
           yaxis=list(title="Estimée (t)"))
})

# Indice de tension par pays
output$tension_plot <- renderPlotly({
  d <- reactive_data() %>%
    group_by(Year, name) %>%
    summarise(tension = mean(tension, na.rm=TRUE))
  
  plot_ly(d, x=~Year, y=~tension, color=~name, type="scatter", mode="lines") %>%
    layout(title="Indice de tension par pays",
           yaxis=list(title="Indice de tension"),
           xaxis=list(title="Année"))
})
}

# -----------------------------
# LANCEMENT
# -----------------------------
shinyApp(ui, server)


