library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(shinymanager)
library(shinythemes)
library(dplyr)
library(highcharter)
library(DT)
library(leaflet)

source("requete.R")

credentials <- data.frame(
  user = utilisateur$username,
  password = utilisateur$password,
  stringsAsFactors = FALSE
) 

set_labels(
  language = "en",
  "Please authenticate" = "Connectez-vous",
  "Username:" = "Nom d'utilisateur",
  "Password:" = "Mot de passe",
  "Login" = "Connexion"
)

css <- HTML(".btn-primary {
                  color: #ffffff;
                  background-color: #0dc5c1;
                  border-color: #0dc5c1;
              }
              .panel-primary {
                  border-color: #0dc5c1;
              }")


  ui = dashboardPage(
    title = "Basic Dashboard",
    header = dashboardHeader(
      title <- dashboardBrand(
        title = "IsseaLink",
        color = "primary",
        href = "https://adminlte.io/themes/v3",
       # image = "issea.png"
      ),
      titleWidth = NULL,
      disable = FALSE,
      .list = NULL,
      skin = "light",
      status = "white",
      border = TRUE,
      compact = FALSE,
      sidebarIcon = shiny::icon("bars"),
      controlbarIcon = shiny::icon("th"),
      fixed = FALSE,
      leftUi = NULL,
      rightUi = NULL
    ),
    sidebar = dashboardSidebar(
      disable = FALSE,
      width = NULL,
      skin = "dark",
      status = "primary",
      elevation = 4,
      collapsed = FALSE,
      minified = TRUE,
      expandOnHover = TRUE,
      fixed = TRUE,
      id = "sidebar",
      customArea = NULL,
      
      sidebarUserPanel(
        image = "issea.png",
        name = "Admin"
      ),
      
      sidebarMenu(
        id = "sidebarmenu",
        sidebarHeader("Menu"),
        menuItem(
          "Employabilité",
          tabName = "Dashboard",
          icon = icon("dashboard")
        ),
        menuItem(
          "Salaire",
          tabName = "item2",
          icon = icon("id-card")
        ),
        
        menuItem(
          "Temps",
          tabName = "time",
          icon = icon("check-circle")
        ),
        menuItem(
          "Geographie",
          tabName = "geo",
          icon = icon("check-circle")
        ),
        
        menuItem(
          "Profils",
          tabName = "profil",
          icon = icon("users")
        ),
        menuItem(
          text = "Item List 1",
          icon = icon("bars"),
          startExpanded = FALSE,
          menuSubItem(
            text = "Item 3",
            tabName = "tab3",
            icon = icon("circle-thin")
          ),
          menuSubItem(
            text = "Item 4",
            tabName = "tab4",
            icon = icon("circle-thin")
          )
        )
      )
    ),
    controlbar = dashboardControlbar(
      id = NULL,
      disable = FALSE,
      width = 250,
      collapsed = TRUE,
      overlay = TRUE,
      skin = "dark",
      pinned = NULL,
     
    ),
    footer = dashboardFooter(
      left = a(
        href = "samirpemi1@gmail.com",
        target = "_blank", "Njifeguen Pemi Samir"
      ),
      right = "2023"
    ),
    body = dashboardBody(
      tabItems(
        tabItem(
          tabName = "Dashboard",
          title = "Employabilité",
          
          fluidRow(
            
            valueBox(
              width = 3,
              value = HTML(paste0("<b style = 'font-size:30'>",nbEl[1],"</b>")),
              HTML("<b style = 'font-size:30'>Anciens Elèves</b>"),
              icon = icon("credit-card"),
              color = "lightblue"
            ),
            
            valueBox(
              width = 3,
              value = HTML(paste0("<b style = 'font-size:30'>",nbTr[1],"</b>")),
              subtitle = HTML("<b style = 'font-size:30'>Travailleurs</b>"),
              icon = icon("users"),
              color = "success"
            ),
            valueBox(
              width = 3,
              value = HTML(paste0("<b style = 'font-size:30'>",nbFr[1],"</b>")),
              subtitle = HTML("<b style = 'font-size:30'>Fréquentent</b>"),
              icon = icon("line-chart"),
              color = "info"
            ),
            valueBox(
              width = 3,
              value = HTML(paste0("<b style = 'font-size:30'>",chom[1],"</b>")),
              subtitle = HTML("<b style = 'font-size:30'>Non situé(s)</b>"),
              icon = icon("line-chart"),
              color = "danger"
            )
          ),
          
          fluidRow(
            
            box(
              width = 6,
              title = "Employabilité par filière",
              highchartOutput("result1")
            ),
            box(
              width =6 ,
              title = "Secteurs sollicités",
              highchartOutput("secteur")
            )
            
          ),
          
          fluidRow(
            box(
              width =12 ,
              title = "",
              highchartOutput("filie_secteur")
            )  
                  
          ),
          
          # fluidRow(
          #   box(width = 2, actionButton("count", "Compter")),
          #   valueBoxOutput("vbox")
          # )
        ),
        # Salaire
        tabItem(
          tabName = "item2",
          
          fluidRow(
            valueBox(
              width = 4,
              value = HTML(paste0("<b style = 'font-size:30'>",intsalaire$min," FCFA</b>")),
              subtitle = HTML("<b style = 'font-size:30'>Salaire Minimum</b>"),
              icon = icon("users"),
              color = "info"
            ),
            valueBox(
              width = 4,
              value = HTML(paste0("<b style = 'font-size:30'>",intsalaire$round," FCFA</b>")),
              subtitle = HTML("<b style = 'font-size:30'>Salaire Moyen</b>"),
              icon = icon("users"),
              color = "warning"
            ),
            valueBox(
              width = 4,
              value = HTML(paste0("<b style = 'font-size:30'>",intsalaire$max," FCFA</b>")),
              subtitle = HTML("<b style = 'font-size:30'>Salaire Maximum</b>"),
              icon = icon("users"),
              color = "success"
            )
          ),
          
          fluidRow(
            box(
              title = "Salaire moyen par secteur",
              width = 6,
              highchartOutput("salair_sect")
            ),
            
            box(
              title = "Salaire moyen par Filière",
              width = 6,
              highchartOutput("salair_filiere")
            )
          ),
          
          fluidRow(
            box(
              title = "Intervalle Salarial",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              dataTableOutput("salair_table")
            )
          )
         
        ),
        tabItem(
          tabName = "time",
          fluidRow(
            box(
              width = 12,
              title = "Temps de recherche d'emploi",
              highchartOutput("temp")
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "Temps de recherche d'emploi par année et par filière",
              highchartOutput("temps_fil")
            )
          )
        ),
        tabItem(
          tabName = "geo",
          fluidRow(
            box(
              width = 12,
              title = "localisation des anciens étudiants",
              highchartOutput("local")
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "Map des anciens étudiants",
              leafletOutput("map")
            )
          )
          
          
          
         
        ),
        tabItem(
          tabName = "profil",
          fluidRow(
            box(
              solidHeader = TRUE,
              width = 8,
              selectInput("selected_option", "Sélectionnez un ancien etudiant :", choices = profil_el$nom)
            )
          ),
          fluidRow(
            box(
              title = "Card with messages",
              width = 9,
              userMessages(
                width = 12,
                status = "success",
                userMessage(
                  author = "Experience"
                 
                ),
                userMessage(
                  author = "Formation"
                )
              )
            )
          )
        )
      )
    )
  )
  
  
  server = function(input, output) {
    res_auth <- secure_server(
      check_credentials = check_credentials(credentials)
    )
    
    output$result1 = renderHighchart({
      highchart() %>% 
        hc_chart(type = "column") %>% 
        hc_add_series(data = travaille$count, showInLegend  = FALSE) %>% 
        hc_title(text = "Employabilité par filière") %>% 
        hc_add_theme(hc_theme_538()) %>% 
        hc_xAxis(categories = travaille$filiere, title = list(text = NULL)) %>% 
        hc_yAxis(title = list(text = "Nbr anciens élèves"))
    })
    
    output$secteur = renderHighchart({
      
      highchart() %>%
        hc_chart(type = "pie") %>% 
        hc_title(text = "Secteurs d'activités", align='center') %>% 
        hc_yAxis(title = list(text = "Fréquence")) %>%
        hc_xAxis(title = list(text = "Secteurs")) %>%
        hc_add_series(data= secto$secteur_activite) %>%
        hc_plotOptions(pie = list(dataLabels = list(enabled = TRUE, format = "{point.name}: {point.percentage:.1f}%")))
      
    })
    
    output$filie_secteur = renderHighchart({
      
      hchart(filier_secto, "column", hcaes(x = filier_secto$filiere, y=filier_secto$count, group="secteur_activite")) %>%
        hc_plotOptions(line = list(marker = list(enabled = FALSE, symbol="circle"))) %>%
        hc_xAxis(title = list(text = "Filière")) %>%
        hc_yAxis(labels = list(format = "{value:,.0f}"), title = list(text = "frequence")) %>%
        hc_title(text = "Secteurs d'activité par filière")
    })
    
    output$temp = renderHighchart({
      
      hc = highchart() %>%
        hc_add_series(type = "bar", temps$avg_years, showInLegend  = FALSE)  %>% 
        hc_xAxis(categories = temps$filiere) %>% 
        hc_title(text = "") %>% 
        hc_subtitle(text = "") %>%
        hc_add_theme(hc_theme_google())
   
    })
    
    output$temps_fil = renderHighchart({
      
      hchart(temps_filiere, "column", hcaes(x = temps_filiere$annee_dipl, y=temps_filiere$avg_years, group="filiere")) %>%
        hc_plotOptions(line = list(marker = list(enabled = FALSE, symbol="circle"))) %>%
        hc_xAxis(title = list(text = "Filière")) %>%
        hc_yAxis(labels = list(format = "{value:,.0f}"), title = list(text = "annees")) %>%
        hc_title(text = "")
    })
    
    output$local = renderHighchart({
      highchart() %>% 
        hc_chart(type = "column") %>% 
        hc_add_series(data = locali$count, showInLegend  = FALSE) %>% 
        hc_title(text = "") %>% 
        hc_xAxis(categories = locali$pays, title = list(text = NULL), color = "green") %>% 
        hc_yAxis(title = list(text = NULL))
    })
    
    output$salair_sect = renderHighchart({
      highchart() %>% 
        hc_chart(type = "column") %>% 
        hc_add_series(data = salaire_sect$moyen, showInLegend  = FALSE) %>% 
        hc_title(text = "") %>% 
        hc_xAxis(categories = salaire_sect$secteur_activite, title = list(text = NULL), color = "green") %>% 
        hc_yAxis(title = list(text = "Salaire"))
    })
    
    output$salair_filiere = renderHighchart({
      highchart() %>% 
        hc_chart(type = "column") %>% 
        hc_add_series(data = salair_filiere$moyen, showInLegend  = FALSE) %>% 
        hc_title(text = "") %>% 
        hc_xAxis(categories = salair_filiere$filiere, title = list(text = NULL)) %>% 
        hc_yAxis(title = list(text = "Salaire"))
    })
    
    output$salair_table <- renderDataTable({
      datatable(tabsalaire)  # Afficher le dataframe dans le tableau interactif
    })
    
    countries <- c("Germany", "Congo", "Côte d'Ivoire", "Spain", "United States", "Italy", "Mali", "Central African Republic", "United Kingdom", "Senegal")
    
    output$map <- renderLeaflet({
      leaflet() %>% 
        addTiles() %>% 
        addMarkers(data = map_data(countries))
    })
    
    
  }
  
  map_data <- function(countries) {
    data.frame(
      country = countries,
      lat = c(51.165691, -4.038333, 7.539989, 40.463667, 37.09024, 41.87194, 17.570692, 6.611111, 55.378051, 14.497401),
      lng = c(10.451526, 21.758664, -5.54708, -3.74922, -95.712891, 12.56738, -3.996166, 20.939444, -3.435973, -14.452362)
    )
  }
  
  #authentification
  ui <- secure_app(ui,
                   # changing theme for the credentials
                   
                   theme = shinythemes::shinytheme("united"),
                   # tags = list(
                   #   tags$h3("Connectez-vous"),
                   #   tags$img(src = "issea.png", width = 100, height = 100),
                   #   tags$br()
                   # ),
                   tags_top = tags$div(
                     tags$head(tags$style(css)))
                   
  )
  
  shinyApp(ui, server)