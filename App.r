#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(hrbrthemes)
library(tidyr)

# Tables
sales2019 <- read.csv('data/ventes_2019.csv', sep = ';')
prix2019 <- read.csv('data/prix_2019.csv', sep = ';')
communes <- read.csv('data/communes.csv', sep = ';')

todayDate <- format(Sys.time(), "%a %d %b %Y")

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

# Define UI for application that draws a histogram
ui <- navbarPage(
    'Herriko Haragia',
    theme = shinytheme('cerulean'),
    
    tabPanel('Accueil',
             sidebarLayout(
                 sidebarPanel(
                     img(src = 'user_icon.png', width = 70, align = 'center'),
                     h4('Benoit Jean'),
                     h4(todayDate),
                     tags$hr(),
                 ),
                 mainPanel(
                     h2('Tableau de Bord principal'),
                     h4('Evolution des Prix'),
                     plotOutput("pricePlot"),
                     tableOutput("excel1")
                 )
             ),
    ),
    
    tabPanel('Analyse Données',
             sidebarLayout(
                 sidebarPanel(
                     img(src = 'user_icon.png', width = 70, align = 'center'),
                     h4('Benoit Jean'),
                     tags$hr()
                     
                 ),
                 mainPanel(
                     h2('Analyse de Données')
                 )
             )
    ),
    
    tabPanel('Carte Interactive',
             sidebarLayout(
                 sidebarPanel(
                     img(src = 'user_icon.png', width = 70, align = 'center'),
                     h4('Benoit Jean'),
                     tags$hr(),
                     
                     selectInput('sel_town',
                                 'Commune :',
                                 c('Tout',
                                   unique(as.character(sales2019$Commune)))),
                     
                 ),
                 mainPanel(
                     
                     h2('Carte Interactive'),
                     h4('Visualisation des points de ventes partenaires.'),
                     leafletOutput("mymap"),
                     p(),
                     actionButton("recalc", "Nouveau point")
                 )
             )
    ),
    
    tabPanel('Ventes Opérateurs',
             sidebarLayout(
                 sidebarPanel(
                     img(src = 'user_icon.png', width = 70, align = 'center'),
                     h4('Benoit Jean'),
                     tags$hr(),
                     helpText('Filtrez les données en sélectionnant les critères qui vous intéressent.'),
                     
                     selectInput('sel_cust',
                                 'Clients :',
                                 c('Tout',
                                   unique(as.character(sales2019$Clients)))),
                     
                     selectInput('sel_type',
                                 'Type de clients :',
                                 c('Tout',
                                   unique(as.character(sales2019$Type.de.client)))),
                     
                     selectInput('sel_zone',
                                 'Zone géographique :',
                                 c('Tout',
                                   unique(as.character(sales2019$Zone.géographique)))),
                     
                     selectInput('sel_city',
                                 'Commune :',
                                 c('Tout',
                                   unique(as.character(sales2019$Commune)))),
                     tags$hr()
                     
                 ),
                 
                 mainPanel(
                     h2('Ventes Opérateurs'),
                     DT::dataTableOutput('sales')
                 )
             )
    ),
    
    ### Onglet FUSION TABLE
    tabPanel('Fusion de tableaux',
             sidebarLayout(
                 
                 # Sidebar panel for inputs ----
                 sidebarPanel(
                     img(src = 'user_icon.png', width = 70, align = 'center'),
                     h4('Benoit Jean'),
                     
                     tags$hr(),
                     
                     # Input: Select a file ----
                     fileInput("csv1", "Opérateur 1 : ",
                               buttonLabel = 'Importer un fichier',
                               multiple = TRUE,
                               accept = c("text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")),
                     
                     fileInput("csv2", "Opérateur 2 : ",
                               buttonLabel = 'Importer un fichier',
                               multiple = FALSE,
                               accept = c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ",",
                                   ".csv")
                     ),
                     
                     fileInput("csv3", "Opérateur 3 : ",
                               buttonLabel = 'Importer un fichier',
                               multiple = FALSE,
                               accept = c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ",",
                                   ".csv")
                     ),
                     
                     fileInput("csv4", "Opérateur 4 : ",
                               buttonLabel = 'Importer un fichier',
                               multiple = FALSE,
                               accept = c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ",",
                                   ".csv")
                     ),
                     tags$hr(),
                     
                     actionButton("fusionBtn", "Fusionner les tableaux", class = "btn-primary"),
                     
                     tags$hr(),
                     
                     downloadButton('downloadBtn', "Télécharger sous Excel", class = "btn-success")
                     
                 ),
                 
                 # Main panel for displaying outputs ----
                 mainPanel(
                     
                     h2('Fusion des données'),
                     h4('Sélectionnez les tableaux des opérateurs à fusionner.'),
                     textOutput("csvname1"),
                     textOutput("csvname2"),
                     textOutput("csvname3"),
                     textOutput("csvname4"),
                     textOutput("fus_ok"),
                     DT::dataTableOutput('final')
                     
                 )
             )
    ),
    
    # Footer
    tags$footer("Herriko Harragia © - 
            Dashboard By Meidi KADRI - Cédric DROMZEE - Ronan PELE - 2020",
                align = "center", 
                style = "
                             position:absolute;
                             bottom:0;
                             width:100%;
                             height:50px;   /* Height of the footer */
                             color: white;
                             padding: 10px;
                             background-color: #3A9AD1;
                             z-index: 1000;")
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    ########################################
    ##### Ventes
    
    output$sales <- DT::renderDataTable(DT::datatable({
        data <- sales2019
        if (input$sel_cust != 'Tout'){
            data <- data[data$Clients == input$sel_cust, ]
        }
        if (input$sel_type != 'Tout'){
            data <- data[data$Type.de.client == input$sel_type, ]
        }
        if (input$sel_zone != 'Tout'){
            data <- data[data$Zone.géographique == input$sel_zone, ]
        }
        if (input$sel_city != 'Tout'){
            data <- data[data$Commune == input$sel_city, ]
        }
        data
    }))
    #######################################
    

    ########################################
    ##### Graphs
    
    output$pricePlot <- renderPlot({
        # Plot
        ggplot(prix2019, aes(x= prix2019[,1])) +
            geom_point(aes(y = prix2019[,2], color = names(prix2019[2])),size = prix2019[,6])+
            geom_point(aes(y = prix2019[,3], color = names(prix2019[3])), size = 4)+
            geom_point(aes(y = prix2019[,4], color = names(prix2019[4])), size = 4)+
            xlab("Trimestres 2019") +
            ylab("Prix")
        
    })
    #######################################
    
    
    ########################################
    ##### Upload des datas
    
    csvOne <- eventReactive(input$csv1,{
      inFile <- input$csv1
      if (is.null(inFile))
        return(NULL)

      read.csv(inFile$datapath, sep = ';')
    })
    
    output$csvname1 <- renderText(paste("Tableaux n°1 : ", input$csv1$name))
    
    csvTwo <- eventReactive(input$csv2,{
        inFile <- input$csv2
        if (is.null(inFile))
            return(NULL)
        
        read.csv(inFile$datapath, sep = ';')
    })
    
    output$csvname2 <- renderText(paste("Tableaux n°2 : ", input$csv2$name))
    
    csvThree <- eventReactive(input$csv3,{
        inFile <- input$csv3
        if (is.null(inFile))
            return(NULL)
        
        read.csv(inFile$datapath, sep = ';')
    })
    
    output$csvname3 <- renderText(paste("Tableaux n°3 : ", input$csv3$name))
    
    csvFour <- eventReactive(input$csv4,{
        inFile <- input$csv4
        if (is.null(inFile))
            return(NULL)
        
        read.csv(inFile$datapath, sep = ';')
    })
    
    output$csvname4 <- renderText(paste("Tableaux n°4 : ", input$csv4$name))
    
    #######################################
    
    ########################################
    ##### Fusions des données 
    # do.call permet de faire un merge sur une liste de dataframes
    final_react <- eventReactive(input$fusionBtn,
                                 do.call("rbind", list(csvOne(), csvTwo(), csvThree(), csvFour())))
    
    output$final <- DT::renderDataTable({
        final_react()
    })
    
    #######################################
    
    ########################################
    ##### Download management
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("test.csv", sep = "")
        },
        content = function(file) {
            write.csv(data, file, row.names = TRUE)
        }
    )
    
    
    # Allow user to download responses
    output$downloadBtn <- downloadHandler(
        filename = function() { 
            sprintf("csv001.csv", final_react())
        },
        content = function(file) {
            write.csv(loadData(), file, row.names = FALSE)
        }
    )
    #######################################
    
    
    ########################################
    ##### Cartes
    
    # villes <- data.frame(Ville = c("Bayonne", "Souraïde", "URT", "Tarnos"),
    #                      Latitude = c(43.483333, 43.2034, 43.2941, 43.3228),
    #                      Longitude = c(-1.483333, -1.2825, -1.1721, -1.2736),
    #                      Population = c(4115, 4259, 3062, 2729))
    
    villes <- data.frame(Ville = c("Bayonne"),
                         Latitude = c(43.483333),
                         Longitude = c(-1.483333),
                         Population = c(4115))
    
    #villes2 <- data.frame(Ville = c('Tout', unique(as.character(sales2019$Commune))))
    # villes2 <- data.frame(Ville = c(unique(as.character(communes$nom_commune))),
    #                       Latitude = c(unique(as.character(communes$latitude))),
    #                       Longitude = c(unique(as.character(communes$longitude))),
    #                       Population = c(unique(as.character(sales2019$Jan-Mar.2019)))
   #                       )
    
    couleurs <- colorNumeric("YlOrRd", villes$Population, n = 5)
    
    output$mymap <- renderLeaflet({
        leaflet(villes) %>% 
          addTiles() %>%
          addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1,
                       radius = ~sqrt(Population) * 50, popup = ~paste(Ville, ":", Population),
                       color = ~couleurs(Population), fillOpacity = 0.9) %>%
          addLegend(pal = couleurs, values = ~Population, opacity = 0.9)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
