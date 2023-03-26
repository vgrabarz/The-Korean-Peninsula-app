library(sf)
library(shiny)
library(shinythemes)
library(ggplot2)
# remotes::install_github('rstudio/leaflet')
library(leaflet)
library(viridis)
library(colorspace)

all_rc = terra::rast("~/Korea/landocover/alllandcover.tif")
my_breaks = c(0, 0.001, 0.01, 0.1, 1.01)

tifclass <- c("Agricultural Land", "Grassland", "Forest Broadleaved",
              "Forest Needleleaved", "Forest Mixed", "Urban Areas", "Wetland" ,
              "Bare Land", "Water", "Shrubs")
classvalue = c(1,2,3,4,5,6,7,8,9,10)
granice = read_sf("~/Korea/boundaries/granice.shp")

safe_pal9= c("#88CCEE", "#CC6677", "#DDCC77",
             "#117733", "#888888","115588", "49FF33", "4D7C4B")

classcolor <- c("#FFFF00", "#CCFF99", "#339900", "#003300", "#669966",
                "#CC0000", "#99FFCC","#CCCCCC" ,"#000066","#663300")
classcolor2 <- c("#FFFF00", "#CCFF99", "#339900", "#003300")

ui <- fluidPage(theme = shinytheme("yeti"),
                navbarPage(
                  "Korean Peninsula App",
                  tabPanel("Landcover",
                           sidebarPanel(
                             helpText("The Landcover of Korean Peninsula."),
                             img(src="legend.png",height=270,width=185),
                             selectInput("year",
                                         label = "Select a year",
                                         choices = c(2001:2020),
                                         selected = "2010"))
                           , # sidebarPanel
                           mainPanel(
                             leafletOutput("map", height = 600),  verbatimTextOutput("info"),
                             helpText("No removal of the gray color from the background was used due to better visibility of the colors.")

                           ) # mainPanel
                  ),

                  tabPanel("Comparision",
                           sidebarPanel(
                             helpText("Select years to see The Map of Comprarision"),
                             selectInput("year1",
                                         label = "Select a year lower than below",
                                         choices = c(2001:2019),
                                         selected = "2010"),
                             selectInput("year2",
                                         label = "Select a year",
                                         choices = c(2002:2020),
                                         selected = "2015"))
                           , # sidebarPanel
                           mainPanel(plotOutput("map2"),
                                     helpText("I used Jensen-Shannon divergence to compare changes, if value equals 0 it means that those areas did not change. Higher result than 0 means bigger change in this area.")
                           )
                  ),
                  tabPanel("Comparision areas",
                           sidebarPanel(
                             helpText("Select years to see The Map of Comprarision in Administrative areas"),
                             selectInput("year1comp",
                                         label = "Select a year lower than below",
                                         choices = c(2001:2019),
                                         selected = "2010"),
                             selectInput("year2comp",
                                         label = "Select a year",
                                         choices = c(2002:2020),
                                         selected = "2015"))
                           , # sidebarPanel
                           mainPanel(plotOutput("map3"),
                                     helpText("I used Jensen-Shannon divergence to compare changes, if value equals 0 it means that those areas did not change. Higher result than 0 means bigger change in this area.")

                           )
                  ),
                  tabPanel("Clustering",
                           sidebarPanel(
                             helpText("Select a Year to see The Map of Clustering"),
                             selectInput("year1cluster",
                                         label = "Select a year",
                                         choices = c(2001:2020),
                                         selected = "2010"),
                             selectInput("k_cluster",
                                         label = "Select number of classes",
                                         choices = c(3,5,9),
                                         selected = "3"))
                           , # sidebarPanel
                           mainPanel(plotOutput("map4")
                           )
                  ),
                  tabPanel("Clustering areas",
                           sidebarPanel(
                             helpText("Select a Year to see The Map of Clustering in Adminstrative areas"),
                             selectInput("year1_clusterarea",
                                         label = "Select a year",
                                         choices = c(2001:2020),
                                         selected = "2010"),
                             selectInput("k_clusterarea",
                                         label = "Select number of classes",
                                         choices = c(3,5,9),
                                         selected = "3"))
                           , # sidebarPanel
                           mainPanel(plotOutput("map5")
                           )
                  ),
                  navbarMenu("More", icon = icon("info-circle"),
                             tabPanel("About", fluid = TRUE,
                                      fluidRow(
                                        column(6,
                                               #br(),
                                               h4(p("About the Project")),
                                               h5(p("This project is intended to show in easier way spatial changes of the Korean Peninsula in XXI century in various ways. Here a prospective student, or anyone else with an interest in this certain topic can find various maps of lanscape changes, for example, comaprison of different years, clustering of different groups and as well land cover in different years of XXI century.")),
                                               br(),
                                               h5(p("Why Koreas? I'm interested in discovering the Korean Peninsula since 2016 and it started because of their music. The project began as an attempt to combine my interest together: geography, graphic design, programming and Koreas.")),
                                               br(),
                                               h5(p("I hope you find it interesting and/or useful.  Any comments or questions are welcome at vicgrabarz@gmail.com"),
                                                  )



                                        ),
                                        column(6,

                                               h4(p("About the Author")),
                                               h5(p("I'm Victoria Grabarz and I'm a 22 years old student of Geoinformation. My degree of engineer I'm gonna finish with project about the Korean Peninsula. "),),
                                               br(),
                                               h5(p("I find the Korean Peninsula interesting, because of different culture and, as well differences between South and North Korea. My biggest dream is to visit South Korea and to see their landscape and architecture, which is even from XII century. Their temples are breath-taking and I cannot imagine how beautiful it is to see with your own eyes.
                                                    However, below I wanna show you how beautiful are mountines there, as well as architecture. It is Bukhansan (북한산) mountain, sometimes called the “lungs of Seoul” in Seoul, South Korea."))
                                              , br(),
                                               img(src = "https://media.cnn.com/api/v1/images/stellar/prod/170418155509-bukhansan.jpg?q=x_2,y_105,h_1150,w_2043,c_crop/h_540,w_960", height = "200px"),
                                               h6(p("Source: Courtesy dconvertini/Creative commons/Flickr"))
                                               )
                                      ),
                                      br(),
                                      hr(),
                                      h5(""),
                                      h6(
                                        p(" ",
                                          a("",
                                            href = ""))),
                                      h6(
                                        p(" ",
                                          a("",
                                            href = ""))),
                                      h5("Built with",
                                         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                         "by",
                                         img(src = "https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Flat.png", height = "30px"),
                                         ".")
                             ))

                )
)

server <- function(input, output) {

  rr = reactive({
    which(c(2001:2020) == input$year)
  })

  selectedData1 <- reactive({
    select("2001", "2002", "2003","2004","2005",
           "2006", "2007","2008","2009","2010",
           "2011","2012","2013","2014","2015",
           "2016","2017", "2018", "2019","2020") %>%
      filter(selectedData1()$year1 %in% input$year1,
             selectedData1()$year2 %in% input$year2) %>%
      filter(year1 = input$year1[1]) %>%
      filter(year2 = input$year2[2])})

  selectedData2 <- reactive({
    select("3", "5", "9") %>%
      filter(selectedData2()$k %in% input$k) %>%
      filter(k = input$k[1])
  })

  output$map <- renderLeaflet({
    map = leaflet()
    map = addTiles(map) %>% addRasterImage(all_rc[[rr()]],
                                           col = colorFactor(classcolor, domain = 1:10))
    map = setView(map, lng = 127.01, lat= 38.01, zoom = 6)
    map
  })

  rr2 = reactive({
    my_compare = terra::rast(paste0("~/Korea/compare/", input$year1, "_", input$year2, ".tif"))
  })

  output$map2 <- renderPlot({
    terra::plot(rr2(), col=viridis(4), breaks = my_breaks,
                legend.outside = TRUE, plg = list(title = "Magnitude of change"))
  })

  rr3 = reactive({
    my_compare3 = sf::read_sf(paste0("~/Korea/compare_poly/", input$year1comp, "_", input$year2comp, ".gpkg"))
  })

  output$map3 <- renderPlot({
    ggplot() + geom_sf(data = rr3(), aes(fill = dist)) +
      scale_fill_binned_sequential(palette = "Viridis", name = "Magnitude of change",
                                   breaks = my_breaks) +
      theme(legend.position = "right")
  })
  rr4 = reactive({
    my_compare4 = sf::read_sf(paste0("~/Korea/cluster/", input$year1cluster, "_", input$k_cluster, ".gpkg"))
  })

  output$map4 <- renderPlot({
    ggplot() + geom_sf(data = rr4(), aes(fill = as.factor(clust))) +
      scale_fill_discrete_qualitative(palette = "Set2", name = "Classes")
      })

  rr5 = reactive({
    my_compare5 = sf::read_sf(paste0("~/Korea/cluster_poly/", input$year1_clusterarea, "_", input$k_clusterarea, ".gpkg"))
  })

  output$map5 <- renderPlot({
    ggplot() + geom_sf(data = rr5(), aes(fill = as.factor(clust)))+
      scale_fill_discrete_qualitative(palette = "Set2", name = "Classes")
  })
}

shinyApp(ui = ui, server = server)
