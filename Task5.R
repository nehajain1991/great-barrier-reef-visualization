#Task 5

library(shiny)
library(leaflet)
library(ggplot2)

myData <- read.csv("assignment-02-data-formated.csv")
myData$value = as.numeric(sub("%", "", myData$value))

myData$location <- reorder(myData$location, myData$latitude)

pal <- colorFactor(c("blue", "red", "green", "black", 
                     "orange", "purple", "pink","grey"),
                   domain = myData$location)

server <- function(input, output) {

  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # Leaflet map as in Task 4
  output$map <- renderLeaflet({
    leaflet(data = myData) %>% addTiles() %>%
      addCircleMarkers(
        ~longitude, 
        ~latitude,  
        color = ~pal(location), radius = 5, label = ~location,
        labelOptions = labelOptions(noHide = TRUE,
                                    offset=c(0,-12), textOnly = TRUE)) %>%
      addLegend("bottomright", pal = pal, values = ~location, title = "Location legend",
                opacity = 1
      ) 
    })
  
  #fetching the selected location marker
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
    
  })
  
  smoother <- reactive({input$smoother})
  
  output$plot=renderPlot({
    #storing the location longitude and latitude of clicked marker
    site_lat =data_of_click$clickedMarker$lat 
    site_lng =data_of_click$clickedMarker$lng
    
    #checking if the marker is clicked, done for displaying the first time the app is run
    if(is.null(site_lat)){
     
      df_subset <- reactive({
         myData[myData$coralType == input$coral_type,]
        
      })
      output$caption <- renderText(paste(input$coral_type, input$smoother, sep = '~'))
      }
       
       else{
         #subset of data taken based on the filer values of coral type and location
         df_subset <- reactive({
           subset(myData, ((myData$coralType == input$coral_type) & 
                             (myData$latitude == site_lat) & (myData$longitude == site_lng) ))

         })
         #fetched the location id of the marker
         new = myData[(myData$coralType == input$coral_type) & 
                        (myData$latitude == site_lat) & (myData$longitude == site_lng),]
         site = unique(new[,"location"])
        #caption to print on ui
         output$caption <- renderText(paste(input$coral_type, input$smoother,site, sep = '~'))        
       }
    
    #handling the enpty facets of missing values
    validate(
      need(nrow(df_subset())!=0 , "The selection has missing data, please select again")
    )
    
    ggplot(df_subset(), aes(x =year ,y = value, group = 1)) + 
      geom_point(color = "red", size =5) +  
      theme(axis.text=element_text(size=7),
            axis.title=element_text(size=8,face="bold"))+
      
      scale_y_continuous(breaks = seq(0, 100, by = 25))+
      geom_smooth(aes(group = 1),
                  method = smoother(),
                  color = "black")+
      facet_grid(location~coralType)+ 
      theme(strip.text.x = element_text(size = 8),
            strip.text.y = element_text(size = 8))    

  
}
)
}


ui <- fluidPage(
  headerPanel("Variation of Bleaching for Corals over years and locations"),
  h3(textOutput("caption")),
 
  sidebarLayout(
    sidebarPanel(
      selectInput("coral_type", "Coral Type:", 
                  c("Blue Corals" = "blue corals", 
                    "Hard Corals" = "hard corals",
                    "Sea Fans" = "sea fans",
                    "Soft Corals" = "soft corals",
                    "Sea Pens" = "sea pens"
                    )
      ),
  
      selectInput("smoother", "Smoother Type:", 
                  c("lm" = "lm", 
                    "glm" = "glm",
                    "auto" = "auto",
                    "gam" = "gam",
                    "loess" = "loess"))
      
    ),
  
    mainPanel(
      leafletOutput("map"),# display Leaflet
      plotOutput('plot')) #display bleaching plot
  )
)

shinyApp(ui = ui, server = server)