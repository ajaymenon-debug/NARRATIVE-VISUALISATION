library(shiny)
library(leaflet)
library(ggplot2)
library(shinythemes)
library(lattice)
library(timevis)
library(corrplot)
library(dplyr)
library(ggcorrplot)
library(plotly)
library(shinyWidgets)

#loading the dataset
Data <- read.csv("database.csv")

#converting year field to integer
Data$Accident.Year <-  as.integer(Data$Accident.Year)

#ui part of the program
ui <- fluidPage(
  #them for the slider for map
  chooseSliderSkin("Modern"),
  #loading bootstrap file
  theme = "bootstrap.css",
  #CSS file
  includeCSS("www/styles.css"),
  
  navbarPage(
    "Oil Accidents around the world",
    id = "main_navbar",
    #Tab for showing maps
    tabPanel(
      "Maps",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          uiOutput("selected_stops_UI"),
          actionButton("bus_refresh", "Refresh"),
          
          br(),
          br(),
          h3("Press Play"),
          #Slider for selecting the year
          sliderInput(inputId = "any", "Select the year Range", min = 2010, max =2017,step = 1
                      , sep = "", value=2010
                      ,animate =animationOptions(interval = 1500, loop = TRUE,
                                                 playButton = icon('play', "fa-3x"),pauseButton = icon('pause', "fa-3x"))),
          

          br(),
          br(),
          # More info having the link to the source information
          h3("More Info"),
          a(
            href = "https://www.phmsa.dot.gov/data-and-statistics/pipeline/pipeline-incident-20-year-trends",
            "More info about oil pipepline accidents",
            target = "_blank"
          ),
          br(),
          br()
        ),
      
  mainPanel( 
    tabsetPanel(
    tabPanel(
      "Map",
        #Display the year on the main panel
       textOutput("show_year"),
                  tags$head(tags$style("#show_year{color: GREEN;
                                 font-size: 22px;
                                 line-height: 20px;
                                 }"
                                    )
                          ),
          # Display the map on the main panel
          leafletOutput("map", height=500),
                  br(),
                  textOutput("selected_var"),
                  tags$head(tags$style("#selected_var{color: Green;
                                 font-size: 20px;
                                 line-height: 20px;
                                 }"
                                )
                            )
                  ),
      #Disply more infor about the map by clicking this tab
    tabPanel(
      "Map Info",

                   h3("More Info about the map"),
                   br(),
                   br(),
                   h4("Things which can be derived from this dashboard"),
                   br(),
                   h5("1. Find the locations where maximum accidents have occured"),
                   br(),
                   h5("2. Find the locations where maximum accidents have occured"),
                   br(),
                   h5("3. Understand what is main cause for Oil Pipeline Accidents "),
                   br(),
                   h5("4. Understand when and where the maximum cost is spend during the accidents "),
                   br(),
                    br(),
                    h4(" The map shows the locations where the accidents have occured from the years 2010 till 2017."),
                   br(),
                      h4(" Clicking the play button will show us all the accidents thathave occured from the years 2010 till 2017 automatically"),
                    br(),
                    h4( "Clicking each of this circle will give more information about the accident."),
                    br(),
                        h4("The Radius of the circle is preportional to the total cost incured during these accidents. Each colour
                       of the represnt each cause of the accidents. There are mainly 7 causes")
        
          )
        
        )
      
      )
    )
    ),
  # tab to show the scatter plot and the correlation matrix
    tabPanel(
      "Plotly",
      h3("Scatter plot showing Total cost spend and net loss of Oil"),
      br(),
      br(),
      plotlyOutput('plot',height = 500),
      br(),
      br(),
      h3("Correlation plot between important variables"),
      sidebarPanel(
        width = 4,
        h4("This is level plot for the correlation matrix.
      As the marker on the right shows, the value 0 means it has very less correlation and value one means it has maximum correlation
From this, we can understand that the total cost and Year are very small, nearly 0 while there is a small correlation between 
total cost and Unintentional Release. It has a moderate amount of   total cost and Liquid Recovered.")
        ),
       mainPanel(
                  
                  plotOutput("distPlot", height= 500)
           )

      ),
  
  # Final tab to show more details of this dashboard
    tabPanel(
      "About",
      fluidRow(
        # About - About Me - start ------------------------------------------------
        h2("Oil Pipeline Accidents"),
        br(),
        h4("The number of Oil Pipeline Accidents around the world are too high and we hear about so much losses due to these accidents. "),
        br(),
        h4("Huge amount of money is lost in each of these accidents and there are even fatalities that occur during these accidents."),
        br(),
        h3("Data source:"),
        br(),
        h4("I have taken a database includes a record for each oil pipeline leak or spill reported to the Pipeline
        and Hazardous Materials Safety Administration from 2010 till 2017. "),
        br(),
        h4("These records include the
        incident date and time, operator and pipeline, cause of incident, type of hazardous liquid and
        quantity lost, injuries and fatalities, and associated costs"),
        br(),
        h3("Data Source URL:"),
        br(),
         a (href = "https://www.kaggle.com/usdot/pipeline-accidents","•https://www.kaggle.com/usdot/pipeline-accidents",target = "_blank"),
        br(),
        br(),
        a (href = "https://www.phmsa.dot.gov/data-and-statistics/pipeline/data-and-statistics-overview", "•https://www.phmsa.dot.gov/data-and-statistics/pipeline/data-and-statistics-overview",target = "_blank"),
        br()
      
              )
          
          )
      
        )
      
)

#Server code of the dashboard
server<-shinyServer(function(input, output,session) {

  
  #show intro message about the dashboard and press CLICK TO CONTINUE to start
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("intro_text.html"),
      easyClose = TRUE,
      footer = tagList(
        actionButton(inputId = "intro", label = "CLICK TO CONTINUE")
      )
    ))
  })
  # remove this message once click to continue is pressed
  observeEvent(input$intro,{
    removeModal()
  })
  
  observeEvent(input$refresh, {
    shinyjs::js$refresh()
  })
  map = leaflet::createLeafletMap(session, 'map')
  # To make the Accinadent.Latitude and Accident.Longitude into numeric field just to make sure everything goes correct
  Data$Accident.Latitude <-  as.numeric(Data$Accident.Latitude)
  Data$Accident.Longitude <-  as.numeric(Data$Accident.Longitude)
  #filter those data with Accident.Latitude NA
  Data=filter(Data, Accident.Latitude != "NA")

  #To display more information about the accident when each circle is clicked
  Data <- mutate(Data, cntnt=paste0('<strong>Cause Category: </strong>', Cause.Category,
                                    '<br><strong>Operator Name:</strong> ', Operator.Name,
                                    '<br><strong>Pipeline Location:</strong> ', Pipeline.Location,
                                    '<br><strong>Pipeline Type:</strong> ',Pipeline.Type,
                                    '<br><strong>Liquid Type:</strong> ',Liquid.Type,
                                    '<br><strong>City:</strong> ',Accident.City,
                                    '<br><strong>County:</strong> ',Accident.County,
                                    '<br><strong>Cost Spend:</strong> ',All.Costs
                                    )) 
  
  filtered <- reactive({
    Data[Data$Accident.Year>= input$range[1] & Data$Accident.Year<=input$range[2], ]
  })
  
  Data_filt = Data %>% filter(All.Costs < 10000)
  
  #Filter to select the data based on the year value of the year slider
  year_data <- reactive({
    
    Data_filt[Data_filt$Accident.Year == input$any,]
  })

#Color pallet which represent each cause of the oil accident. These colours are selected considering the colour blind people as well
  pal <- colorFactor(pal = c("#E66100", "#5D3A9B", "#994F00","#006CD1","#1AFF1A","#4B0092","#FEFE62"), domain = c("ALL OTHER CAUSES", "CORROSION", "EXCAVATION DAMAGE","INCORRECT OPERATION","MATERIAL/WELD/EQUIP FAILURE","NATURAL FORCE DAMAGE","OTHER OUTSIDE FORCE DAMAGE"))
  
  #Output for the map
  output$map <- renderLeaflet({
      leaflet(Data) %>% 
      addTiles() %>%
      setView(lat = 38.6707,lng = -97.78123, zoom = 4.2) %>%
      addCircleMarkers(
        lat = ~Data$Accident.Latitude, 
        lng = ~Data$Accident.Longitude,
        opacity = 1.5,
        popup = ~as.character(cntnt),
        radius = ~Data$All.Costs / 700,
        stroke = FALSE,
        fillOpacity = 0.8,
        color = ~pal(Data$Cause.Category))%>%
        addLegend(pal=pal,values=Data$Cause.Category,opacity=0.5, na.label = "Not Available",title = "ROOT CAUSE FOR ACCIDENTS")
      
  })
  
  observeEvent(input$any, {
    leafletProxy("map", data = year_data()) %>%
      clearMarkers()%>%
      addCircleMarkers(
        lat = ~year_data()$Accident.Latitude, 
        lng = ~year_data()$Accident.Longitude,
        opacity = 1.5,
        radius =~year_data()$All.Costs /700,
        popup = ~as.character(cntnt), 
        stroke = FALSE,
        fillOpacity = 0.8,
        color = ~pal(Data$Cause.Category))
      
  })
  
  #Selecting the fields which are neccessary for the correlation matrix
  Data_new = Data %>% 
    select(All.Costs, Net.Loss..Barrels., Liquid.Recovery..Barrels., Intentional.Release..Barrels., Unintentional.Release..Barrels., Accident.Year)
  
  #Displaying the correlation matrix
  output$distPlot <- 
    renderPlot({
    levelplot(cor(Data_new[sapply(Data_new, is.numeric)]), scales = list(x = list(rot = 90)))
    })
  
# Data filter for scatter plot
  Data_fil = Data %>% filter(All.Costs < 10000)
  year_data2 <- reactive({
    
    Data_fil[Data_fil$Accident.Year == input$any,]
  })

#Output for plotly scatter plot showing the scatter plot between net loss of barrels vs Total cost over each year
output$plot <- renderPlotly( 
  p <- Data_fil %>%
    plot_ly(
      x = ~Net.Loss..Barrels., 
      y = ~All.Costs, 
      size = 12, 
      text = ~Cause.Subcategory, 
      hoverinfo = "text"
    )  %>%
    layout( xaxis = list( type = "log" ) ,legend=list(title=list(text='<b> Cause of Oil Accident </b>'))) %>%
    add_markers(color = ~Cause.Category,  frame = ~Accident.Year, ids = ~Cause.Subcategory,) %>%
    animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
    animation_button(
      x = 1, xanchor = "right", y = 0, yanchor = "bottom"
    ) %>%
    animation_slider(
      currentvalue = list(prefix = "YEAR ", font = list(color="green"))
    )
  
)
  #to display the year in the main panel
  output$show_year <- renderText({ 
    input$any
  })
  
  
  # Output to display the text below the Map 
  output$selected_var <- renderText({ 
    if(input$any == 2010)
          {
      "Majority of the cause for accidents are MATERIAL/WELD/EQUIP FAILURE. Highest cost spend is by PLAINS PIPELINE, L.P. company and the root cause was CORROSION which happend in Scurry "
        }
    else if (input$any == 2011)
    {
      "Highest cost spend is by the company BUCKEYE PARTNERS, LP and the root cause was  NATURAL FORCE DAMAGE which happend in COOK. 
        Majority of the root cause for all the accidents are still MATERIAL/WELD/EQUIP FAILURE"
    }
    else if (input$any == 2012) 
    {
      "Accidents evenly spread accross the Map. Majority of the cause is MATERIAL/WELD/EQUIP FAILURE, but there are many cases for Corrosion as well
      Highest cost spend is by the company SHELL PIPELINE CO., L.P. and the root cause was  MATERIAL/WELD/EQUIP FAILURE which happend in ST. CHARLES "
    }
    else if (input$any == 2013)
    {
      "Majority of the cause for accidents are still MATERIAL/WELD/EQUIP FAILURE.
      Highest cost spend is by the company KINDER MORGAN PIPELINES (USA) INC and the root cause was  MATERIAL/WELD/EQUIP FAILURE which happend in ST. CHARLES "
    }
    else if (input$any == 2014)
    {
      "Majority of the cause is still MATERIAL/WELD/EQUIP FAILURE.
      Highest cost spend is by the company KINDER MORGAN PIPELINES (USA) INC and the root cause was 
      BUCKEYE PARTNERS, LP which happend in MIDDLESEX"
    }
    else if (input$any == 2015)
    {
      "There are quite a lot accidents toward the western side
      Majority of the cause is still MATERIAL/WELD/EQUIP FAILURE.
      Highest cost spend is by the company ENTERPRISE PRODUCTS OPERATING LLC and the root cause was 
      BUCKEYE PARTNERS, LP which happend in JEFFERSON"
    }
    else if (input$any == 2016)
    {
      "Accidents evenly spread accross the Map. Majority of the cause is MATERIAL/WELD/EQUIP FAILURE,
      Highest cost spend is by the company  KINDER MORGAN CO2 CO. LP and the root cause was  MATERIAL/WELD/EQUIP FAILURE which happend in ST. CHARLES "
    }
    else if (input$any == 2017)
    {
      "Very few instance available. Couldnt provide much insights"
    }
  })
})

shinyApp(ui, server)
