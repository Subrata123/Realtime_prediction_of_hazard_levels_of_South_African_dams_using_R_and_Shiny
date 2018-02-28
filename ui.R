options(shiny.maxRequestSize = 9*1024^2)
# options(shiny.error = browser)

if (!require("pacman")) install.packages("pacman")

pacman::p_load(shiny,
               shinydashboard,
               tableHTML,
               data.table,
               Amelia,
               measurements,
               Hmisc,
               ggmap,
               ggplot2,
               lattice,
               dplyr,
               ggvis,
               dplyr,
               caret,
               C50,
               rpart,
               randomForest)


shinyUI(
  
  fluidPage(
    
    #the below chunk of code changes the look of the sliderinput
    tags$style(make_css(list('.irs-bar',
                             c('border-top', 'border-bottom', 'background'),
                             rep('#0147FA', 3)),
                        list('.irs-bar-edge',
                             c('background', 'border'),
                             c('#0147FA', '100px !important')),
                        list('.irs-single',
                             'background',
                             'black'))),
    
    tags$style(make_css(list('.irs-bar', 
                             c('border-top', 'border-bottom', 'background'), 
                             rep('red', 3)),
                        list('.irs-bar-edge',
                             c('background', 'border'),
                             c('red', '0px !important')),
                        list('.irs-single',
                             'background',
                             'red'))),
            
            
    titlePanel(
      div(
        
        h1("Expert System to Analyze and Predict South African Dams Hazard Level",
           style = "color: #CD2626")
        )
      ),
    
    sidebarLayout(
      
      sidebarPanel(
        
        #The tag helps in changing overall background colour of the app 
        (tags$head
         (
           tags$style("body {background-color: #d0d2ff; }")
         )
        ),
        
        helpText(h5("This web application is created by Dr. Satyakama Paul. 
                    Reach out to me at: satyakama.paul@gmail.com",
                    style = "color: #551A8B")),
        
        
        br(),
        
        helpText(h4("Fill in your parameter values to find a dam's hazard level",
                    style = "color: #000000")),
        
        #User input 1
        sliderInput(inputId = "Water.management.area",
                    label = "Water management area (codified categories)",
                    min = 1,
                    max = 9,
                    value = 5,
                    step =1),
        
        #User input 2
        sliderInput("Distance.from.Town", 
                    "Distance from Town",
                    min = 0,
                    max = 500, 
                    value = 250,
                    step = 1),
        
        #User input 3
        selectInput(inputId = "Region.Code",
                    label = "Region",
                    choices = c("EASTERN CAPE" = "EASTERN CAPE",
                                "FREE STATE" = "FREE STATE",
                                "GAUTENG" = "GAUTENG",
                                "KWAZULU NATAL" = "KWAZULU NATAL",
                                "LIMPOPO" = "LIMPOPO",
                                "MPUMALANGA" = "MPUMALANGA",
                                "NORTH WEST" = "NORTH WEST",
                                "NORTHERN CAPE" = "NORTHERN CAPE",
                                "WESTERN CAPE" = "WESTERN CAPE")),
        
        #User input 4
        
        sliderInput(inputId = "Completion.Year",
                    label = "Completion year",
                    min = 1840,
                    max = 2025,
                    value = 2005,
                    step = 1),
        
        #User input 5
        
        selectInput(inputId = "Wall.type",
                   label = "Wall type",
                   choices = c("ARCH" = "ARCH",
                               "ARCH & BUTTRESS" = "ARCH & BUTTRESS",
                               "ARCH & EARTHFILL" = "ARCH & EARTHFILL",
                               "ARCH & GRAVITY" = "ARCH & GRAVITY",
                               "ARCH GRAVITY" = "ARCH GRAVITY",
                               "BUTTRESS" = "BUTTRESS",
                               "BUTTRESS & EARTHFILL" = "BUTTRESS & EARTHFILL",
                               "BUTTRESS & GRAVITY " = "BUTTRESS & GRAVITY ",
                               "BUTTRESS & REINFORCED CONCRETE SERVICE RESERVOIR " = "BUTTRESS & REINFORCED CONCRETE SERVICE RESERVOIR ",
                               "CLAY CORE ROCKFILL" = "CLAY CORE ROCKFILL",
                               "CONCRETE" = "CONCRETE",
                               "CONCRETE BUTTRESS" = "CONCRETE BUTTRESS ",
                               "CONCRETE GRAVITY" = "CONCRETE GRAVITY",
                               "CONCRETE LINED EXCAVATION EMBANKMENT" = "CONCRETE LINED EXCAVATION EMBANKMENT",
                               "CONCRETE MASONRY" = "CONCRETE MASONRY",
                               "CONCRETE MASS GRAVITY WITH ROCKFILL ABUTMENTS" = "CONCRETE MASS GRAVITY WITH ROCKFILL ABUTMENTS",
                               "EARTFILL" = "EARTFILL",
                               "EARTH-ROCKFILL & GRAVITY" = "EARTH-ROCKFILL & GRAVITY",
                               "EARTH SERVICE RESERVOIR" = "EARTH SERVICE RESERVOIR",
                               "EARTH & ROCKFILL" = "EARTH & ROCKFILL",
                               "EARTHFIL" = "EARTHFIL",
                               "EARTHFILL" = "EARTHFILL",
                               "EARTHFILL & ARCH & REINFORCED CONCRETE" = "EARTHFILL & ARCH & REINFORCED CONCRETE",
                               "EARTHFILL & CONCRETE" = "EARTHFILL & CONCRETE",
                               "EARTHFILL & GRAVITY" = "EARTHFILL & GRAVITY",
                               "EARTHFILL & GRAVITY & ROCK FILL" = "EARTHFILL & GRAVITY & ROCK FILL",
                               "EARTHFILL & MULT ARCH" = "EARTHFILL & MULT ARCH",
                               "EARTHFILL & MULTI-ARCH" = "EARTHFILL & MULTI-ARCH",
                               "EARTHFILL & REINFORCED CONCRETE" = "EARTHFILL & REINFORCED CONCRETE",
                               "EARTHFILL & ROCKFILL" = "EARTHFILL & ROCKFILL",
                               "EARTHFILL/ ROCKFILL WITH CONCRETE CAPPING" = "EARTHFILL/ ROCKFILL WITH CONCRETE CAPPING",
                               "EATHFILL" = "EATHFILL",
                               "GRAVITY" = "GRAVITY",
                               "GRAVITY ARCH" = "GRAVITY ARCH",
                               "GRAVITY BUTTRES" = "GRAVITY BUTTRES",
                               "GRAVITY BUTTRESS" = "GRAVITY BUTTRESS",
                               "INDUSTRIAL RESIDUE DEPOSIT" = "INDUSTRIAL RESIDUE DEPOSIT",
                               "MASONRY GRAVITY" = "MASONRY GRAVITY",
                               "MINE RESIDUE DEPOSIT" = "MINE RESIDUE DEPOSIT",
                               "MULTI-ARCH" = "MULTI-ARCH",
                               "MULTI ARCH" = "MULTI ARCH",
                               "MULTIPLE ARCH BUTTRESS DAM" = "MULTIPLE ARCH BUTTRESS DAM",
                               "REINFORCED CONCRETE RESERVOIR" = "REINFORCED CONCRETE RESERVOIR",
                               "ROCKFILL" = "ROCKFILL",
                               "ROOFED CONCRETE LINED EARTHFILL" = "ROOFED CONCRETE LINED EARTHFILL",
                               "UNKNOWN" = "UNKNOWN",
                               "ZONED EARTHFILL" = "ZONED EARTHFILL")),
        
        #User input 6
        
        sliderInput("Wall.height", 
                    "Wall height",
                    min = 0,
                    max = 120, 
                    value = 60,
                    step = 1),
        
        #User input 7
        
        sliderInput("Crest.length", 
                    "Crest length",
                    min = 0,
                    max = 1000, 
                    value = 500,
                    step = 1),
        
        #User input 8
        
        sliderInput("Capacity", 
                    "Capacity",
                    min = 0,
                    max = 7000000, 
                    value = 3500000,
                    step = 1),
        
        #User input 9
        sliderInput("Surface.area", 
                    "Surface area",
                    min = 0,
                    max = 100000, 
                    value = 50000,
                    step = 1),
        
        #User input 10
        
        radioButtons(inputId = "Size",
                     label = "Size",
                     choices = c("Small" = "Small",
                                "Medium" = "Medium",
                                "Large" = "Large"),
                     inline = TRUE),
        
        #User input 11
        
        radioButtons(inputId = "Category",
                     label = "Category",
                     choices = c("1" = "1",
                                 "2" = "2",
                                 "3" = "3"),
                     inline = TRUE),
        
        #User input 12
        radioButtons(inputId = "Sector",
                     label = "Sector",
                     choices = c("A" = "A",
                                 "B" = "B",
                                 "M" = "M",
                                 "O" = "O",
                                 "S" = "S",
                                 "W" = "W"),
                     inline = TRUE),
        
        #User input 13
        sliderInput("latitide.dec", 
                    "Latitude (in degree)",
                    min = 20,
                    max = 36, 
                    value = 28,
                    step = 0.1),
        
        #User input 14
        sliderInput("longitude.dec", 
                    "Longitude (in degree)",
                    min = 15,
                    max = 34, 
                    value = 24,
                    step = 0.1),
        
        selectInput(inputId = "newProvincecode",
                    label = "Province",
                    choices = c("EASTERN CAPE" = "E.CP",
                                "FREE STATE" = "F.ST",
                                "GAUTENG" = "GAU",
                                "KWAZULU NATAL" = "K.ZN",
                                "LIMPOPO" = "LIM",
                                "MPUMALANGA" = "MPU",
                                "NORTH WEST" = "N.WE",
                                "NORTHERN CAPE" = "N.CP",
                                "WESTERN CAPE" = "W.CP",
                                "Unknown" = "Unknown")),
        
        width = 3
         
      ),
      
      mainPanel(
        
        tabsetPanel(
          type = "pills",
          navbarPage(
            h4("Tabs",
               align = "left",
               style = "color: black"),
            
            navbarMenu(
              
              h4("Background",
                 align = "left",
                 style = "color: #008000"),
              
              tabPanel("Contents",
                       helpText(h3("What is the app about?",
                                   align = "left",
                                   style = "color: #EF3E5B")),
                       
                       helpText(h5("South African dams have three levels of hazards - low, significant 
                                   and high. Dams are characteristed by multiple different 
                                   parameters and each parameter can assume multiple values. Thus hazard 
                                   prediction of a dam is a complex nonlinear decision.",
                                   style = "color: #000000")),
                       
                       helpText(h5("_________________________________________________________________________________________________________________________________________________________________________" ,
                                   style = "color: #660000")),
                       
                       helpText(h3("What do we provide?",
                                   align = "left",
                                   style = "color: #03396c")),
                       
                       helpText(h5("This web application is an AI based Expert System to
                                   visualize and predict the hazard level of South African dams.",
                                   style = "color: #000000")),
                       
                       helpText(h5("_________________________________________________________________________________________________________________________________________________________________________" ,
                                   style = "color: #660000")),
                       
                       helpText(h3("How do we help?",
                                   align = "left",
                                   style = "color:  #028900")),
                       
                       helpText(h5("[1] The app is INTERACTIVE in nature. You input the value of dam parameters,
                                   and the app will tell its hazard level.",
                                   style = "color: #000000")),
                       
                       br(),
                       
                       helpText(h5("[2] The app reduces COST of survey. It can be operated from any place. 
                                   By filling in the parameter values, it predicts the hazard
                                   level of a dam.",
                                   style = "color: #000000")),
                       
                       br(),
                       
                       helpText(h5("[3] It reduces the DEPENDENCE on skilled surveyors. ",
                                   style = "color: #000000")),
                       
                       br(),
                       
                       helpText(h5("[4] We provide multiple VISUALIZATIONS to better understand
                                   the states of the dams, their geographical locations etc. ",
                                   style = "color: #000000")),
                       
                       br(),
                       
                       helpText(h5("[5] The expert system is ROBUST and can handle outliers and wrong inputed 
                                   values (to certain extent). ",
                                   style = "color: #000000")),
                       
                      
                       helpText(h5("_________________________________________________________________________________________________________________________________________________________________________" ,
                                   style = "color: #660000")),
                       
                       helpText(h3("How good is our prediction?",
                                   align = "left",
                                   style = "color: 	#0892d0")),
                       
                       helpText(h5("We have achieved a very high level of overall predictive 
                                   accuracy (above 93%).",
                                   style = "color: #000000")),
                       
                       helpText(h5("_________________________________________________________________________________________________________________________________________________________________________" ,
                                   style = "color: #660000"))
                       
                       ),
              
              tabPanel("Software architecture",
                       img(src='architecture.png', 
                           align = "centre",
                           width="900",
                           height="600"))      
               
            ),
            
            navbarMenu(
              
              h4("View of Raw Data",
                 align = "left",
                 style = "color: #D2691E"),
              
              tabPanel("First couple of rows",
                       dataTableOutput("o.RawData")),
              
              tabPanel("Volume or Dimensionality",
                       valueBoxOutput("o.Rows"),
                       valueBoxOutput("o.Columns"),
                       valueBoxOutput("o.Total.No.Dams"))
              
            ),
            
            navbarMenu(
              
              h4("In your historical data",
                 align = "left",
                 style = "color: #8E388E"),
              
              tabPanel("View of statistically significant data",
                       dataTableOutput("o.junk1")),
              
              tabPanel("Characteristics of the Dams ",
                       plotOutput("o.DamCharacteristics")),
              
              tabPanel("Geography",
                       plotOutput("o.Geography")),
              
              tabPanel("Time related plot",
                       plotOutput("o.Completiondate.Hazard"))
             
            ),
            
            navbarMenu(
              
              h4("In your historical data - model testing",
                 align = "left",
                 style = "color: #292421"),
              
              tabPanel("How complex is the decision boundary",
                       plotOutput("o.parcoordplot")),
              
              tabPanel("How accurate is the prediction",
                       verbatimTextOutput("o.testingmodel"))
              
              
            ),
            
            navbarMenu(
              
              h4("Final prediction based upon user's input data",
                 align = "left",
                 style = "color: #8B0000"),
              
              tabPanel("Find the hazard level ",
                       verbatimTextOutput("o.final.pred"))
              
              
            )
            
          )
          
        ),
        width = 9
      )
    )
  )
)