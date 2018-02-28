options(shiny.maxRequestSize = 9*1024^2)
options(shiny.error = browser)

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

shinyServer(function(input, output){
  
  masterData <- reactive({
    
    setwd("C:/Users/satyakama.paul/Documents/git_test")
    
    masterData <- read.csv("mother_data.csv",
                           header = TRUE,
                           sep = ",",
                           stringsAsFactors = TRUE,
                           na.strings = c("",
                                          "-",
                                          "--",
                                          "---",
                                          "----",
                                          "-----",
                                          "/",
                                          " ",
                                          ".",
                                          "NA",
                                          "OFS",
                                          "UNKN",
                                          "warm",
                                          "#NAME?"))

    })
  
  #Raw data visualization -----------------------------
  
  output$o.RawData <- renderDataTable({
    
    if(is.null(masterData())){return()}
    else
      masterData()
  })
  
  output$o.Rows <- renderValueBox({
    
    valueBox("Total number of rows:",
             dim(masterData())[1],
             icon = icon("info"))
  })
  
  output$o.Columns <- renderValueBox({
    
    valueBox("Total number of columns:",
             dim(masterData())[2],
             icon = icon("info"))
  })
  
  output$o.Total.No.Dams <- renderValueBox({
    
    valueBox("Total number of dams",
             length(table(masterData()$Name.of.dam)),
             icon = icon("info"))
  })
  
  #Dropping variables/columns that dont cater to a minimum number of NA values-----------------------------
  masterData.ReducingVars <- reactive({
    
    masterData.ReducingVars <- masterData()[ lapply( masterData(),
                                                     function(x) sum(is.na(x)) / length(x) ) < 0.1 ]
    
    
  })
  
  
  #Complete cases-----------------------------
  
  masterData.Complete <- reactive({
    
    masterData.Complete <- masterData.ReducingVars()[complete.cases(masterData.ReducingVars()), ]
    
    #Removing statistically non significant variables
    
    masterData.Complete <- subset(masterData.Complete,
                                  select = -c(No.of.dam,
                                              Name.of.dam,
                                              Quaternary.Drainage.Area,
                                              Town.nearest,
                                              Name.of.farm,
                                              River.or.Watercourse,
                                              Spillway.Type,
                                              Purpose,
                                              Owner.Name,
                                              Registration.date,
                                              Classification.date,
                                              Number.Last.DSI))

    #Working with latitude and longitude
    #Latitude
    masterData.Complete$Lat.sec <- round(masterData.Complete$Lat.sec)
    
    latitude.dms <- paste(masterData.Complete$Latitude.deg,
                          masterData.Complete$Lat.min,
                          masterData.Complete$Lat.sec,
                          sep = " ")

    latitude.dec <- conv_unit(latitude.dms,
                              "deg_min_sec",
                              "dec_deg")

    latitude.dec <- as.numeric(latitude.dec)

    latitude.dec <- latitude.dec * (-1)
    
    #Longitude
    masterData.Complete$Long.sec <- round(masterData.Complete$Long.sec)

    longitude.dms <- paste(masterData.Complete$Longitude.deg,
                           masterData.Complete$Long.min,
                           masterData.Complete$Long.sec,
                           sep = " ")

    longitude.dec <- conv_unit(longitude.dms,
                               "deg_min_sec",
                               "dec_deg")

    longitude.dec <- as.numeric(longitude.dec)
    
    
    
    masterData.Complete <- cbind(masterData.Complete,
                                 latitude.dec,
                                 longitude.dec)

    
    new.ProvinceCode = NULL
    for(i in 1:length(masterData.Complete$Province.code)){
      if(masterData.Complete$Province.code[i] =="EASTERN CAPE"){

        new.ProvinceCode[i] = "E.CP"
      }
      
      else if(masterData.Complete$Province.code[i] =="FREE STATE"){
        
        new.ProvinceCode[i] = "F.ST"
      }
      
      else if(masterData.Complete$Province.code[i] =="GAUTENG"){
        
        new.ProvinceCode[i] = "GAU"
      }
      
      else if(masterData.Complete$Province.code[i] =="KWAZULU NATAL"){
        
        new.ProvinceCode[i] = "K.ZN"
      }
      
      else if(masterData.Complete$Province.code[i] =="LIMPOPO"){
        
        new.ProvinceCode[i] = "LIM"
      }
      
      else if(masterData.Complete$Province.code[i] =="MPUMALANGA"){
        
        new.ProvinceCode[i] = "MPU"
      }
      
      else if(masterData.Complete$Province.code[i] =="NORTH WEST"){
        
        new.ProvinceCode[i] = "N.WE"
      }
      
      else if(masterData.Complete$Province.code[i] =="NORTH WEST"){
        
        new.ProvinceCode[i] = "N.WE"
      }
      
      else if(masterData.Complete$Province.code[i] =="NORTHERN CAPE"){
        
        new.ProvinceCode[i] = "N.CP"
      }
      
      else if(masterData.Complete$Province.code[i] =="WESTERN CAPE"){
        
        new.ProvinceCode[i] = "W.CP"
      }
      
      else if(masterData.Complete$Province.code[i] =="Unknown"){
        
        new.ProvinceCode[i] = "Unknown"
      }
      
      else {new.ProvinceCode[i] = masterData.Complete$Province.code[i]}
    }
    
    new.ProvinceCode <- as.factor(new.ProvinceCode)
    
    #Imputing zero values of Completion.date
    
    masterData.Complete$Completion.date <- replace(masterData.Complete$Completion.date, 
                                                   masterData.Complete$Completion.date==0, 
                                                   median(masterData.Complete$Completion.date))
    
    #Imputing year (less than 1800) values of Completion.date
    masterData.Complete$Completion.date <- replace(masterData.Complete$Completion.date, 
                                                   masterData.Complete$Completion.date <= 1800, 
                                                   median(masterData.Complete$Completion.date))
    
    masterData.Complete$Completion.date <- replace(masterData.Complete$Completion.date, 
                                                   masterData.Complete$Completion.date >= 2035, 
                                                   median(masterData.Complete$Completion.date))
    
    
    masterData.Complete <- cbind(masterData.Complete,
                                 new.ProvinceCode)
    
    
    masterData.Complete <- subset(masterData.Complete,
                                  select =-c(Latitude.deg,
                                             Lat.min,
                                             Lat.sec,
                                             Longitude.deg,
                                             Long.min,
                                             Long.sec,
                                             Province.code))
    
    #Converting variables into appropriate factors, integers, numbers etc

    masterData.Complete$Water.management.area <- as.factor(masterData.Complete$Water.management.area)
    masterData.Complete$Distance.from.Town <- as.numeric(masterData.Complete$Distance.from.Town)
    masterData.Complete$Region.Code <- as.factor(masterData.Complete$Region.Code)
    masterData.Complete$Completion.date <- as.numeric(masterData.Complete$Completion.date)
    masterData.Complete$Wall.type <- as.factor(masterData.Complete$Wall.type)
    masterData.Complete$Wall.height <- as.numeric(masterData.Complete$Wall.height)
    masterData.Complete$Crest.Length..m. <- as.numeric(masterData.Complete$Crest.Length..m.)
    masterData.Complete$Capacity..1000.cub.m. <- as.numeric(masterData.Complete$Capacity..1000.cub.m.)
    masterData.Complete$Surface.area..ha. <- as.numeric(masterData.Complete$Surface.area..ha.)
    masterData.Complete$Size <- as.factor(masterData.Complete$Size)
    masterData.Complete$Category <- as.factor(masterData.Complete$Category)
    masterData.Complete$Sector <- as.factor(masterData.Complete$Sector)
    masterData.Complete$latitude.dec <- as.numeric(masterData.Complete$latitude.dec)
    masterData.Complete$longitude.dec <- as.numeric(masterData.Complete$longitude.dec)
    masterData.Complete$new.ProvinceCode <- as.factor(masterData.Complete$new.ProvinceCode)
    masterData.Complete$Hazard.Potential <- as.factor(masterData.Complete$Hazard.Potential)
    
    
    
    #Comment out the below chunk if u want to view masterData.Complete
    #Dropping all unused levels
   
    masterData.Complete <- droplevels.data.frame(masterData.Complete)
    
    masterData.Complete <- as.data.frame(masterData.Complete)
    
    masterData.Complete <- masterData.Complete %>% select(-Hazard.Potential, 
                                                            everything())
    
  })
  
  output$o.junk1 <- renderDataTable({
    
    masterData.Complete()
    
  })
  
  #----------------
  #Fig.1
  #----------------
  
  count.Hazard.Size <- reactive({
    table(masterData.Complete()$Hazard.Potential, masterData.Complete()$Size)
  })
  
  
  output$o.DamCharacteristics <- renderPlot({
    
    par(mfrow = c(1,3))
    barplot(prop.table(table(as.factor(masterData.Complete()$Hazard.Potential)))*100,
            col = rainbow(length(table(as.factor(masterData.Complete()$Hazard.Potential)))),
            ylim = c(0, 100),
            xlab = "Hazard level",
            ylab = "Relative frequency in %",
            main = "Relative frequency of Hazard level",
            cex.lab = 1.5,
            cex.main = 1.5)
    
    
    #minor.tick(ny=5)
    
    barplot(prop.table(table(as.factor(masterData.Complete()$Size)))*100,
            col = rainbow(length(table(as.factor(masterData.Complete()$Size)))),
            ylim = c(0, 100),
            xlab = "Size",
            ylab = "Relative frequency in %",
            main = "Relative frequency of Size",
            cex.lab = 1.5,
            cex.main = 1.5)
 
    barplot(prop.table(count.Hazard.Size())*100,
            legend = rownames(count.Hazard.Size()),
            col = rainbow(length(table(as.factor(masterData.Complete()$Hazard.Potential)))),
            xlab = "Size",
            ylab = "Relative frequency as %",
            main = "Stacked bar plot of Hazard against Size",
            cex.lab = 1.5,
            cex.main = 1.5)
            
            
            
  })
  
  #----------------
  #Fig.2
  #----------------
  
  count.Province.Hazard <- reactive({
    table(masterData.Complete()$Hazard.Potential, masterData.Complete()$new.ProvinceCode)
  })
  
  output$o.Geography <- renderPlot({
    
    par(mfrow = c(1,2))
    
    barplot(prop.table(table(as.factor(masterData.Complete()$new.ProvinceCode)))*100,
            col = rainbow(length(table(as.factor(masterData.Complete()$new.ProvinceCode)))),
            ylim = c(0, 100),
            xlab = "Province",
            ylab = "Relative frequency in %",
            main = "Relative frequency of Provinces",
            cex.lab = 1.5,
            cex.main = 1.5)
    
    barplot(prop.table(count.Province.Hazard())*100,
            legend = rownames(count.Province.Hazard()),
            col = rainbow(length(table(as.factor(masterData.Complete()$Hazard.Potential)))),
            xlab = "Provinces",
            ylab = "Relative frequency as %",
            main = "Stacked bar plot of Hazard for Provinces",
            cex.lab = 1.5,
            cex.main = 1.5)
    
    
  })
  

  
  #----------------
  #Fig.3
  #----------------
  #Time related plot------------
  
  output$o.Completiondate.Hazard <- renderPlot({
    
    
    barplot(log(table(masterData.Complete()$Hazard.Potential, 
                             masterData.Complete()$Completion.date)),
            ylim = c(0,15),
            legend = rownames(count.Province.Hazard()),
            col = rainbow(length(table(as.factor(masterData.Complete()$Hazard.Potential)))),
            xlab = "Year",
            ylab = "Frequency(log transformed)",
            main = "Comparison of Hazard accross years",
            cex.lab = 1.5,
            cex.main = 1.5)
    
    
  })
  
  #Parallel coordinates plot
  
  output$o.parcoordplot <- renderPlot({
    parallelplot(~masterData.Complete(),
                 group = Hazard.Potential,
                 data = masterData.Complete(),
                 horizontal.axis = FALSE,
                 auto.key = TRUE)
    
  })
  

  # #Prediction--------------------
  # 
  model <- reactive({
    
    small <- masterData.Complete()
    
    trainIndex <- sample(nrow(small),
                         size = nrow(small)*0.7)
    training <- small[trainIndex,]
    #str(training)
    testing  <- small[-trainIndex,]
    #print(str(testing))
    
    testing.x <- subset(testing,
                        select = -c(Hazard.Potential))
    
    #str(testing.x)
    #str(testing)
    cat("***************************************************************************************", "\n")
    cat("Your training data has",
              dim(training)[1],
              "data points and your test data has",
              dim(testing)[1],
              "data points.",
        "\n")
    cat("***************************************************************************************", "\n")
    
    # build rpart model
    set.seed(62433)
    #print('model starts')
    start.time <- Sys.time()
    withProgress(message = "1st Model building in progress",
                 detail = "This will take a while ...",
                 value = 0,{
    model <- randomForest(Hazard.Potential ~ . ,
                          data= training)
                 }

    )
    end.time <- Sys.time()

    time.taken <- end.time - start.time

    #print('model ends')


    # test rpart model
    pred <- predict(model,
                    testing,
                    type  = "class")

    cat("----------------------------------------", "\n")
    cat("Predictive accuracy on your testing data", "\n")
    cat("----------------------------------------", "\n")
    
    conf.matrix.on.testingdata <- table(predicted = pred,
                                        reference = testing$Hazard.Potential)

    print(conf.matrix.on.testingdata)

    test.accu <- (sum(diag(conf.matrix.on.testingdata))/dim(testing)[1])*100
    
    cat("***************************************************************************************", "\n")
    cat("Overall accuracy of testing data is",
         test.accu,
         "%",
        "\n")
    cat("***************************************************************************************", "\n")
    
    
    # cat("---------------------------------------------------------------------------------", "\n")
    # cat("Had the model been used on the training data, the predictive accuracies would be", "\n")
    # cat("---------------------------------------------------------------------------------", "\n")
    # print(model$confusion)
    # 
    # cat("---------------------------------------------------------------------------------","\n")
    
    
    cat("*******************************************", "\n")
    cat("Total time taken in processing",
        round(time.taken, digits = 1),
        "secounds",
        "\n")
    cat("*******************************************", "\n")


  })

  
  #Junk
  output$o.testingmodel <- renderPrint({
    model()
  })
  
  
  #Modeling for actual predictions
  
  final.model <- reactive({
    
    #print(colnames(masterData.Complete()[1:15]))
    
    
    start.time.final <- Sys.time()
    withProgress(message = "2nd Model building in progress",
                 detail = "This will take a while ...",
                 value = 0,{
                  
                  f.model <- randomForest(Hazard.Potential ~
                                            Water.management.area
                                          + Distance.from.Town
                                          + Region.Code
                                          + Completion.date
                                          + Wall.type
                                          + Wall.height
                                          + Crest.Length..m.
                                          + Capacity..1000.cub.m.
                                          + Surface.area..ha.
                                          + Size
                                          + Category
                                          + Sector
                                          + latitude.dec
                                          + longitude.dec
                                          + new.ProvinceCode,
                                         data= masterData.Complete())
             
                }
    )
    
    
    
    user.inputs <- data.frame(as.factor(input$Water.management.area),
                                as.numeric(input$Distance.from.Town),
                                as.factor(input$Region.Code),
                                as.numeric(input$Completion.Year),
                                as.factor(input$Wall.type),
                                as.numeric(input$Wall.height),
                                as.numeric(input$Crest.length),
                                as.numeric(input$Capacity),
                                as.numeric(input$Surface.area),
                                as.factor(input$Size),
                                as.factor(input$Category),
                                as.factor(input$Sector),
                                as.numeric(input$latitide.dec),
                                as.numeric(input$longitude.dec),
                                as.factor(input$newProvincecode))

      assign.colnames <- colnames(masterData.Complete()[1:15])

      colnames(user.inputs) <- assign.colnames

      cat("************************", "\n")
      cat("Your chosen inputs are", "\n")
      cat("************************", "\n") 
      print(t(user.inputs))
      # 
      # class(user.inputs)
      levels(user.inputs$Water.management.area) <- levels(masterData.Complete()$Water.management.area)
      levels(user.inputs$Region.Code) <- levels(masterData.Complete()$Region.Code)
      levels(user.inputs$Wall.type) <- levels(masterData.Complete()$Wall.type)
      levels(user.inputs$Size) <- levels(masterData.Complete()$Size)
      levels(user.inputs$Category) <- levels(masterData.Complete()$Category)
      levels(user.inputs$Sector) <- levels(masterData.Complete()$Sector)
      levels(user.inputs$new.ProvinceCode) <- levels(masterData.Complete()$new.ProvinceCode)
      
      
      cat("*******************************************************************************************", "\n")
      cat("The probability predictions (in %) of hazard level of a dam for the above chosen inputs are", "\n")
      cat("*******************************************************************************************", "\n")
      pred.ui.prob <- predict(f.model,
                         newdata = user.inputs,
                         type  = "prob")
      print(pred.ui.prob*100)
      
      cat("***************************************************", "\n")
      cat("Your Hazard level for the chosen inputs are", "\n")
      cat("***************************************************", "\n")
      pred.ui.class <- predict(f.model,
                              newdata = user.inputs,
                              type  = "class")
      print(pred.ui.class)
      
      cat("*************************************************", "\n")
      end.time.final <- Sys.time()
      cat("Time taken for the preiction is",
          end.time.final - start.time.final,
          "seconds.",
          "\n")
      cat("*************************************************", "\n")
    })


  # 
  output$o.final.pred <- renderPrint({

    final.model()

  })
  
  
  
  
  
  
  
  
  
 
  

})