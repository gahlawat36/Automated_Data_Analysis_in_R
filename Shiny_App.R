library(shiny)
library(shinythemes)
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  #shinythemes ::themeSelector(),
  titlePanel("Statistics for Data Analytics",windowTitle = "10523631-Project"),
  navbarPage(
    span(textOutput("Statistics for Data Analytics"), style="color:blue"),
    
    ################################################### DATA IMPORT ####################################################  
    
    tabPanel("Data Source",
             sidebarLayout(
               
               
               # Sidebar panel for inputs 
               sidebarPanel(
                 
                 # Input: Select a file 
                 fileInput("file1", "Choose CSV File",
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv",
                                      ".xlsx")),
                 
                 # Horizontal line
                 tags$hr(),
                 
                 # Input: Checkbox if file has header 
                 checkboxInput("header", "Header", TRUE),
                 
                 # Input: Select separator 
                 radioButtons("sep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              selected = ","),
                 
                 # Input: Select quotes 
                 radioButtons("quote", "Quote",
                              choices = c(None = "",
                                          "Double Quote" = '"',
                                          "Single Quote" = "'"),
                              selected = '"'),
                 
                 # Horizontal line 
                 tags$hr(),
                 
                 # Input: Select number of rows to display 
                 radioButtons("disp", "Display",
                              choices = c(Head = "head",
                                          All = "all"),
                              selected = "head"),
                 
                 
                 actionButton("SourceData", "Use Data!", class = "btn-primary")
                 
               ),
               
               # Main panel for displaying outputs 
               mainPanel(
                 
                 # Output: Data file 
                 tableOutput("contents"),
                 verbatimTextOutput("DataSummary")
                 #textOutput("ColNames")
                 
               )  
             )
    ),
    
    
    #####################  DESCRIPTIVE-ANALYSIS UI #####################################################################
    
    tabPanel("Descriptive Analysis",
             sidebarPanel(
               selectInput("XAxis", "Select First dimension", "Loading..."),
               selectInput("YAxis", "Select second dimension", "Loading..."),
               
               print("\n \n Note: Histogram takes only one dimension hence only first dimension goes into it. \n\n ")
               
               
             ),
             mainPanel(
               tabsetPanel(
                 
                 tabPanel("Scatter Plot",
                          
                          plotOutput("ScatterOutput")),
                 
                 tabPanel("Histogram",
                          
                          plotOutput("HistogramOutput")),
                 
                 
                 
                 tabPanel("Central Tendencies",
                          
                          selectInput("DataForAnalysis", "Select Column Name", "Loading..."),
                          print("Mean"),
                          verbatimTextOutput("DisplayMean"),
                          print('Median'),
                          verbatimTextOutput("DisplayMedian"),
                          print('Mode'),
                          verbatimTextOutput("DisplayMode")),
                 
                 tabPanel("Spread", 
                          selectInput("DataForSpread", "Select Column Name", "Loading..."),
                          print("Variance"),
                          verbatimTextOutput("DisplayVariance"),
                          print('Standard deviation'),
                          verbatimTextOutput("DisplayStd"))
                 
               )
             )
    ),
    
    
    ######################  PROBABLISTIC-MODEL ######################################################################
    
    tabPanel("Discreate Random Models",
             sidebarLayout(
               sidebarPanel( 
                 
                 selectInput("dismodel", "Select Model", 
                             
                             choices = c("Binomial" = "binomial", 
                                         
                                         "Poisson" = "poisson", 
                                         
                                         "Geometric" = "geometric"), 
                             
                             selected = "binomial" 
                             
                 ), 
                 
                 conditionalPanel( 
                   
                   condition = "input.dismodel == 'binomial'", 
                   
                   numericInput("n", "parameter n in Binomial" , value = 10), 
                   
                   numericInput("p", "parameter p in Binomial" , value = 0.5) 
                   
                 ), 
                 
                 
                 
                 conditionalPanel(     
                   
                   condition = "input.dismodel == 'poisson'", 
                   
                   numericInput("lam", "parameter lambda in Poisson" , value = 1) 
                   
                 ), 
                 
                 
                 
                 conditionalPanel(     
                   
                   condition = "input.dismodel == 'geometric'", 
                   
                   numericInput("p", "parameter p in Geometric" , value = 0.5) 
                   
                 ), 
                 
                 
                 
                 numericInput("max", "upper limit for x" , value = 5),  
                 
                 sliderInput("s", "number of simulated data" ,min=1, max=1000, value = 10),  
                 
                 
                 
                 conditionalPanel( 
                   
                   condition = "input.dismodel == 'binomial'", 
                   
                   numericInput("j1", "j for Bin" , value = 1) 
                   
                 ), 
                 
                 
                 
                 conditionalPanel( 
                   
                   condition = "input.dismodel == 'poisson'", 
                   
                   numericInput("j2", "j for Poisson" , value = 1) 
                   
                 ), 
                 
                 
                 
                 conditionalPanel( 
                   
                   condition = "input.dismodel == 'geometric'", 
                   
                   numericInput("j3", "j for geometric" , value = 1) 
                   
                 ) 
                 
                 
                 
                 
                 
               ),  
               
               mainPanel(  
                 
                 plotOutput("histogram"),  
                 
                 tableOutput('tab')  
                 
               )
               
               
             )),
    
    
    
    ################################### HYPOTHESIS-TESTING ############################################################
    tabPanel("Hypothesis Testing",
             
             sidebarPanel( 
               
               sliderInput("bins", 
                           
                           "Size of Graph", 
                           
                           min = 1, 
                           
                           max = 50, 
                           
                           value = 2 
                           
               ), 
               
               radioButtons("sample", 
                            
                            "Please select the type of t-test:", 
                            
                            choices = c("Two-Sample" = "twoSamp", 
                                        
                                        "One-Sample" = "oneSamp")), 
               
               selectInput("var1",  
                           
                           label = "Please Select a Numerical Variable", 
                           
                           "Loading..." 
                           
               ), 
               
               conditionalPanel(condition = "input.sample == 'twoSamp'", 
                                
                                selectInput("var2",  
                                            
                                            label = "Please Select a Numerical Variable", 
                                            
                                            "Loading..." 
                                            
                                ), 
                                
                                radioButtons("varequal", 
                                             
                                             "Does the two samples have equal variance:", 
                                             
                                             choices = c("Yes" = "y", 
                                                         
                                                         "No" = "n")) 
                                
               ), 
               
               selectInput("tail", 
                           
                           label = "Please Select a relationship you want to test:", 
                           
                           choices = c("Equal" = "two.sided",  
                                       
                                       "Less" = "less", 
                                       
                                       "Greater" = "greater")), 
               
               conditionalPanel(condition = "input.sample == 'oneSamp'", 
                                
                                numericInput("test", 
                                             
                                             "Choose the Mean Value to Test", 
                                             
                                             value = 0 
                                             
                                             
                                             
                                ) 
                                
               ), 
               
               numericInput("conf", 
                            
                            label = "Please Select a confidence level:", 
                            
                            value = 0.95, 
                            
                            min = 0.8, 
                            
                            max = 0.99), 
               
               helpText("Note: Please assign a number between 0 and 1 in the numeric Input") 
               
               
               
               
               
             ), 
             
             mainPanel( 
               
               fluidRow(column(10, offset = 1, 
                               
                               plotOutput("graph1"))), 
               
               hr(), 
               
               fluidRow(column(8, offset = 1, 
                               
                               h2("Hypothesis of the t-test"), 
                               
                               paste("We are testing the null hypothesis that the mean of population equals to the value you set."), 
                               
                               paste("The observed t-test statistic :"), 
                               
                               textOutput("tvalue"), 
                               
                               paste("A low p-value suggests that your sample provides enough evidence that you can reject the null hypothesis for the entire population."), 
                               
                               textOutput("pvalue"), 
                               
                               hr(), 
                               
                               h2("Summary"), 
                               
                               paste("The observed sample statistics were:"), 
                               
                               tableOutput("parametric"), 
                               
               ) 
               
               ) 
             ) 
             
    ),
    
    
    
    ################################################  GENERAL LINEAR MODELS ###############################################
    tabPanel("GLM",  
             
             sidebarPanel( 
               
               
               
               selectInput("outVar",  
                           
                           label = "Predicting Var", 
                           
                           "Loading..." 
                           
               ), 
               
               selectInput("indVar",  
                           
                           label = "Independent Var", 
                           
                           "Loading..." 
                           
               ), 
               
               uiOutput("model_select"), 
               
               hr(), 
               
               uiOutput("var1_select"), 
               
               uiOutput("rest_var_select") 
               
               
             ), 
             
             mainPanel( 
               
               tabsetPanel( 
                 
                 tabPanel("Liner Regression", id = "linReg", 
                          
                          h4("Liner Regression Summary"), 
                          
                          verbatimTextOutput("summary"), 
                          
                          hr(), 
                          
                          plotOutput("scatterplot"), 
                          
                          hr(), 
                          
                          fluidRow(h4("Distribution"), 
                                   
                                   column(6, plotOutput("distrb1")), 
                                   
                                   column(6, plotOutput("distrb2")) 
                                   
                          ) 
                          
                 ), 
                 
                 tabPanel("Logistic Regression",  
                          
                          h4("Logistic Regression Summary"), 
                          
                          verbatimTextOutput("other_val_show") 
                          
                 ) 
                 
               ) 
               
             )) 
    
    #####################################   END OF UI #########################################################   
  )
)



#######################################  SERVER #############################################################



server <- function(input, output, session) {
  output$txtout <- renderText({
    paste(input$txt, input$slider, format(input$date), sep = ", ")
  })
  
  
  general_df <- reactive({
    req(input$file1)
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        
        stop(safeError(e))
      }
    )
    
    
  })
  
  output$contents <- renderTable({
    
    
    if(input$disp == "head") {
      return(head(general_df()))
    }
    else {
      return(general_df())
    }
    
  })
  
  
  output$DataSummary <- renderPrint({
    sumData <- general_df()
    summary(sumData)
  })
  
  
  output$ColNames <- renderText({
    
    colnames(general_df())
    
  })
  
  observe({
    
    cacheColName <- colnames(general_df())
    updateSelectInput(session, "DataForAnalysis", choices = cacheColName)
    updateSelectInput(session, "DataForSpread", choices = cacheColName)
    updateSelectInput(session, "XAxis", choices = cacheColName)
    updateSelectInput(session, "YAxis", choices = cacheColName)
    updateSelectInput(session, "var1", choices = cacheColName)
    updateSelectInput(session, "var2", choices = cacheColName)
    updateSelectInput(session, "outVar", choices = cacheColName)
    updateSelectInput(session, "indVar", choices = cacheColName)
    
    
  })
  
  output$DisplayMean <- renderText({
    
    Cachedf <- general_df()
    DtMean <- input$DataForAnalysis
    mean(Cachedf[[DtMean]])
  })
  
  output$DisplayMedian <- renderText({
    
    Cachedf <- general_df()
    DtMedian <- input$DataForAnalysis
    median(Cachedf[[DtMedian]])
  })
  
  Mode = function(x){
    ta = table(x)
    tam = max(ta)
    if (all(ta == tam))
      mod = NA
    else
      if(is.numeric(x))
        mod = as.numeric(names(ta)[ta == tam])
    else
      mod = names(ta)[ta == tam]
    return(mod)
  }
  
  output$DisplayMode <- renderText({
    
    Cachedf <- general_df()
    DtMode <- input$DataForAnalysis
    Mode(Cachedf[[DtMode]])
  })
  
  
  output$ScatterOutput <- renderPlot({
    
    
    Cachedf <- general_df()
    
    library(car)
    scatterplot(Cachedf[[input$XAxis]],Cachedf[[input$YAxis]], main="Scatterplot", col = 'green',
                xlab=Cachedf[[input$XAxis]], ylab=Cachedf[[input$YAxis]], pch=19)
    
    
  })
  
  output$HistogramOutput <- renderPlot({
    
    
    Cachedf <- general_df()
    hist(Cachedf[[input$XAxis]], main="Histogram", 
         xlab="Selected Column", 
         border="blue", 
         col="green",
         breaks=7)
  })
  
  var.p = function(x){var(x)*(length(x)-1)/length(x)}
  
  output$DisplayVariance <- renderText({
    
    Cachedf <- general_df()
    DtVariance <- input$DataForSpread
    var.p(Cachedf[[DtVariance]])
  })
  
  sd.p=function(x){sd(x)*sqrt((length(x)-1)/length(x))}
  
  output$DisplayStd <- renderText({
    
    Cachedf <- general_df()
    DtStd <- input$DataForSpread
    sd.p(Cachedf[[DtStd]])
    
  })
  
  
  
  ######################################### PROBABILISTIC MODEL ###########################################3
  
  output$histogram <- renderPlot({ 
    
    
    
    # binomial  
    
    if (input$dismodel == 'binomial') { 
      
      par(mfrow=c(1,2))  
      
      d <- density(rbinom(1000,input$n,input$p))  
      
      plot(d, main="Kernel Density of generated data")  
      
      polygon(d, col="red", border="blue") 
      
      x=0:input$n  
      
      plot(x,dbinom(x,input$n,input$p))  
      
      
      
    }
    
    # poisson 
    
    if (input$dismodel == 'poisson') { 
      
      par(mfrow=c(1,2))   
      
      D=rpois(input$s, input$lam)  
      
      tab=table(D)  
      
      barplot(tab,col='blue')  
      
      x1=0:input$max  
      
      y1=dpois(x1,input$lam)  
      
      plot(x1,y1,type='b')  
      
    }
    
    # geometric  
    
    if (input$dismodel == 'geometric') { 
      
      par(mfrow=c(1,2)) 
      
      D=rgeom(input$s, input$p)  
      
      tab=table(D)  
      
      barplot(tab,col='blue')  
      
      x2=0:input$max  
      
      y2=dgeom(x2,input$p)  
      
      plot(x2,y2,type='b')  
      
    } 
    
    
    
  })    
  
  
  
  output$tab <- renderTable({  
    
    
    
    p1=dbinom(input$j1,input$n, input$p)  
    
    p2=dpois(input$j2,input$lam)  
    
    p3=dgeom(input$j3,input$p)  
    
    
    
    
    
    c(p1,p2,p3) 
    
  })
  
  
  
  
  ############################################## Hypothesis Testing #################################
  
  
  output$graph1 <- renderPlot({
    var1 <- general_df()[,input$var1]
    var2 <- general_df()[,input$var2]
    if (is.null(var1)){return(NULL)}
    if (is.null(var2)){return(NULL)}
    graph2 <- ifelse(input$sample == 'oneSamp', FALSE, TRUE)
    p1 <- hist(var1, breaks = input$bins)
    p2 <- hist(var2, breaks = input$bins)
    plot(p1, col=rgb(0,0,1,1/4))
    if(input$sample == 'twoSamp')
      plot(p2, col=rgb(1,0,0,1/4),add = graph2)
  })
  
  output$parametric <- renderTable({
    var1 <- general_df()[,input$var1]
    if (is.null(var1)){return(NULL)}
    var2 <- general_df()[,input$var2]
    if (is.null(var2)){return(NULL)}
    mean1 <- mean(var1)
    mean2 <- mean(var2)
    standard_deviation1 <- sd(var1)
    standard_deviation2 <- sd(var2)
    standard_error1 <- sd(var1)/sqrt(length(var1))
    standard_error2 <- sd(var2)/sqrt(length(var2))
    parametric1 <- data.frame(mean = mean1, 
                              standard_deviation=standard_deviation1, 
                              standard_error=standard_error1)
    row.names(parametric1) <- input$var1
    parametric2 <- data.frame(mean = mean2, 
                              standard_deviation=standard_deviation2, 
                              standard_error=standard_error2)
    row.names(parametric2) <- input$var2
    if(input$sample == "oneSamp") {return(parametric1)}
    if(input$sample == "twoSamp") {return(rbind(parametric1,parametric2))}
  })  
  
  # Create a one sample and two sample t-test reactive function
  ttestout <- reactive({
    var1 <- general_df()[,input$var1]
    conf <- input$conf
    if (is.null(var1)){return(NULL)}
    t1 <- t.test(var1, alternative = input$tail, mu = input$test, conf.level = conf)
    var2 <- general_df()[,input$var2]
    if (is.null(var2)){return(NULL)}
    ve <- ifelse(input$varequal == 'y', TRUE, FALSE)
    t2 <- t.test(var1, var2, alternative = input$tail, var.equal = ve, conf.level = conf)
    if(input$sample == "oneSamp") {return(t1)}
    if(input$sample == "twoSamp") {return(t2)}
    
  })
  
  # Output of one sample t value of t-test
  output$tvalue <- renderPrint({
    vals <- ttestout()
    if (is.null(vals)){return(NULL)}
    vals$statistic
  })
  
  # Output of p value
  output$pvalue <- renderPrint({
    vals <- ttestout()
    if (is.null(vals)){return(NULL)}
    vals$p.value 
  })
  

  
  ##############################GLM ########################
  
  #--------------- Linear Regression --------------- 
  
  # Scatterplot output 
  
  output$scatterplot <- renderPlot({ 
    
    plot(general_df()[,input$indVar], general_df()[,input$outVar], main="Scatterplot", 
         
         xlab=input$indVar, ylab=input$outVar, pch=19) 
    
    abline(lm(general_df()[,input$outVar] ~ general_df()[,input$indVar]), col="red") 
    
    lines(lowess(general_df()[,input$indVar],general_df()[,input$outVar]), col="blue") 
    
  }, height=400) 
  
  
  
  # Histogram output var 1 
  
  output$ distrb  <- renderPlot({ 
    
    hist(general_df()[,input$outVar], main="", xlab=input$ outVar) 
    
  }, height=300, width=300) 
  
  
  
  # Histogram output var 2 
  
  output$ distrb2 <- renderPlot({ 
    
    hist(general_df()[,input$indVar], main="", xlab=input$ indVar) 
    
  }, height=300, width=300) 
  
  
  
  #----- Logistic Regression --------------- 
  
  # Regression output 
  
  output$summary <- renderPrint({ 
    
    fit <- lm(general_df()[,input$outVar] ~ general_df()[,input$indVar]) 
    
    names(fit$coefficients) <- c("Intercept", input$var2) 
    
    summary(fit) 
    
  }) 
  
  
  
  output$model_select<-renderUI({ 
    
    selectInput("modelselect","Select Algo",choices = c("Logistic_reg"="logistreg","Linear_reg "="lm")) 
    
  }) 
  
  
  
  output$var1_select<-renderUI({ 
    
    selectInput("ind_var_select","Select Independent Variable", choices =as.list(names(general_df())),multiple = FALSE) 
    
  }) 
  
  
  
  output$rest_var_select<-renderUI({ 
    
    checkboxGroupInput("other_var_select","Select other Variable",choices =as.list(names(general_df()))) 
    
  }) 
  
  
  
  output$other_val_show<-renderPrint({ 
    
    input$other_var_select 
    
    input$ind_var_select 
    
    f<-general_df() 
    
    library(caret) 
    
    form <- sprintf("%s~%s",input$ind_var_select,paste0(input$other_var_select,collapse="+")) 
    
    print(form) 
    
    
    
    logistreg <-glm(as.formula(form),family=binomial(),data=f) 
    
    print(summary(logistreg)) 
    
  })  
  
  ######################################### END OF SERVER ##########################################################
  
  
}

shinyApp(ui, server)
