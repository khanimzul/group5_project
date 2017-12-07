
ui=fluidPage(

  titlePanel("Oral Health"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      helpText("Choose discrete variable to produce plot:"),

      column(12,
             selectInput("disvar",
                         "Discrete Variable:",
                         c("--", "Topic", "Year", "Category")
             )
      ),

      helpText("Choose continuous variable to produce plot:"),

      column(12,
             selectInput("contvar",
                         "Continuous Variable:",
                         c("--", "Age", "Crude")
             )
      ),

      helpText("Customize table with various variables as desired:"),

      column(12,
             selectInput("topic",
                         "Topic:",
                         c("All",
                           unique(as.character(data$Topic))))
      ),

      column(12,
             selectInput("locat",
                         "Location:",
                         c("All",
                           unique(as.character(data$Location))))
      ),

      column(12,
             selectInput("year",
                         "Year:",
                         c("All",
                           unique(as.character(data$Year))))
      ),

      column(12,
             selectInput("cat",
                         "Category:",
                         c("All", "Gender", "Race"))
      ),
      column(12,
             selectInput("gender",
                         "Gender:",
                         c("All", "Male", "Female"))
      )

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Tabset w/ plot, table, and about ----
      tabsetPanel(type = "tabs",
                  tabPanel("Discrete Variables",
                           plotOutput("oralPlot"),
                           plotOutput("oralpie")),
                  tabPanel("Continuous Variables",
                           plotOutput("histPlot")),
                  tabPanel("Table",
                           fluidRow(
                             DT::dataTableOutput("table")
                           )),
                  tabPanel("Definition", uiOutput("text"))
      )

    )
  )
)


server=function(input, output) {


  # If else statement for bar plot

  output$oralPlot <- renderPlot({

    if (input$disvar != "--") {

      if (input$disvar == "Topic") {

        dataplot <- data[,4]

      }else if(input$disvar == "Year") {

        dataplot <- data[,2]

      }else if(input$disvar == "Category") {

        dataplot <- data[,7]

      }

      count= table(dataplot)

      # Render a barplot
      barplot(count,
              main="Barplot of Indicator Variable",
              xlab="Topic Indicator",
              ylab="Frequency",
              ylim = c( 0 , 500)
              )

    }
  }
  )

  output$oralpie <- renderPlot({

    if (input$disvar != "--") {

      if (input$disvar == "Topic") {

        dataplot <- data[,4]

      }else if(input$disvar == "Year") {

        dataplot <- data[,2]

      }else if(input$disvar == "Category") {

        dataplot <- data[,7]

      }

      count= table(dataplot)
      lbls <- paste(names(count), "\n", count, sep="")

      #Render piechart
      pie(count, labels = lbls,
          main="Pie Chart of Indicator Variable")

    }
  }
  )

  output$histPlot <- renderPlot({

    if (input$contvar != "--") {

      if (input$contvar == "Age") {

        data=age$`Data Value`

      }else if(input$contvar == "Crude") {

        data=crude$`Data Value`

      }

    #Render histogram
     hist(data)

    }
  }
  )


  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    if (input$topic != "All") {
      data <- data[data$Topic == input$topic,]
    }
    if (input$locat != "All") {
      data <- data[data$Location == input$locat,]
    }
    if (input$year != "All") {
      tabledata <- data[data$Year == input$year,]
    }
    if (input$cat != "All") {

      if (input$cat == "Gender") {

        if(input$gender == "Male") {
          data <- data[data$Category == "Male",]
        }else if(input$gender == "Female") {
          data <- data[data$Category == "Female",]
        }
      }
    }
    data
  }
  )
  )

  #Render text

  list <- c(
    "Measure definitions:",
  "Prevalence = The measured or estimated percentage of people -- weighted to population characteristics – with an attribute or disease during a specific year.",
  "Age-adjusted Prevalence = Prevalence (see above) standardized to the age distribution of a specific population, usually the U.S. 2000 standard population.",
"Crude Prevalence = Prevalence standardized to the measured number of deaths, cases of conditions, diseases or hospitalizations during a specific year – incidence and mortality rates are per 100,000 persons; hospitalization rates are per 1,000 persons."
  )

  text.data <- as.data.frame(list)

  colnames(text.data) <- " "

  output$text <- renderTable({

    print(text.data)

  })



 }

shinyApp(ui=ui, server=server)
