
ui=fluidPage(

  titlePanel("Oral Health"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      helpText("Instruction: Choose Topic of Interest to produce plot"),

      column(12,
             selectInput("topicinterest",
                         "Topic of Interest:",
                         c("--", "Topic", "Year", "Category")
             )
      ),

      helpText("Instruction: Customize table with various variables as desired"),

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
                  tabPanel("Bar Plot", plotOutput("oralPlot")),
                  tabPanel("Pie Chart", plotOutput("oralpie")),
                  tabPanel("Table",
                           fluidRow(
                             DT::dataTableOutput("table")
                           )),
                  tabPanel("About", uiOutput("text"))
      )

    )
  )
)


server=function(input, output) {


  # If else statement for input type

  output$oralPlot <- renderPlot({

    if (input$topicinterest != "--") {

      if (input$topicinterest == "Topic") {

        dataplot <- data[,4]

      }else if(input$topicinterest == "Year") {

        dataplot <- data[,2]

      }else if(input$topicinterest == "Category") {

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

    if (input$topicinterest != "--") {

      if (input$topicinterest == "Topic") {

        dataplot <- data[,4]

      }else if(input$topicinterest == "Year") {

        dataplot <- data[,2]

      }else if(input$topicinterest == "Category") {

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
