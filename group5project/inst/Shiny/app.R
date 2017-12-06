
ui=fluidPage(

  titlePanel("Oral Health"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      helpText("Instruction: Choose Topic of Interest to produce bar plot"),

      column(12,
             selectInput("topicinterest",
                         "Topic of Interest:",
                         c("All", "Topic", "Year", "Category")
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

      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("oralPlot")),
                  tabPanel("Table",
                           fluidRow(
                             DT::dataTableOutput("table")
                           ))
      )

    )
  )
)


server=function(input, output) {


  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    if (input$topic != "All") {
      data <- data[data$Topic == input$topic,]
    }
    if (input$locat != "All") {
      data <- data[data$Location == input$locat,]
    }
    if (input$year != "All") {
      data <- data[data$Year == input$year,]
    }
    if (input$cat != "All") {

      if (input$cat == "Gender") {

        if(input$gender == "Male") {
          data <- data[data$Category == "Male",]
        }

        else if(input$gender == "Female") {
          data <- data[data$Category == "Female",]
        }
      }
    }
    data
  }
  )
  )


  # Render a barplot

  output$oralPlot <- renderPlot({

    if (input$topicinterest != "All") {

      if (input$topicinterest == "Topic") {

        data <- data[,4]

      }else if(input$topicinterest == "Year") {

        data <- data[,2]

      }else if(input$topicinterest == "Category") {

        data <- data[,7]

      }

      count= table(data)

      # Render a barplot
      barplot(count,
              main=input$topicinterest,
              horiz = TRUE,
              ylab="Topic Indicator",
              xlab="Frequency",
              xlim = c( 0 , 200))
    }
  })

}

shinyApp(ui=ui, server=server)
