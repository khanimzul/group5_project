library(stat297datasets)

ui=fluidPage(
  titlePanel("Basic DataTable"),

  # Create a new Row in the UI for selectInputs
  fluidRow(

    column(4,
           selectInput("topic",
                       "Topic:",
                       c("All",
                         unique(as.character(data$Topic))))
    ),

    column(4,
           selectInput("locat",
                       "Location:",
                       c("All",
                         unique(as.character(data$Location))))
    )
  ),

  column(4,
         selectInput("year",
                     "Year:",
                     c("All",
                       unique(as.character(data$Year))))
  ),

  column(4,
         selectInput("cat",
                     "Category:",
                     c("All", "Gender", "Race"))
  ),

  column(4,
         selectInput("gender",
                     "Gender:",
                     c("All", "Male", "Female"))
  ),



  # Create a new row for the table.
  fluidRow(
    DT::dataTableOutput("table")
  )
)

server=function(input, output) {


  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({

    library(stat297datasets)
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
  }))

}

shinyApp(ui=ui, server=server)
