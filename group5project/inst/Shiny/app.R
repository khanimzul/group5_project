library(stat297datasets)
data1=oral_health_service_2012_
data2=oral_health_service_2013_
data3=oral_health_service_2014_
data4=teeth_loss_6_crude_female_12_
data5=teeth_loss_6_crude_male_12_

ui=fluidPage(
  titlePanel("Basic DataTable"),

  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("locat",
                       "Location:",
                       c("All",
                         unique(as.character(data()$Location))))
    )
  ),

  column(4,
         selectInput("year",
                     "Year:",
                     c("All",
                       unique(as.character(data()$Year))))
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

    data=rbind(data1,data2,data3,data4,data5)

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
