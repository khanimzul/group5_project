library(DT)

ui=fluidPage(

  titlePanel("Oral Health"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      helpText("Choose discrete variable to produce bar plot and pie chart:"),

      column(12,
             selectInput("disvar",
                         "Discrete Variable:",
                         c("--", "Topic", "Year", "Category")
             )
      ),

      helpText("Choose continuous variable to histogram:"),

      column(12,
             selectInput("contvar",
                         "Continuous Variable:",
                         c("--", "Age", "Crude")
             )
      ),

      helpText("Customize table with desired variables:"),

      column(12,
             selectInput("topic",
                         "Topic:",
                         c("All","All teeth lost among adults aged >= 65 years","No tooth loss among adults aged 18-64 years",
                           "Oral Health Service ","Six or more teeth lost among adults aged >= 65 years","Visits to dentist or dental clinic among adults aged >= 18 years"))
      ),

      column(12,
             selectInput("locat",
                         "Location:",
                         c("All","Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware",
                           "District of Columbia","Florida","Georgia","Guam","Hawaii","Idaho","Illinois",
                           "Indiana","Iowa","Kansas","Kentucky","Lousiana","Maine","Maryland","Massachusetts",
                           "Michigan","Minnesota","Mississppi","Missouri","Montana","Nebraska","Nevada","New Hampshire",
                           "New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon",
                           "Pennsylvania","Puerto Rico","Rhode Island","South Carolina","South Dakota","Tennessee","Texas",
                           "United State","Utah","Vermont","Virgin Island","Virginia","Washington","West Verginia","Wisconsin",
                           "Wyoming"))

      ),

      column(12,
             selectInput("year",
                         "Year:",
                         c("All","2012","2013","2014","2015"))
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
      ),
      column(12,
             selectInput("race",
                         "Race:",
                         c("All", "Black", "White","Multiracial","Other","Hispanic"))
      )),
    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Tabset w/ plot, table, and about ----
      tabsetPanel(type = "tabs",
                  tabPanel("Discrete Variables",
                           plotOutput("oralpie", width = 500)),
                  tabPanel("Continuous Variables",
                           plotOutput("histPlot")),
                  tabPanel("Table",fluidRow(DT::dataTableOutput("table"))),
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

        dataplot <- oralhealthdata[,4]

      }else if(input$disvar == "Year") {

        dataplot <- oralhealthdata[,2]

      }else if(input$disvar == "Category") {

        dataplot <- oralhealthdata[,7]

      }

      count= table(dataplot)

      # Render a barplot
      barplot(count,
              main="Barplot of Indicator Variable",
              horiz = TRUE,
              ylab="Topic Indicator",
              xlab="Frequency",
              xlim = c( 0 , 400)

      )

    }
  }
  )

  output$oralpie <- renderPlot({

    if (input$disvar != "--") {

      if (input$disvar == "Topic") {

        dataplot <- oralhealthdata[,4]

      }else if(input$disvar == "Year") {

        dataplot <- oralhealthdata[,2]

      }else if(input$disvar == "Category") {

        dataplot <- oralhealthdata[,7]

      }

      count= table(dataplot)
      lbls <- paste(names(count), "\n", count, sep="")

      #Render piechart
      pie(count, labels = lbls,
          main="Pie Chart of Indicator Variable", radius=1)

    }
  }
  )

  output$histPlot <- renderPlot({

    if (input$contvar != "--") {

      if (input$contvar == "Age") {
        dataage=oralhealthdata[oralhealthdata$`Data Type` == "Age-adjusted Prevalence",]

        age=dataage$`Data Value`

        data=age

      }else if(input$contvar == "Crude") {

        datacrude=oralhealthdata[oralhealthdata$`Data Type` == "Crude Prevalence",]

        crude=datacrude$`Data Value`

        data=crude

      }

      #Render histogram
      hist(data)

    }
  }
  )


  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    if (input$topic != "All") {
      data <- oralhealthdata[oralhealthdata$Topic == input$topic,]
    }
    if (input$locat != "All") {
      data <- oralhealthdata[oralhealthdata$Location == input$locat,]
    }
    if (input$year != "All") {
      data <- oralhealthdata[oralhealthdata$Year == input$year,]
    }
    if (input$cat != "All") {

      if (input$cat == "Gender") {

        if(input$gender == "Male") {
          data <- oralhealthdata[oralhealthdata$Category == "Male",]
        }else if(input$gender == "Female") {
          data <- oralhealthdata[oralhealthdata$Category == "Female",]

        }else if (input$cat == "Race") {

          if(input$race == "Black") {
            data <- oralhealthdata[oralhealthdata$Category == "Black",]
          }else if(input$race == "White") {
            data <- oralhealthdata[oralhealthdata$Category == "White",]
          }else if(input$race == "MultiRacial") {
            data <- oralhealthdata[oralhealthdata$Category == "Multiracial",]
          }else if(input$race == "Other") {
            data <- oralhealthdata[oralhealthdata$Category == "Other",]
          }else if(input$race == "Hispanic") {
            data <- oralhealthdata[oralhealthdata$Category == "Hispanic",]
          }

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
    "Age-adjusted Prevalence = Prevalence standardized to the age distribution of a specific population, usually the U.S. 2000 standard population.",
    "Crude Prevalence = Prevalence standardized to the measured number of deaths, cases of conditions, diseases or hospitalizations during a specific year – incidence and mortality rates are per 100,000 persons; hospitalization rates are per 1,000 persons."
  )

  text.data <- as.data.frame(list)

  colnames(text.data) <- " "

  output$text <- renderTable({

    print(text.data)

  })

  options(shiny.sanitize.errors = FALSE)


}

shinyApp(ui=ui, server=server)

