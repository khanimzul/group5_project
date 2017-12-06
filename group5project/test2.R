library(readr)

oral_health_service_2012_ <- read_delim("oral health service (2012).csv",
                                        "\t", escape_double = FALSE, trim_ws = TRUE)

oral_health_service_2013_ <- read_delim("oral health service (2013).csv",
                                        "\t", escape_double = FALSE, trim_ws = TRUE)

oral_health_service_2014_ <- read_delim("oral health service (2014).csv",
                                        "\t", escape_double = FALSE, trim_ws = TRUE)

teeth_loss_6_crude_female_12_ <- read_csv("teeth loss 6 crude female'12 .csv")
teeth_loss_6_crude_male_12_ <- read_csv("teeth loss 6 crude male'12csv.csv")

library(ggplot2)

View(data)

names(data2)==names(data7)

data1=oral_health_service_2012_
data2=oral_health_service_2013_
data3=oral_health_service_2014_
data4=oral_health_service_2015_
data5=teeth_loss_6_crude_female_12_
data6=teeth_loss_6_crude_male_12_
data7=notoothlossMale12Age
data8=AllTeethLostFem14_Age_
data9=AllTeethLostMale14_Age_
data10=crude_female_teeth_loss_6_more_14
data11=crude_teeth_loss_6_more_male_14

data=rbind(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,data11)

##tab
##visual tools: boxplot, pie chart, histogram

View(data)

library(markdown)
includeMarkdown(about.md)

dataage=(data[,2]== "Crude Prevalence")

dataage=TRUE

oralhealth=data



##histogram
output$oralHist <- renderPlot({

  #If else statement for input type
  if (input$dataset != "--") {

    if (input$dataset == "Dental Visit" && input$variable == "Age Prevalence") {

      data= dentalvisit$Age

    }else if(input$dataset == "Dental Visit" && input$variable == "Crude Prevalence"){

      data= dentalvisit$Crude

    }else if(input$dataset == "All Teeth Loss for Adult >=65 years" && input$variable == "Age Prevalence"){

      data= allloss$Age

    }else if(input$dataset == "All Teeth Loss for Adult >=65 years" && input$variable == "Crude Prevalence"){
      data= allloss$Crude

    }
  }

  hist(data,
       main= c(input$dataset, input$variable),
       ylab="Frequency")
}
)


##Pie chart
if (input$topicinterest != "--") {

  if (input$topicinterest == "Topic") {

    dataplot <- data[,4]

  }else if(input$topicinterest == "Year") {

    dataplot <- data[,2]

  }else if(input$topicinterest == "Category") {

    dataplot <- data[,7]

  }

  count= table(data)

  # Render a barplot
  mytable <- table(count)
  lbls <- paste(names(mytable), "\n", mytable, sep="")
  pie(mytable, labels = lbls,
      main="Pie Chart of Indicator Variable")
