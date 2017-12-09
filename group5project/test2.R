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
data8=AllTeethLostFem14_Age_
data9=AllTeethLostMale14_Age_
data10=crude_female_teeth_loss_6_more_14
data11=crude_teeth_loss_6_more_male_14
data12=NotoothlossBlack12_Age
data13=NotoothlossBlack12_Crude
data14=NotoothlossBlack14_Age
data15=NotoothlossBlack14_Crude
data16=NotoothlossFemale12_Age
data17=NotoothlossFemale12_Crude
data18=NotoothlossFemale14_Age
data19=NotoothlossFemale14_Crude
data20= NotoothlossMale12_Age
data21=NotoothlossMale12_Crude
data22=NotoothlossMale14_Age
data23=NotoothlossMale14_Crude
data24=NotoothlossMulti12_Age
data25=NotoothlossMulti12_Crude
data26=NotoothlossMulti14_Age
data27=NotoothlossMulti14_Crude
data28=NotoothlossOther12_Age
data29=NotoothlossOther12_Crude
data30=NotoothlossOther14_Age
data31=NotoothlossOther14_Crude
data32=NotoothlossWhite12_Age
data33= NotoothlossWhite12_Crude
data34= NotoothlossWhite14_Age
data35= NotoothlossWhite14_Crude
data36= NotoothlossHispanic12_Age
data37= NotoothlossHispanic12_Age
data38=NotoothlossHispanic12_Crude
data39=NotoothlossHispanic14_Age
data40=NotoothlossHispanic14_Crude

data=rbind(data1,data2,data3,data4,data5,data6,data8,data9,data10,data11,data12,data13,data14,data15,data16,data17,data18,data19,data20,data21,data22,data23,data24,data25,data26,data27,data28,data29,data30,data31,data32,data33,data34,data35,data36,data37,data38,data39,data40)

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
}
  count= table(data)

  # Render a barplot
  mytable <- table(count)
  lbls <- paste(names(mytable), "\n", mytable, sep="")
  pie(mytable, labels = lbls,
      main="Pie Chart of Indicator Variable")


  crude=data[data$`Data Type` == "Crude Prevalence",]

  age=data[data$`Data Type` == "Age-adjusted Prevalence",]

devtools::use_data(crude)
