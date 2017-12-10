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
data41=age_dentalvisit_18ormore_female_12
data42=age_dentalvisit_18ormore_female_14
data43=age_dentalvisit_18ormore_male_12
data44=age_dentalvisit_18ormore_male_14
data45=crude_dentalsit_18ormore_male_14
data46=crude_dentalvisit_18ormore_female_12
data47=crude_dentalvisit_18ormore_female_14
data48=crude_dentalvisit_18ormore_male_12
data49=crude_dentalvisit_18ormore_male_14
data50=age_6teethloss_black_14
data51=age_6teethloss_hispanic_14
data52=age_teeth_loss_6_more_female_12
data53=age_teeth_loss_6_more_female_14
data54=age_teeth_loss_6_more_male_14
data55=age_teeth_loss_6_white_14
data56=AllTeethLostBlack12_Age_
data57=AllTeethLostBlack12_Crude_
data58=AllTeethLostBlack14_Age_
data59=AllTeethLostBlack14_Crude_
data60=AllTeethLostHispanic12_Age_
data61=AllTeethLostHispanic12_Crude_
data62=AllTeethLostHispanic14_Age_
data63=AllTeethLostMultiracial12_Age_
data64=AllTeethLostMultiracial12_Crude_
data65=AllTeethLostMultiRacial14_Age_
data66=AllTeethLostOther12_Age_
data67=AllTeethLostOther12_Crude_
data68=AllTeethLostOther14_Age_
data69=AllTeethLostWhite12_Age_
data70=AllTeethLostWhite12_Crude_
data71=AllTeethLostWhite14_Age_
data72=AllTeethLostWhite14_Crude_

data=rbind(data1,data2,data3,data4,data5,data6,data8,data9,data10,data11,data12,data13,data14,data15,data16,data17,data18,data19,data20,data21,data22,data23,data24,data25,data26,data27,data28,data29,data30,data31,data32,data33,data34,data35,data36,data37,data38,data39,data40,data41,data42,data43,data44,data45,data46,data47,data48,data49)

data=rbind(data,data41,data42,data43,data44,data45,data46,data47,data48,data49)

data=rbind(data,data50,data51,data52,data53,data54,data55,data56,data57,data58,data59,data60,data61,data62,data63,data64,data65,data66,data67,data68,data69,data70,data71,data72)
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

names(data49)==names(data50)
