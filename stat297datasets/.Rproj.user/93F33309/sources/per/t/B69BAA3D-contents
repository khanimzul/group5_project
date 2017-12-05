library(readr)

oral_health_service_2012_ <- read_delim("oral health service (2012).csv",
                                        "\t", escape_double = FALSE, trim_ws = TRUE)

oral_health_service_2013_ <- read_delim("oral health service (2013).csv",
                                        "\t", escape_double = FALSE, trim_ws = TRUE)

oral_health_service_2014_ <- read_delim("oral health service (2014).csv",
                                        "\t", escape_double = FALSE, trim_ws = TRUE)

teeth_loss_6_crude_female_12_ <- read_csv("teeth loss 6 crude female'12 .csv")
teeth_loss_6_crude_male_12_ <- read_csv("teeth loss 6 crude male'12csv.csv")



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

View(data)
