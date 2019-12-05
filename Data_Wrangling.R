#install.packages("RODBC") or install.packages('RODBC', dependencies=TRUE, repos='http://cran.rstudio.com/')
#install.packages("odbc")
#install.packages("ggplot2", lib = "C:/Users/ju0d/Documents/R/win-library/3.6")

library(odbc)
#library(DBI)
library(lubridate)
library(tidyverse) # tidyverse contains library(readr),library(readxl),library(stringr),#library(dplyr),library(tidyr)
                    # and library("ggplot2", lib.loc= "C:/Users/ju0d/Documents/R/win-library/3.6") #library(gglot2) stopped working after upgrade to version 3.6.1


myConn <- dbConnect(odbc::odbc(), "RFH-INFORMATION")

myData<- dbGetQuery(myConn,"
              
        Select Financial_Year,Financial_Month
		    ,Report_Date,Left(Datename(m,Report_Date),3) As [Month]
		    ,p.Indicator_Code,p.Indicator_Name
        ,SUM(Numerator) As Numerator
        ,Sum(Denominator) As Denominator
        ,Business_Unit
        From(
        Select Financial_Year,Financial_Month,Cast(DATEADD(d,-day(Report_Date)+1,Report_Date) As Date) As Report_Date
        ,Indicator_Code,Indicator_Name
        ,Numerator
        ,Denominator
        ,Case when Site_Code in ('RALC7','RVLC7') then 'RVLC7'
              when Site_Code in ('RVL01','RAL26') then 'RVL01'
              when Site_Code in ('RVL01') then 'RAL01'
              ELSE 'Others' end As Site_Code
        ,Case When Business_Unit IS NULL OR Business_Unit = '' THEN
                    Case when Site_Code in ('RALC7','RVLC7') then 'Chase Farm Hospital'
                   when Site_Code in ('RVL01','RAL26') then 'Barnet Hospital'
                   when Site_Code in ('RAL01') then 'Royal Free Hospital'
              ELSE 'Others' End
        ELSE
              Case when Business_Unit like 'Royal Free%' Then 'Royal Free Hospital'
                   when Business_Unit like 'Barnet%' Then 'Barnet Hospital'
                   when Business_Unit like 'Chase Farm%' Then 'Chase Farm Hospital'
              Else 'Others' End
        END As Business_Unit
        
        From RF_Indicators.dbo.All_Indicators
        Where Financial_Year >= '2015/2016' 
        ) a
        INNER JOIN (Select Indicator_Code,Indicator_Name From RF_Indicators.dbo.Indicators_Metadata) p 
        ON a.Indicator_Code= p.Indicator_Code
        Group by Financial_Year,Financial_Month,Report_Date,p.Indicator_Code,p.Indicator_Name,Business_Unit")

dbDisconnect(myConn) # Disconnect to free up resources


shinyData <- myData%>%
  arrange(Indicator_Code,as.factor(Financial_Month))%>%
  select(Financial_Year,Report_Date,Indicator_Code,Indicator_Name,Month,Numerator,Denominator#,Site_Code
         ,Business_Unit)%>%
  mutate(Performance = ifelse(Denominator <= 0|Denominator==''|is.null(Denominator)|is.na(Denominator)
                               ,Numerator,Numerator/Denominator))%>%
  select(Financial_Year,Month,Report_Date,Indicator_Code,Indicator_Name,Numerator,Denominator,Performance
         ,Business_Unit)


#Exporting imported and cleaned data to the shiny_App folder 
sFldr <- "//netshare-ds3/Performance/Team/Jonathan/Shiny_App/"
sFile <- "shinyData_New_Table.csv"
write_csv(shinyData,file.path(sFldr,sFile)) #write.csv(shinyData,[path]) will also work

# End of Data wrangling


#To export the file as excel instead of csv, use
library(readxl)

sFldr <- "//netshare-ds3/Performance/Team/Jonathan/Shiny_App/"
sFile <- "shinyData.xlsx"
write.xlsx2(shinyData,file.path(sFldr,sFile))
#or u can use the one below but is much slower
write.xlsx(shinyData,file.path(sFldr,sFile))


#importing the exported data back
new_ShinyData <- read_csv(path) #for the csv files
new_ShinyData2 <- read_xlsx(file.path(sFldr,sFile)) #for excel files



