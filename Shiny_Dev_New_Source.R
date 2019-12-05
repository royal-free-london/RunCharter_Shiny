#install.packages("RODBC") or install.packages('RODBC', dependencies=TRUE, repos='http://cran.rstudio.com/')
#install.packages("odbc")
#install.packages("ggplot2", lib = "C:/Users/ju0d/Documents/R/win-library/3.6")

library(odbc)
#library(DBI)
library(tidyverse) # tidyverse contains library(readr),library(readxl),library(lubridate),library(stringr),#library(dplyr),library(tidyr)
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
  select(Financial_Year,Month,Report_Date,Indicator_Code,Indicator_Name,Numerator,Denominator,Performance#,Site_Code
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

head(new_ShinyData2)


RunData <- new_ShinyData2%>%
  filter(Indicator_Code %in% c('LT036a'))%>%
  arrange(ReportDate)%>%
  select(y=Performance,date=ReportDate,grp=Metric_name)%>%
  filter(date >= '2015-04-01')%>%
  runcharter(
    med_rows = 12,
    runlength = 8,
    chart_title = "Analysis of RFL Performance",
    chart_subtitle = "Performance Report",
    direction = "both",#direction can be "below", "above" or "both"
    facet_cols = 1
    #line_colr = "#005EB8",    # blue
    #point_colr ="#005EB8",    # blue
    # median_colr = "#E87722",  # orange
    # sus_fill = "#DB1884",     # magenta
  )
RunData 

# end of runcharter script

View(RunData$sustained)


# utilizing the output of the run (i.e sustained)

RunData$sustained%>%
  group_by(grp,new_median,run_start-run_end)%>%
  summarize(shift=max(month(run_start)))


RunData$sustained%>%
  group_by(grp,new_median,run_start,run_end)%>%
  mutate(differ = run_start - run_end)%>%
  select(differ)
summarize(shift=max(month(run_start)))

max(RunData$sustained[,5])
min(RunData$sustained[,5])
nrow(RunData$sustained)

total_rec <- nrow(RunData$sustained)
RunData$sustained%>%
  select(new_median,run_start,run_end)%>%
  mutate(total_rec)%>%
  filter(run_start[3])


RunData$sustained[num,6]
RunData$sustained[num-1,6]
num<-nrow(RunData$sustained)
test <- ifelse(RunData$sustained[num,6]>RunData$sustained[num-1,6],"Outlier","Great")
test  


rm(zeros)

View(myData)
