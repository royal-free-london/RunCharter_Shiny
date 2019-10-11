#install.packages("RODBC") or install.packages('RODBC', dependencies=TRUE, repos='http://cran.rstudio.com/')
#install.packages("odbc")
.libPaths()  #this tells packages are installed
?.libPaths()

#install.packages("ggplot2", lib = "C:/Users/ju0d/Documents/R/win-library/3.6")
library(odbc)
library(DBI)
library(runcharter)
library(tidyverse) # tidyverse contains library(readr),library(readxl),library(lubridate),library(stringr),#library(dplyr),library(tidyr)
# and library("ggplot2", lib.loc= "C:/Users/ju0d/Documents/R/win-library/3.6") #library(gglot2) stopped working after upgrade to version 3.6.1


myConn <- dbConnect(odbc::odbc(), "RFH-INFORMATION")
# to specify a particular database, use the below connection
#myConn <- dbConnect(odbc::odbc(), "RFH-INFORMATION",Database  = "RF_Performance")

myData<- dbGetQuery(myConn,
          "Select * From(
              Select FinancialYear,FinancialMonth,Month,[Month-Year],Indicators
              ,RTrim(LEFT(Indicators,charIndex('-',Indicators,1)-1)) As Indicator_Code
              ,Case when Site_Code In ('RAL01','RVL01','RVLC7','RAL26','RALRA') THEN Site_Code_Desc 
              ELSE 'Others' end As SiteDesc,BuisnessUnit As BusinessUnit--,Site_Code,Specialty_Code,Specialty_Desc
              ,sum(Activity) As Activity
              From dbo.RF_Performance_All_Indicators
              Where FinancialYear In ('2015/2016','2016/2017','2017/2018','2018/2019','2019/2020')
              --And [Month-Year] <= 'Jul-19'
              Group by FinancialYear,FinancialMonth,Month,[Month-Year],Indicators
              ,Case when Site_Code In ('RAL01','RVL01','RVLC7','RAL26','RALRA') THEN Site_Code_Desc 
              ELSE 'Others' end,BuisnessUnit--,Site_Code,Specialty_Code,Specialty_Desc
              ) a
                
              INNER JOIN (Select Indicator_Name,Indicator_Code As Metadata_Code From RF_Indicators.dbo.Indicators_Metadata) p 
              ON a.Indicator_Code= p.Metadata_Code
          ")

dbDisconnect(myConn) # Disconnect to free up resources
#View(head(myData))

shinyData <- myData%>%
  mutate(#Indicator_Code2 = trimws(substring(Indicators,1,regexpr("-",Indicators)-1)),
         Metric_name = trimws(substring(Indicators,regexpr(" ",Indicators)+3,nchar(Indicators)))
         ,Num_Den = ifelse(grepl("[\\-]d",str_sub(Indicators,1,8)),"Denominator","Numerator"))%>%
  arrange(Indicator_Code,as.factor(FinancialMonth))%>%
  select(FinancialYear,Indicator_Code,Metric_name,Month,`Month-Year`
         #,Site_Code,Specialty_Code,Specialty_Desc
         ,SiteDesc,Business_Unit = BusinessUnit,Num_Den,Activity)%>%
  spread(key = Num_Den,value = Activity)%>%
  mutate(ReportDate = dmy(str_replace_all(paste0('01-',`Month-Year`),"-"," "))
         ,Performance = ifelse(Denominator <= 0|Denominator==''|is.null(Denominator)|is.na(Denominator)
                               ,Numerator,Numerator/Denominator))%>%
  select(FinancialYear,Month,ReportDate,Metric_name,SiteDesc,Business_Unit,ReportDate,Indicator_Code,Performance)

#nrow(shinyData)
#nrow(myData)
#View(shinyData)
#sFile <- "//netshare-ds3/Performance/Team/R/How-Tos/Runcharter/AllBed_Data.csv" 
#sFile <- "//netshare-ds3/Performance/Projects/Measurement and Reporting Framework/Data/Performance Metadata/PerformanceMeasuresMetadata.xlsx"

#Exporting imported and cleaned data to the shiny_App folder 
sFldr <- "//netshare-ds3/Performance/Team/Jonathan/Shiny_App/"
sFile <- "shinyData_MultiDim.csv"

write_csv(shinyData,file.path(sFldr,sFile)) #write.csv(shinyData,[path]) will also work

#To export the file as excel instead of csv, use
library(readxl)


#importing the exported data back
new_ShinyData <- read_csv(file.path(sFldr,sFile)) #for the csv files
nrow(new_ShinyData)
nrow(shinyData)
View(new_ShinyData)


RunData <- new_ShinyData%>%
  #filter(Indicator_Code %in% c('LT036a'))%>%
  arrange(ReportDate)%>%
  mutate(grp2= paste(Metric_name,SiteDesc,Business_Unit,sep='_'))%>%
  #mutate(grp2= grp)%>%
  select(date=ReportDate,grp=grp2,y=Performance)%>% 
  runcharter(grpvar="grp",datecol="date",yval="y",
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



#columns <- quos(a,b,c)
columns <- quos("Indicator Name", "Business Unit", "Hospital Site")

grp=paste(!!!columns, sep="_")



RunData$sustained%>%
  separate(grp,sapply(columns,quo_name),sep="_") %>%
  View()

?split
?str_split
# utilizing the output of the run (i.e sustained)

RunData$sustained%>%
  group_by(grp,new_median,start-run_end)%>%
  summarize(shift=max(month(run_start)))


RunData$sustained%>%
  group_by(grp,new_median,run_start,run_end)%>%
  mutate(differ = run_start - run_end)%>%
  select(differ)
summarize(shift=max(month(run_start)))

RunData$sustained%>%
  mutate(`months since start of shift` = extend_to - start_date)%>%
  select(month(`months since start of shift`))



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
#test
