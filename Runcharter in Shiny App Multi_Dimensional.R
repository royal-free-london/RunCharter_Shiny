
# You will need to install runcharter if not already installsed
#install.packages("devtools")
#devtools::install_github("johnmackintosh/runcharter")

library(tidyverse)

#importing the exported data back
#sFldr <- "//netshare-ds3/Performance/Team/R/How-Tos/Runcharter/"
sFldr <- "//netshare-ds3/Performance/Team/Jonathan/Shiny_App/"
sFile <- "shinyData_MultiDim.csv"
path <- file.path(sFldr,sFile)
shinyData <- read_csv(path) # see Shiny_Dev.R for the import script from SQL Server
#View(shinyData)
#nrow(shinyData)

#data for for run chart
# main_data <- signals%>%
# select(date=date,grp=grp,y=y)%>%
# arrange(grp,date)

#dimensions <- ifelse(is.na(shinyData$Business_Unit),paste(shinyData$Metric_name,shinyData$SiteDesc)
#,paste(shinyData$Metric_name,shinyData$Business_Unit))
#dimensions
#data for for run chart
main_data <- shinyData%>%
  mutate(grp2= paste(Metric_name,SiteDesc,Business_Unit,sep='_'))%>% # change
  #mutate(grp2 = ifelse(is.na(shinyData$Business_Unit),paste(shinyData$Metric_name,shinyData$SiteDesc,sep = "_")
  #,paste(shinyData$Metric_name,shinyData$Business_Unit,sep = "_")))%>%
  #mutate(grp2= grp)%>%
  select(date=ReportDate,grp=grp2,Metric_name,y=Performance)%>% 
  #filter(year(date)>=2018)%>%
  arrange(grp,date)
#View(main_data)


#shiny dashboard'
RuncharterShiny <- function(p_data,n = 1,shift_check=FALSE,...){
  #installations and data manipulation
  #install.packages("shiny")
  library(shiny)
  
  #install.packages("colourpicker")
  library(colourpicker)
  library(runcharter)
  library(tidyverse)
  library(plyr) #for the ddply function
  
  
  shinyApp(
    
    #set up the App user interface  
    ui <- fluidPage(
      sidebarLayout(
        sidebarPanel(
          textInput("title", "Title", "RFL Performance Measurement: Signal Finder"),
          #checkboxInput("run", "Use run_start", FALSE),
          
          
          sliderInput(inputId = "num_run", 
                      label = "Period to check",
                      min = 1, 
                      max = 12,
                      value = 3),
          
          sliderInput(inputId = "date", 
                      label = "Select Period of report",
                      min = round(as.integer(min(year(p_data$date))),0), 
                      max = round(as.integer(max(year(p_data$date))),0),
                      value = c(2015,2019)),
          
          
          selectInput(inputId = "grp",
                      label = "Indicators",
                      choices = levels(as.factor(p_data$grp)),
                      #choices = levels(as.factor(trimws(substring(data$grp,1,regexpr("_",data$grp)-1)))),
                      multiple = FALSE,
                      selected = levels(as.factor(p_data$grp))[2]),
          #selected = levels(as.factor(trimws(substring(data$grp,1,regexpr("_",data$grp)-1))))[2]), #this can be use to separate the metric name
          
          numericInput("num1", "Median rows", 12,1),
          numericInput("num2", "Number of runs", 8,1),
          colourInput("color", "Line color", value = "blue"),
          colourInput("color2", "Median color", value = "#FFAE00")
        ),
        
        mainPanel(
          #"Make the plot 600 pixels wide and 800 pixels wide"
          tabsetPanel(type = "tab",
                      tabPanel("Run Chart",plotOutput("plot", height = 600, width = 800)),
                      tabPanel("Sustained Data (Ouput)",dataTableOutput("table2")),
                      tabPanel("Original Data (Input)", dataTableOutput("table"))
                      
          )  
        )
      )
    ),
    
    
    # Define the server logic
    server <- function(input, output) {
      
      
      output$plot <- renderPlot({
        
        plot_data<- filter(p_data,
                           grp %in% input$grp &
                             year(date) >= input$date[1] & year(date) <= input$date[2])
        
        
        #num <- round(length(levels(as.factor(p_data$grp)))/2)
        
        med_rows <- input$num1
        runlength <- input$num2
        chart_title <- "Analysis of RFL Performance"
        line_colr <- input$color
        median_colr <- input$color2
        
        plot <- plot_data%>%
          separate(grp,c("grp", NA,NA),sep="_") %>% #this can be used to separate out the metric name
          group_by(grp)%>%
          runcharter(grpvar="grp",datecol="date",yval="y"
                     ,med_rows = med_rows
                     ,runlength = runlength
                     ,chart_title = chart_title
                     ,chart_subtitle = "Performance Report"
                     ,direction = "both" #direction can be "below", "above" or "both"
                     ,facet_cols = 2 #num
                     ,line_colr = line_colr #"#005EB8",    # blue
                     #point_colr ="red" ##005EB8",    # blue
                     ,median_colr = median_colr #"#E87722",  # orange
                     ,sus_fill = "#DB1884"     # magenta
          )
        plot
        
      })
      
      output$print <- renderText({
        if (shift_check==TRUE){
          print("Sustained Output")
        }
        
        
      })
      
      output$table <- renderDataTable({
        data1<- filter(p_data,
                       grp %in% input$grp &  # to apply reactive filter
                         #grp %in% grp & #all metrics will  show in the table
                         year(date) >= input$date[1] & year(date) <= input$date[2])%>%
          select(Date=date,Indicator_Name = grp, Performance = y)
        data1
      })
      
      output$table2 <- renderDataTable({
        
        # data for shifts starts
        shift_data <- main_data %>%
          filter(grp %in% grp & #all metrics will  show in the table
                   #year(date) >= 2014 & year(date) <= 2019)%>%
                   year(date) >= input$date[1] & year(date) <= input$date[2])%>%
          #separate(grp,c("Indicator Name", "Hospital Site","Business Unit"),sep="_") %>% #this can be used to separate out the metric name
          runcharter(grpvar="grp",datecol="date",yval="y"
                     ,med_rows=12,
                     runlength=8 
                     ,direction = "both"
          )
        #shift_data$sustained
        
        #function to generate shifts data
        shift_fun <- function(shift_data,run_choice=FALSE){ 
          
          period_to_check <- input$num_run
          
          shifts<-shift_data$sustained%>%
            #RunData$sustained%>%
            separate(grp,c("Indicator Name", "Hospital Site","Business Unit"),sep="_") %>%
            group_by(`Indicator Name`)%>%
            arrange(`Indicator Name`,desc(end_date))%>%
            top_n(1,end_date)#%>%            # To test if this will also select the first record in each group
          #ddply("`Indicator Name`",head,1) # this will select the first record in each group
          
          if(run_choice==TRUE){
            ## Using run_start
            
            shifts%>%
              #mutate(Day_runs=extend_to - start_date)%>%
              mutate(Month_runs = interval(start=start_date,end=extend_to)%/%months(1))%>%
              filter(Month_runs <=period_to_check)%>%
              mutate(shift_Start=end_date-months(input$num2)+months(1))%>%
              select(`Indicator Name`,`Hospital Site`,`Business Unit`,`Start of Shift`= shift_start,`End of shift` = end_date,`Month since start of shift`= Month_runs)
            
          } else {
            ## Using run_end -- This makes it easier to test shift for immediate last month
            
            shifts%>%
              #mutate(Day_runs=extend_to-end_date)%>%
              mutate(Month_runs = interval(start=end_date,end=extend_to)%/%months(1))%>%
              filter(Month_runs<=period_to_check)%>%
              mutate(shift_Start=end_date-months(input$num2)+months(1))%>%
              select(`Indicator Name`,`Hospital Site`,`Business Unit`,`Start of Shift` = shift_Start,`End of Shift`= end_date,`Month since end of shift` = Month_runs)
            
          }
          
        }
        
        if (shift_check == TRUE){
          shift_fun(shift_data
                    #,run_choice = input$run
          )
        }
        
      })
    }
  )
}


RuncharterShiny(p_data=main_data
                ,shift_check = TRUE
)



# 
# 
# # to transform the sustained data
# 
# #columns <- quos(a,b,c)
# new_columns <- quos("Indicator Name", "Business Unit", "Hospital Site")
# 
# #grp=paste(!!!columns, sep="_")
# 
# RunData$sustained%>%
#   separate(grp,sapply(new_columns,quo_name),sep="_") %>%
#   View()
# 
# #this columns names can also be supplied as below:
# RunData$sustained%>%
#   separate(grp,c("Indicator Name", "Business Unit", "Hospital Site"),sep="_") %>%
#   View()
#  
# #You can use NA to suppress any unwanted columns:
# #this columns names can also be supplied as below:
# RunData$sustained%>%
#   separate(grp,c("Indicator Name", NA, "Hospital Site"),sep="_") %>%
#   View()








