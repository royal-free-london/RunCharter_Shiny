
# You will need to install runcharter if not already installsed
#install.packages("devtools")
#devtools::install_github("johnmackintosh/runcharter")

#installations and data manipulation
#install.packages("shiny")
library(shiny)
#install.packages("colourpicker")
library(colourpicker)
library(runcharter)
library(tidyverse)
library(plyr) #for the ddply function

#importing the exported data back
sFldr <- "//netshare-ds3/Performance/Team/Jonathan/Shiny_App/"
#sFile <- "shinyData.csv"
sFile <- "shinyData_New_Table.csv" #This  uses the RF_Inidcators database
path <- file.path(sFldr,sFile)
shinyData <- read_csv(path) # see Shiny_Dev.R for the import script from SQL Server

#data for for run chart
main_data <- shinyData%>%
  select(date=Report_Date,grp=Indicator_Name,y=Performance)%>% 
  arrange(grp,date)

# 
# shinyData%>%
#   filter(Indicator_Code == 'SQU05a')%>%
#   group_by(Month)


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
                  min = round(min(year(main_data$date)),0), 
                  max = round(max(year(main_data$date)),0),
                  value = c(2015,2019)),
      
      
      selectInput(inputId = "grp",
                  label = "Indicators",
                  choices = levels(as.factor(main_data$grp)),
                  #choices = levels(as.factor(trimws(substring(data$grp,1,regexpr("_",data$grp)-1)))),
                  multiple = FALSE,
                  selected = levels(as.factor(main_data$grp))[2]),
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
)


# Define the server logic

server <- function(input, output) {
  
  
  output$plot <- renderPlot({
    
    plot_data<- filter(main_data,
                       grp %in% input$grp &
                         year(date) >= input$date[1] & year(date) <= input$date[2])
    
    med_rows <- input$num1
    runlength <- input$num2
    chart_title <- "Analysis of RFL Performance"
    line_colr <- input$color
    median_colr <- input$color2
    
    plot <- plot_data%>%
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
    data1<- filter(main_data,
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
      runcharter(grpvar="grp",datecol="date",yval="y"
                 ,med_rows=12,
                 runlength=8 
                 ,direction = "both"
      )
    #shift_data$sustained
    
    #function to generate shifts data
    shift_fun <- function(shift_data){ 
      
      period_to_check <- input$num_run
      
      shifts<-shift_data$sustained%>%
        group_by(grp)%>%
        arrange(grp,desc(end_date))%>%
        top_n(1,end_date)#%>%            # To test if this will also select the first record in each group
      #ddply(grp,head,1) # this will select the first record in each group
      
      shifts%>%
        mutate(Month_runs = interval(start=end_date,end=extend_to)%/%months(1))%>%
        filter(Month_runs<=period_to_check)%>%
        mutate(shift_Start=end_date-months(input$num2)+months(1))%>%
        select(`Indicator Name`=grp,`Start of Shift` = shift_Start,`End of Shift`= end_date,`Month since end of shift` = Month_runs)
    }
    
    shift_fun(shift_data)
  })
}



#Run the shiny App'

shinyApp(ui = ui,server= server)


