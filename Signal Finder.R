
# You will need to install runcharter if not already installsed
#install.packages("devtools")
devtools::install_github("johnmackintosh/runcharter")

#package installations 
#install.packages("shiny")
#install.packages("colourpicker")
library(shiny)
library(colourpicker)
library(lubridate)
library(runcharter, "lib.loc = C:/Users/ju0d/Documents/R/win-library/3.6")
library(tidyverse)


#importing the exported data back
sFldr <- "//netshare-ds3/Performance/Team/Jonathan/Shiny_App/"
sFile <- "shinyData_New_Table.csv" #This  uses the RF_Inidcators database
path <- file.path(sFldr,sFile)
shinyData <- read_csv(path) # see Data_Wrangling.R for the import script from SQL Server

main_data <- shinyData%>%
    mutate(grp2= paste(Indicator_Name,Business_Unit,sep='_'))%>% # change
    select(date=Report_Date,grp=grp2,y=Performance)%>% 
    arrange(grp,date)
  

#set up the App user interface  
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "RFL Performance Measurement: Signal Finder"),
      #checkboxInput("run", "Use run_start", FALSE),
      
      
      sliderInput(inputId = "period", 
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
                  multiple = TRUE, #FALSE,
                  selected = levels(as.factor(main_data$grp))[2]),
    
      numericInput("num1", "Median rows", 12,1),
      numericInput("num2", "Number of runs", 8,1),
      #numericInput("point_size", "Point Size", 4,1),
      #numericInput("highlight_point_size", "Highlight Point Size", 4,1),
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


#function to generate shifts data
shift_fun <- function(shift_data,period_to_check,run){ 
  
 shifts<-shift_data$sustained%>%
    arrange(grp,desc(end_date))%>%
    separate(grp,c("Indicator_Name","Business_Unit"),sep="_")%>%
    group_by(Indicator_Name)%>%
    top_n(1,end_date)       # This will select the first record in each group
  
 shifts%>%
    mutate(Month_runs = interval(start=end_date,end=extend_to)%/%months(1))%>%
    filter(Month_runs<=period_to_check)%>%
    mutate(shift_Start=end_date-months(run)+months(1))%>%
    select(Indicator_Name,Business_Unit,`Start of Shift` = shift_Start,`End of Shift`= end_date,`Month since end of shift` = Month_runs)
  }


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
    #point_size <- input$point_size
    #highlight_point_size <- input$highlight_point_size
    
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
                 ,point_size = 3.5 #point_size
                 ,highlight_point_size = 4 #highlight_point_size
                 ,line_size = 1
                 
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
    
    
    #function call to generate shifts data
    
    shift_fun(shift_data,period_to_check=input$period,run=input$num2)
        
    })
}


#Run the shiny App'
shinyApp(ui = ui,server= server)

#End
