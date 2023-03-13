#---
#title: "HW2: Shiny Portfolio Project"
#author: "Emma Chen"
#date: "2023-03-13"
#---


# load packages
library(tidyverse)
library(shiny)
library(plotly)
theme_set(theme_bw())


## Read and Clean Dataset

# read data set (retrieved from UCI Machine Learning Repository: https://archive.ics.uci.edu/ml/datasets/student+performance)
por <- read.table("https://github.com/wxuan309/stat436-hw2/raw/main/student/student-por.csv"
                  , sep=";", header=TRUE) %>% 
  # Multiply the score for the first, second, and final grade period 
  # by 5 to have the total grade as 100.
  mutate(G1 = G1*5,
         G2 = G2*5,
         G3 = G3*5) %>% 
  select(c(sex, age, address, studytime, schoolsup, absences, G1, G2, G3))

# modify values in the address column (U to urban and R to rural)
por$address[por$address == 'U'] <- 'urban'
por$address[por$address == 'R'] <- 'rural'

# modify the values in the studytime column to its represented hours
# 1 - <2 hours, 2 - 2 to 5 hours, 3 - 5 to 10 hours, or 4 - >10 hours
time <- c("<2 hrs", "2-5 hrs", "5-10 hrs", ">10 hrs")
for (i in (1:4)){
  por$studytime[por$studytime == i] <- time[i]
}

# modify column names
colnames(por)[4] <- "study time (hrs/week)"
colnames(por)[5] <- "school_support"
colnames(por)[6] <- "absences (days)"

# color code for later use
cols <- c("#023859", "#99ADBF")




## Shiny app

text1 = "Retrieved from UCI Machine Learning Repository, the dataset documents students’ performance in Portuguese language. It includes demographic, personal, and social features that may play a role in their achievement. Each observations represent a student and includes both numeric and categorical data. It has no missing values and the three response variables are: G1, G2, and G3, representing a student’s performance for term 1, term 2, and term 3."
text2 = "Below, the age slide bar allows users to select the desired age range. The histogram displays the grade distribution of the students' performance on the final term and enables users to select the desired grade range. The graph on the right hand side (absences vs study time) and the  table below change according to the selected age and grade range."
ui <- fluidPage(
  titlePanel("Students' Performance during the 2005-2006 school year in Portugal"),
  mainPanel(text1),
  br(),
  mainPanel(text2),
  sliderInput("age", "Age", 15, 22, c(15, 22), sep = ""),
  fluidRow(
    h4("Grade Distribution"),
    column(4, plotOutput("histogram", brush = brushOpts("plot_brush", direction = "x"))),
    column(8, plotlyOutput("boxplot")),
  ),
  dataTableOutput("table")
)
server <- function(input, output) {
  # setup the brush interaction
  selected <- reactiveVal(rep(TRUE, nrow(por)))
  observeEvent(
    input$plot_brush, {
      selected(brushedPoints(por, input$plot_brush, allRows = TRUE)$selected_)
    }
  )
  output$boxplot <- renderPlotly({
    current <- por %>%
      filter(selected() & age >= input$age[1] & age <= input$age[2])
    p <- ggplot(current, aes(x = factor(`study time (hrs/week)`, level=time), `absences (days)`)) +
      geom_boxplot(color=cols[1], fill=cols[1], alpha=0.3) +
      xlab("study time (hrs/week)") +
      ylim(c(0,35))
    ggplotly(p)
  })
  
  output$histogram <- renderPlot({
    ggplot(por, aes(G3)) +
      geom_histogram(fill = cols[2]) +
      geom_histogram(data = filter(por, selected()), fill = cols[1]) +
      xlab("Final grade") +
      ylab("number of students")
  })
  
  output$table <- renderDataTable({
    por %>%
      filter(selected() & age >= input$age[1] & age <= input$age[2])
  })
}

shinyApp(ui, server)

