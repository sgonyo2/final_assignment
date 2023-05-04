
library(haven)
library(labelled)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(broom)
library(shinycssloaders)
library(stringr)
library(tidyverse)

###############
# Import data #
###############
data_subset <- read_rds(file = "./data/processed/data_subset.rds")

####################
# Create Shiny App #
####################

intro_text <- list("The purpose of this app is to understand attitudes towards gender roles and immigration using data from the 2017 European Value Study (EVS)",
                   "Select a country and input and outcome variables to explore their relationships",
                   "The EVS is a large-scale, cross-national and longitudinal survey research on how Europeans think about family, work, religion, politics, and society.",
                   "For more information, visit https://search.gesis.org/research_data/ZA7500#variables%7Cexploredata-ZA7500_Varv80%7C0%7Cvariable_order%7Casc%7Cv80")

#UI
ui <- dashboardPage(
  
  dashboardHeader(title = "European Value Study"),
  
  #Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "intro"),
      menuItem("Data Exploration", tabName = "exploration"),
      menuItem("Regression Results", tabName = "regression"),
      selectInput("country",
                  label = "Select a country",
                  choices = country),
      radioButtons("outcome",
                   label = "Select an outcome variable",
                   choices = c("Job Priority" = "job_priority", "Child Suffers" = "child_suffer")),
      checkboxGroupInput("control",
                         label = "Select control(s)",
                         choices = c("Education" = "education", "Sex" = "male")),
      numericInput("age_poly",
                   label = "Select age polynomials",
                   value = 1,
                   min = 1,
                   max = 5),
      downloadButton("report", "Generate report")
    )
  ),
  
  #Body content
  dashboardBody(tabItems(
    tabItem(tabName = "intro",
            h2("Overview of the app"),
            map(intro_text, p)),

    tabItem(tabName = "exploration",
            h2("Data Exploration"),
            withSpinner(plotOutput("outcome_plot"))),
    
    tabItem(tabName = "regression",
            h2("Regression Results"),
            fluidRow(
              h3("Model Summary"),
              withSpinner(dataTableOutput("model")),
              h3("Residuals Plot"),
              withSpinner(plotOutput("residuals"))
      )
    )
  )
  )
)

#Server
server <- function(input, output, session) {
  data <- reactive(if (input$country == "Overall")
  {
    data_subset
  } else{
    data_subset %>%
      filter(country == input$country)
  })
  
  #########
  # Tab 1 #
  #########
  
  output$outcome_plot <- renderPlot({
    if (input$outcome == "child_suffer") {
      data() %>%
        ggplot(aes(
          x = age,
          y = child_suffer,
          color = as.factor(education)
        )) +
        geom_line(stat = "summary", fun = "mean") +
        labs(color = "Education", x = "Age", y = "When a mother works for pay, the children suffer") +
        facet_wrap( ~ male,
                    ncol = 1,
                    labeller = label_value) +
        ggtitle(paste0(input$country, " Results"))
      
    } else{
      data() %>%
        ggplot(aes(
          x = age,
          y = job_priority,
          color = as.factor(education)
        )) +
        geom_line(stat = "summary", fun = "mean") +
        labs(color = "Education", x = "Age", y = "Employment priority to nationals over immigrants") +
        facet_wrap( ~ male,
                    ncol = 1,
                    labeller = label_value) +
        ggtitle(paste0(input$country, " Results"))
    }
  })
  
  
  
  #########
  # Tab 2 #
  #########
  
  model <- reactive({
    if (input$age_poly > 1) {
      
      lm(formula = str_c(input$outcome, " ~ " ,
                         
                         str_c(c(input$control, "+"), collapse = " + "),
                         
                         "poly(age, ", input$age_poly, ")"),
         
         data = data())
    } else {
      
      lm(formula = str_c(input$outcome, " ~ " ,
                         
                         str_c(c(input$control, "+"), collapse = " + "),
                         
                         "poly(age, ", 1, ")"),
         
         data = data())
    }
  })
  
  output$model <- renderDataTable({
    tidy(model())
  })
  
  output$residuals <- renderPlot({
    ggplot(model(), aes(x = .fitted, y = .resid)) +
      geom_point() +
      geom_hline(yintercept = 0) +
      labs(x = "Residuals", y = "Predicted") +
      ggtitle(paste0(input$country, " Predicted vs Residuals Plot"))
  })
  
  ###################
  # Download Report #
  ###################
  
  output$report <- downloadHandler(
    filename = "dynamic_report.html",
    content = function(file) {
      
      tempReport <- file.path(tempdir(), "dynamic_report.Rmd")
      file.copy("./dynamic_report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <-
        list(
          country = input$country,
          outcome = input$outcome,
          control = input$control,
          age_poly = input$age_poly
        )
      
      # Knit the document, passing in the `params` list, and evaluate
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
