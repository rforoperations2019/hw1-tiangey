#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(stringr)
library(tools)
dat <- read.csv('/Users/tiangeyang/Desktop/City_Employee_Salaries_March_2018.csv')
save(dat,file="dat.RData")
load("dat.RData")


ui <- fluidPage(
    
    titlePanel("CITY SALARIES"),
    sidebarLayout(
        sidebarPanel(
            

            selectInput(inputId = "y", 
                        label = "Y-axis:",
                        choices = c("Annual_Rt", "FID", "Hrly_Rate"), 
                        selected = "Annual_Rt"),
            
 
            selectInput(inputId = "x", 
                        label = "X-axis:",
                        choices = c("Annual_Rt", "FID", "Hrly_Rate"), 
                        selected = "Hrly_Rate"),
            
 
            selectInput(inputId = "z", 
                        label = "Color by:",
                        choices = c("Full_Part", "Reg_Temp"),
                        selected = "Full_Part"),
            
    
            sliderInput(inputId = "alpha", 
                        label = "Alpha:", 
                        min = 0, max = 1, 
                        value = 0.5),
            

            checkboxInput(inputId = "show_data",
                          label = "Show data table",
                          value = TRUE),
            
            checkboxGroupInput(inputId = "selected_type",
                               label = "Select type:",
                               choices = c("ATTOR","AVIA","CATS","CDOT","CLERK","CLTWT","CTMGR","E&PM","ED",
                                           "FIRE","FRRET","H&NS","HR","I&T","M&CC","M&FS","PLAN","POL",  
                                           "S&B","SW"),
                               selected = "name"),
            
            downloadButton("City Employee Salary", "Download")
            
        ),
        
    
        mainPanel(
            
           
            plotOutput(outputId = "scatterplot"),
            
            plotOutput(outputId = "smoothplot"),
            
            plotOutput(outputId = "areaplot"),
           
            DT::dataTableOutput(outputId = "datable")
        )
    )
)


server <- function(input, output) {
    
    dat_subset <- reactive({
        req(input$selected_type) 
        filter(dat, Unit %in% input$selected_type)
    })
    
   
    output$scatterplot <- renderPlot({
        ggplot(data = dat_subset(), aes_string(x = input$x, y = input$y,
                                         color = input$z)) +
            geom_point(alpha = input$alpha) +
            labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
                 y = toTitleCase(str_replace_all(input$y, "_", " ")),
                 color = toTitleCase(str_replace_all(input$z, "_", " ")))
    })
    
    output$smoothplot <- renderPlot({
        ggplot(data = dat_subset(), aes_string(x = input$x, y = input$y,
                                               color = input$z)) +
            geom_smooth(alpha = input$alpha,model = lm) +
            labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
                 y = toTitleCase(str_replace_all(input$y, "_", " ")),
                 color = toTitleCase(str_replace_all(input$z, "_", " ")))
    })
    
    output$areaplot <- renderPlot({
        ggplot(data = dat_subset(), aes_string(x = input$x, y = input$y,
                                               color = input$z)) +
            geom_area(alpha = input$alpha,model = lm) +
            labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
                 y = toTitleCase(str_replace_all(input$y, "_", " ")),
                 color = toTitleCase(str_replace_all(input$z, "_", " ")))
    })
    

   
    output$datable <- DT::renderDataTable(
        if(input$show_data){
            DT::datatable(data = dat[, 1:3], 
                          options = list(pageLength = 10), 
                          rownames = FALSE)
        }
    )
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$dataset, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(datasetInput(), file, row.names = FALSE)
        }
    )
   
}


shinyApp(ui = ui, server = server)