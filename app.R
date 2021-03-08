#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

list.of.packages <-
    c("shiny",
      "plotly",
      "tictoc",
      "shinycssloaders",
      "magrittr",
      "dplyr",
      "data.table")

new.packages <-
    list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
    install.packages(new.packages)

library(shiny)
library(plotly)
library(tictoc)
library(shinycssloaders)
library(magrittr)
library(dplyr)
library(data.table)

# Define UI for the application
ui <- fluidPage(
    
    # number of points per feature
    sliderInput(
      "points",
      "# Points per Feature",
      min = 1,
      max = 1000000,
      value = 100000,
      step = 50000
    ),
    # number of features to plot (plot range of cached features)
    sliderInput(
        "range",
        "Features Plotting Range <start:end>",
        min = 1,
        max = 100,
        value = c(1, 10)
    ),
    # number of features cached
    sliderInput(
        "cached",
        "# Cached Features",
        min = 1,
        max = 1000,
        value = 50,
        step = 50
    ),
    # plot with lines
    checkboxInput("lines",
                  "Lines",
                  value = TRUE),
    # plot with markers
    checkboxInput("markers",
                  "Markers",
                  value = TRUE),
    mainPanel(plotlyOutput("plotlyplot", height = '100%') %>% withSpinner())
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    my_range <- reactive({
        c(input$range[1], input$range[2])
    })
    
    my_points <- reactive({
        input$points[1]
    })
    
    my_cached <- reactive({
        input$cached[1]
    })
    
    my_markers <- reactive({
        input$markers[1]
    })
    
    my_lines <- reactive({
        input$lines[1]
    })
    
    generatePlotDF <- reactive({
        tic(msg = "generate cached timeseries")
        size <- my_points()
        start <- 1
        end <- my_cached()
        cached <- data.frame(date = seq(1:size))
        
        for (i in seq(start:end)) {
            cached[paste0("y", i)] <- rbinom(size, 100, 0.5)
        }
        toc()
        cached
    })
    
    
    output$plotlyplot <- renderPlotly({
        size <- my_points()
        start <- as.integer(my_range()[1])
        end <- as.integer(my_range()[2])
        nr <- end - start
        
        fig <- generatePlotDF()[, start:end]
        fig %<>% tidyr::gather(variable, value, -date)
        fig  %<>% transform(id = as.integer(factor(variable)))
        fig %<>% as.data.table(fig)
       
         # data rerieval.
        tic(msg = "Data Plotting")
        
        n_fig <- nr
        
        plot_size <- 200
        
        mode <- ""
        
        if (my_lines() == T && my_markers() == T) {
            mode <- "lines+markers"
        } else if (my_lines() == T && my_markers() == F) {
            mode <- "lines"
        } else{
            mode <- "markers"
        }
        
        
        fig <- fig %>% plot_ly(
            x = ~ date,
            y = ~ value,
            color = ~ variable,
            height = nr * plot_size,
            type = 'scattergl',
            #hoverinfo = 'none',
            mode = mode,
            yaxis = ~ paste0("y", id)
        ) %>% config(displayModeBar  = FALSE)
        
        
        fig <-
            fig %>% subplot(
                nrows = n_fig,
                shareX = TRUE,
                titleY = T,
                titleX = T,
                margin = 0
            )
        
        toc()
        fig
        
    })
}

# Run the application

shinyApp(ui = ui, server = server)
