library(shiny)
library(dplyr)
library(ggplot2)

# Generate the population data
appenv <- new.env()

appenv$total_population <- 1e6
appenv$population <- NULL
appenv$sample_data <- NULL

plot_style <- theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold")
)

# UI
ui <- fluidPage(
    titlePanel("Sampling Demonstration"),
    fluidRow(
        column(6, plotOutput("population_plot")), # Left plot
        column(6, plotOutput("sample_plot")) # Right plot
    ),
    fluidRow(
        column(2, offset = 1, numericInput("pop_mean", "Population mean", value = 100, width = 150)),
        column(2, numericInput("pop_sd", "Population SD", value = 5, min = 0.1, max = 100, width = 150)),
        column(2, offset = 2, numericInput("n_sample", "Sample size", value = 10, min = 1, max = 1000, width = 150))
    ),
    fluidRow(
        column(12,
            align = "center",
            actionButton("sample_button", "Pick another sample")
        ) # Button centered underneath the right plot
    )
)

# Server
server <- function(input, output) {
    # Render the population histogram
    output$population_plot <- renderPlot({
        appenv$population <- data.frame(Value = rnorm(appenv$total_population,
            mean = input$pop_mean,
            sd = input$pop_sd
        ))

        ggplot(appenv$population, aes(Value)) +
            geom_histogram(
                binwidth = input$pop_sd / 5, 
                col = "black",
                fill = rgb(0.1, 0.7, 0.8)
            ) +
            ylab("Number of observations") +
            xlim(input$pop_mean - 5 * input$pop_sd, input$pop_mean + 5 * input$pop_sd) +
            ggtitle("Population (n = 1,000,000)") +
            plot_style
    })

    # Render the sample histogram
    output$sample_plot <- renderPlot({
        req(appenv$population)

        # Make the plot dependent on the sample_button button
        input$sample_button

        appenv$sample_data <- appenv$population %>%
            sample_n(input$n_sample)

        appenv$sample_data %>%
            {
                ggplot(., aes(Value)) +
                    geom_histogram(
                        binwidth = input$pop_sd / 5, 
                        col = "black",
                        fill = rgb(0.1, 0.7, 0.8)
                    ) +
                    ylab("Number of observations") +
                    xlim(input$pop_mean - 5 * input$pop_sd, input$pop_mean + 5 * input$pop_sd) +
                    ylim(0, 5) +
                    labs(title = paste0(
                        "Sample - (n = ", nrow(.), ") - ",
                        "Mean: ", round(mean(.$Value), 1),
                        ", SD: ", round(sd(.$Value), 1)
                    )) +
                    plot_style
            }
    })
}

# Run the Shiny app
shinyApp(ui, server)
