library(shiny)
library(dplyr)
library(ggplot2)

# Generate the population data
total_population <- 1e6
population_mean <- 100
population_sd <- 5
n_sample <- 10

population <- data.frame(Value = rnorm(total_population,
    mean = population_mean,
    sd = population_sd
))

rv <- reactiveValues(sample_data = population %>%
    sample_n(n_sample))

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
        ggplot(population, aes(Value)) +
            geom_histogram(
                binwidth = 2.5, col = "black",
                fill = rgb(0.1, 0.7, 0.8)
            ) +
            ylab("Number of observations") +
            xlim(75, 125) +
            ggtitle("Population - (n = 1,000,000) - Mean: 100, SD: 5") +
            plot_style
    })

    # Render the sample histogram
    output$sample_plot <- renderPlot({
        rv$sample_data %>%
            {
                ggplot(., aes(Value)) +
                    geom_histogram(
                        binwidth = 2.5, col = "black",
                        fill = rgb(0.1, 0.7, 0.8)
                    ) +
                    ylab("Number of observations") +
                    xlim(75, 125) +
                    ylim(0, 5) +
                    labs(title = paste0(
                        "Sample - (n = ", nrow(.), ") - ",
                        "Mean: ", round(mean(.$Value), 1),
                        ", SD: ", round(sd(.$Value), 1)
                    )) +
                    plot_style
            }
    })

    # Refresh the sample when the button is pressed
    observeEvent(input$sample_button, {
        rv$sample_data <- population %>%
            sample_n(n_sample)
    })
}

# Run the Shiny app
shinyApp(ui, server)
