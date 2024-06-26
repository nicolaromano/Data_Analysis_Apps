library(shiny)
library(shinyvalidate)
library(dplyr)
library(ggplot2)

rv <- reactiveValues(
    population = rnorm(1e6, 100, 5)
)

get_sampling_means <- function(population, sample_size, n_samples) {
        sample_means <- sapply(1:n_samples, function(i) {
            smp <- sample(population, sample_size)
            mean(smp)
        })

        return(data.frame(sample_means = sample_means))
    }

# UI
ui <- fluidPage(
    titlePanel("The sampling distribution and the Central Limit Theorem"),
    tags$head(
        HTML("<link rel='stylesheet' type='text/css' href='styles.css'>
            <link href='https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap' rel='stylesheet'>")
        ),
    HTML("<div class='description'>This app can help you revise the concept of sampling distribution and the Central Limit Theorem.<br />The app will <ol><li>Generate a population of 1 million individuals from a distribution of your choice.</li><li>Take a number of samples of a given size from this population</li><li>Plot the distribution of the means of the samples (called the <strong>sampling distribution</strong>).</li></ol><div align='center'>See how varying the sample size and/or the number of samples affects the sampling distribution.</div></div><br />"),
    fluidRow(
        column(
            width = 4,
            selectInput(
                "distribution",
                "Select a distribution",
                choices = c(
                    "Normal", "Uniform", "Exponential",
                    "Bimodal", "Gamma"
                ),
                selected = "Normal"
            )
        ),
        column(
            width = 4,
            numericInput(
                "sample_size",
                "Sample size",
                value = 10,
                min = 2,
                max = 1000
            )
        ),
        column(
            width = 4,
            numericInput(
                "n_samples",
                "Number of samples",
                value = 100,
                min = 2,
                max = 1000
            )
        )
    ), # end fluidRow
    fluidRow(
        column(
            width = 6,
            plotOutput("population_plot")
        ),
        column(
            width = 6,
            plotOutput("sampling_distribution_plot")
        )
    ) # end fluidRow
) # end fluidPage

server <- function(input, output, session) {
    iv <- InputValidator$new()

    my_theme <- theme(
        axis.text = element_text(family = "Roboto", size = 14),
        axis.title = element_text(family = "Roboto", size = 18),
        plot.title = element_text(family = "Roboto", size = 20,
            face = "italic")
    )

    iv$add_rule("sample_size", sv_between(2, 1000))
    iv$add_rule("n_samples", sv_between(2, 1000))
    iv$enable()

    # Update population when distribution is changed
    observeEvent(input$distribution, {
        rv$population <- switch(
            input$distribution,
            "Normal" = rnorm(1e6, 100, 5),
            "Uniform" = runif(1e6, 70, 200),
            "Exponential" = rexp(1e6, 1/50),
            "Bimodal" = c(
                rnorm(5e5, 80, 5),
                rnorm(5e5, 120, 5)
            ),
            "Gamma" = rgamma(1e6, 2, 50) * 1000
        )
    })

    output$population_plot <- renderPlot({
        rv$population %>%
            data.frame() %>%
            ggplot(aes(x = .)) +
            geom_density(aes(y = after_stat(density)),
                bins = 50,
                col = "black",
                fill = rgb(0.1, 0.7, 0.8)
            ) +
            labs(
                x = "Value",
                y = "Density"
            ) +
            ggtitle("Distribution of the value in the population") +
            annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1,
                label = paste("Mean:", format(mean(rv$population), digits=2), "- SD:", format(sd(rv$population), digits=2)),
                family = "Roboto", size = 6
            ) +
            my_theme
    })

    output$sampling_distribution_plot <- renderPlot(
        {
        req(iv$is_valid())

        sampling_means <- get_sampling_means(rv$population,
            input$sample_size,
            input$n_samples) 
            
        sampling_means %>%
            slice(1:input$n_samples) %>%
            ggplot(aes(x = sample_means)) +
            geom_histogram(aes(y = after_stat(density)),
                bins = 50,
                col = "black",
                fill = rgb(0.1, 0.7, 0.8)
            ) +
            labs(
                x = "Sample mean",
                y = "Density"
            ) +
            ggtitle(paste("Distribution of the means of", input$n_samples, "samples of size", input$sample_size)) +
            annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1,
                label = paste("Mean:", format(mean(sampling_means$sample_means), digits=2), "- SD:", format(sd(sampling_means$sample_means), digits=2)),
                family = "Roboto", size = 6
            ) +
            my_theme
    })
}

# Run the Shiny app
shinyApp(ui, server)
