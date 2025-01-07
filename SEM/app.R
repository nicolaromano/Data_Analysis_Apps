library(shiny)
library(dplyr)
library(ggplot2)

plot_style <- theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic")
)

globalenv <- new.env()
globalenv$total_population <- 1e6
globalenv$pop_mean <- 100
globalenv$pop_sd <- 5
globalenv$sample_size <- 10
globalenv$num_experiment <- 10

# UI
ui <- fluidPage(
    tags$head(
        HTML("<link rel='stylesheet' type='text/css' href='styles.css'>
            <link href='https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap' rel='stylesheet'>")
    ),
    titlePanel("The Standard Error of the Mean"),
    conditionalPanel(
        condition = "input.start == 0",
        fluidRow(
            column(
                12,
                HTML(
                    paste("<h2>Introduction</h2>
            <p class='intro'>It's the year 2300 and you are a data scientist on board a spaceship exploring the galaxy. You just discovered a species of plants you've never seen before.</p>
            <p class='intro'>You decide to collect data on the height of these plants, but there are too many, and you don't have time to measure them all!<br />You take a sample of", globalenv$sample_size, "plants and use the sample data to estimate the population mean height.</p>")
                )
            )
        ),
        img(src = "planet.png", class = "planet-img"),
        br(),
        actionButton("start", "START THE MISSION!", class = "btn btn-primary center-block large")
    ), # End of conditionalPanel for the introduction

    conditionalPanel(
        condition = paste("input.start > 0 && input.sample_button <", globalenv$num_experiment),
        fluidRow(
            column(
                12,
                HTML(paste(
                    "<h2>Your task!</h2>
            <p class='intro'>Each time you click the button below, you will take a new sample of", globalenv$sample_size, "plants and measure their heights; the plot will update to show the measured values.</p>
            <p class='intro'><strong>Do you think the mean of the sample is a good estimate of the population mean height?</strong></p>
            <p class='intro'>Let's take several samples and calculate the mean of each sample.</p>
            <p class='intro'><strong>What do you think will happen to the distribution of the sample means as you take more samples?</strong><br />Repeat the process", globalenv$num_experiment, "times and see if you were right!</p>
            <p class='intro'>The table and plot below will update showing the sample means as you take more and more samples.</p>"
                ))
            )
        ),
        fluidRow(
            column(12,
                align = "center",
                actionButton("sample_button", "Sample some plants!", class = "btn btn-primary center-block large")
            )
        ),
        fluidRow(
            column(3, plotOutput("population_plot")), # Population plot
            column(3, plotOutput("sample_plot")), # Sample histogram
            column(3, plotOutput("sample_mean_plot")), # Sample means plot
            column(3, tableOutput("sample_means_table")) # Table of sample means
        )
    ), # End of conditionalPanel for the main content

    conditionalPanel(
        condition = paste("input.sample_button == ", globalenv$num_experiment),
        fluidRow(
            column(
                12,
                htmlOutput("conclusion")
            )
        ),
        fluidRow(
            column(
                width = 4, offset = 4,
                plotOutput("sample_mean_plot_2") # Sample means plot, as above
            )
        ),
        HTML("<p class='intro'>What is the difference between Standard Error of the Mean and Standard Deviation?<br>They are different numbers, but more importantly, they are different concepts.<br />
        The standard deviation measures variability in a sample.<br />
        The standard error of the mean measures how good your estimate of the population mean is")
    ) # End of conditionalPanel for the conclusion,
)

# Server
server <- function(input, output, session) {
    # Generate the population data
    sessionenv <- new.env()

    sessionenv$population <- NULL
    sessionenv$sample_data <- NULL
    sessionenv$sample_means <- NULL

    observeEvent(input$start, {
        # Reset sample data and means when the app starts
        sessionenv$sample_data <- NULL
        sessionenv$sample_means <- NULL
    })

    # Render the population histogram
    output$population_plot <- renderPlot({
        sessionenv$population <- data.frame(Value = rnorm(globalenv$total_population,
            mean = globalenv$pop_mean,
            sd = globalenv$pop_sd
        ))

        ggplot(sessionenv$population, aes(Value)) +
            geom_density(aes(y = after_stat(count)), fill = rgb(0.1, 0.7, 0.8)) +
            xlab("Plant height (cm)") +
            ylab("Number of observations") +
            xlim(globalenv$pop_mean - 5 * globalenv$pop_sd, globalenv$pop_mean + 5 * globalenv$pop_sd) +
            ylim(0, 100000) +
            ggtitle("All plants",
                subtitle = "The height of all plants on the planet.\nYou can't measure that!"
            ) +
            plot_style
    })

    # Take a sample when the "Pick another sample" button is clicked
    observeEvent(input$sample_button, {
        req(sessionenv$population) # Ensure population data exists

        sessionenv$sample_data <- sessionenv$population %>%
            sample_n(globalenv$sample_size)

        sessionenv$sample_means <- c(sessionenv$sample_means, mean(sessionenv$sample_data$Value))

        # For some reason, the sample_means vector is getting one NA value at the start...
        sessionenv$sample_means <- na.omit(sessionenv$sample_means)
    })

    # Render the sample histogram
    output$sample_plot <- renderPlot({
        input$sample_button

        req(sessionenv$sample_data)
        ggplot(sessionenv$sample_data, aes(Value)) +
            geom_histogram(
                binwidth = globalenv$sample_size / 5,
                col = "black",
                fill = rgb(0.1, 0.7, 0.8)
            ) +
            ylab("Number of observations") +
            xlab("Plant height (cm)") +
            xlim(globalenv$pop_mean - 5 * globalenv$pop_sd, globalenv$pop_mean + 5 * globalenv$pop_sd) +
            ylim(0, 5) +
            labs(
                title = paste0(
                    "Your sample - ",
                    "Mean: ", round(mean(sessionenv$sample_data$Value), 1),
                    ", SD: ", round(sd(sessionenv$sample_data$Value), 1)
                ),
                subtitle = "The heights of the plants you measured."
            ) +
            plot_style
    })

    # Render the sample means plot
    output$sample_mean_plot <- renderPlot({
        input$sample_button

        req(sessionenv$sample_means)

        ggplot(aes(x = sessionenv$sample_means), data = data.frame(sessionenv$sample_means)) +
            geom_histogram(
                binwidth = 1,
                col = "black",
                fill = rgb(0.1, 0.7, 0.8)
            ) +
            ylab("Number of observations") +
            xlab("Mean plant height (cm)") +
            xlim(globalenv$pop_mean - 5 * globalenv$pop_sd, globalenv$pop_mean + 5 * globalenv$pop_sd) +
            labs(
                title = paste0("Sample means (", length(sessionenv$sample_means), ifelse(length(sessionenv$sample_means) == 1, " experiment", " experiments"), ")"),
                subtitle = "The distribution of the sample means."
            ) +
            plot_style
    })

    output$sample_mean_plot_2 <- renderPlot({
        input$sample_button

        req(sessionenv$sample_means)

        ggplot(aes(x = sessionenv$sample_means), data = data.frame(sessionenv$sample_means)) +
            geom_histogram(
                binwidth = 1,
                col = "black",
                fill = rgb(0.1, 0.7, 0.8)
            ) +
            ylab("Number of observations") +
            xlab("Mean plant height (cm)") +
            xlim(globalenv$pop_mean - 5 * globalenv$pop_sd, globalenv$pop_mean + 5 * globalenv$pop_sd) +
            labs(
                title = paste0("Sample means (", length(sessionenv$sample_means), ifelse(length(sessionenv$sample_means) == 1, " experiment", " experiments"), ")"),
                subtitle = "The distribution of the sample means."
            ) +
            plot_style
    })

    # Render the table of sample means
    output$sample_means_table <- renderTable({
        input$sample_button

        req(sessionenv$sample_means)

        data.frame(Sample = 1:length(sessionenv$sample_means), Mean = sessionenv$sample_means)
    })

    # Render the conclusion
    output$conclusion <- renderText({
        input$sample_button

        # Debugging
        paste("<h2>Conclusion</h2>
        <p class='intro'>You have taken", globalenv$num_experiments, "samples of ", globalenv$sample_size, " plants each and calculated the mean of each sample. The plot below shows the distribution of the sample means.</p>
        <p class='intro'>What do you notice about the distribution of the sample means?</p>
        <p class='intro'>The <strong>standard error of the mean</strong> is a measure of how much the sample mean varies from sample to sample. It is calculated as the standard deviation of the sample means (in your case", round(sd(sessionenv$sample_means, na.rm = TRUE), 2), ").</p>
        <p class='intro'>If you only had one sample, you can estimate the standard error of the mean using the formula: SEM = SD / sqrt(n). For example, from your last sample, SEM = ", round(sd(sessionenv$sample_data$Value) / sqrt(globalenv$sample_size), 2), "</p>")
    })
}

# Run the Shiny app
shinyApp(ui, server)
