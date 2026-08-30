library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)

set.seed(102)

sim_env <- new.env()
sim_env$population_1 <- rnorm(10000, mean = 50, sd = 5)
sim_env$population_2 <- rnorm(10000, mean = 50, sd = 5)
sim_env$initial_n <- 20

plot_style <- theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold")
)

# UI
ui <- fluidPage(
    useShinyjs(),
    titlePanel("Optional stopping"),
    fluidRow(
        column(6, plotOutput("boxplot")),
        column(6, plotOutput("p_val_plot"))
    ),
    fluidRow(
        column(12,
            align = "center",
            actionButton("sample_button", "Just one more measurement..."),
            hidden(actionButton("add_many", "Add many measurements"))
        )
    )
)

server <- function(input, output) {
    sample1 <- sample(sim_env$population_1, size = sim_env$initial_n, replace = FALSE)
    sample2 <- sample(sim_env$population_2, size = sim_env$initial_n, replace = FALSE)

    rv <- reactiveValues(
        sample1 = sample1,
        sample2 = sample2,
        pvals = t.test(sample1, sample2)$p.value
    )

    output$boxplot <- renderPlot({
        df <- data.frame(
            group = rep(c("A", "B"), each = length(rv$sample1)),
            value = c(rv$sample1, rv$sample2)
        )

        ggplot(df, aes(x = group, y = value)) +
            geom_boxplot() +
            labs(x = "Group", y = "Value") +
            plot_style
    })

    output$p_val_plot <- renderPlot({
        df <- data.frame(
            sample_size = sim_env$initial_n:(sim_env$initial_n + length(rv$pvals) - 1),
            p_value = rv$pvals
        )

        last_p <- round(df$p_value[length(df$p_value)], 3)
        ggplot(df, aes(x = sample_size, y = p_value)) +
            geom_line(linetype = "dotted") +
            geom_point(
                col = c(
                    rep("black", length(rv$pvals) - 1),
                    ifelse(last_p <= 0.05, "green", "red")
                ),
                size = c(rep(1, length(rv$pvals) - 1), ifelse(last_p <= 0.05, 5, 3))
            ) +
            geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") +
            labs(
                x = "Sample Size", y = "p-value",
                title = paste("p-value = ", last_p)
            ) +
            ylim(0, 1) +
            xlim(0, length(rv$sample1) * 2) +
            plot_style +
            theme(plot.title = element_text(
                size = 16, face = "plain",
                color = ifelse(last_p < 0.05, "darkgreen", "darkred")
            ))
    })

    observeEvent(input$sample_button, {
        new_sample1 <- sample(sim_env$population_1, size = 1, replace = FALSE)
        new_sample2 <- sample(sim_env$population_2, size = 1, replace = FALSE)

        rv$sample1 <- c(rv$sample1, new_sample1)
        rv$sample2 <- c(rv$sample2, new_sample2)

        res <- t.test(rv$sample1, rv$sample2)
        rv$pvals <- c(rv$pvals, res$p.value)

        if (res$p.value < 0.05) {
            show("add_many")
            hide("sample_button")
        }
    })

    observeEvent(input$add_many, {
        for (i in 1:20) {
            new_sample1 <- sample(sim_env$population_1, size = 1, replace = FALSE)
            new_sample2 <- sample(sim_env$population_2, size = 1, replace = FALSE)
            rv$sample1 <- c(rv$sample1, new_sample1)
            rv$sample2 <- c(rv$sample2, new_sample2)

            res <- t.test(rv$sample1, rv$sample2)
            rv$pvals <- c(rv$pvals, res$p.value)
        }
    })
}

shinyApp(ui, server)


