library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)

### Global simulation parameters ###
default_seed <- 14
pop_size <- 100000
pop_mean_1 <- 50
pop_mean_2 <- 50
pop_sd_1 <- 5
pop_sd_2 <- 5
initial_n <- 20
animation_delay <- 275 # ms between frames
n_multi_point <- 150 # number of points to add when "add many" is clicked

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
        column(12, hidden(
            plotOutput("true_dist_plot")
        ))
    ),
    fluidRow(
        column(12,
            align = "center",
            actionButton("sample_button", "Just one more measurement..."),
            hidden(actionButton("add_many", "Add many measurements")),
            hidden(actionButton("show_true", "Show true population distributions")),
            hidden(actionButton("reset", "Reset"))
        )
    )
)

server <- function(input, output, session) {
    query <- isolate(parseQueryString(session$clientData$url_search))
    if (!is.null(query$seed)) {
        set.seed(as.numeric(query$seed))
    } else {
        set.seed(default_seed)
    }

    ### Simulation environment ###
    sim_env <- new.env()
    sim_env$population_1 <- rnorm(pop_size, mean = pop_mean_1, sd = pop_sd_1)
    sim_env$population_2 <- rnorm(pop_size, mean = pop_mean_2, sd = pop_sd_2)

    sample1 <- sample(sim_env$population_1, size = initial_n, replace = FALSE)
    sample2 <- sample(sim_env$population_2, size = initial_n, replace = FALSE)

    rv <- reactiveValues(
        sample1 = sample1,
        sample2 = sample2,
        pvals = t.test(sample1, sample2)$p.value,
        # Used to track how many more samples to add automatically during the animation
        # of add_many
        auto_remaining = 0,
        animating = FALSE
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

    output$true_dist_plot <- renderPlot({
        df <- data.frame(
            value = c(sim_env$population_1, sim_env$population_2),
            group = rep(c("A", "B"), each = length(sim_env$population_1))
        )

        ggplot(df, aes(x = value, fill = group)) +
            geom_density(alpha = 0.2) +
            labs(x = "Value", y = "Density") +
            plot_style
    })

    observeEvent(input$show_true, {
        show("true_dist_plot")
        show("reset")
        hide("show_true")
        hide("add_many")
        hide("boxplot")
        hide("p_val_plot")
    })

    output$p_val_plot <- renderPlot({
        df <- data.frame(
            sample_size = initial_n:(initial_n + length(rv$pvals) - 1),
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
        hide("add_many")
        rv$auto_remaining <- n_multi_point
        rv$animating <- TRUE
    })

    observe({
        req(rv$animating)
        invalidateLater(animation_delay, session) # number of ms between each new sample

        isolate({ # We isolate to avoid re-triggering this observer when rv$sample1 or rv$sample2 changes
            new_sample1 <- sample(sim_env$population_1, size = 1, replace = FALSE)
            new_sample2 <- sample(sim_env$population_2, size = 1, replace = FALSE)

            rv$sample1 <- c(rv$sample1, new_sample1)
            rv$sample2 <- c(rv$sample2, new_sample2)

            res <- t.test(rv$sample1, rv$sample2)
            rv$pvals <- c(rv$pvals, res$p.value)

            rv$auto_remaining <- rv$auto_remaining - 1
            if (rv$auto_remaining == 0) {
                rv$animating <- FALSE
                show("add_many")
                show("show_true")
            }
        })
    })

    # If we pressed "just one more measurement" more than 20 times, we show the "add many" button to speed up the process and the "show true" button to reveal the true distributions
    observe({
        if (length(rv$sample1) > initial_n + 20) {
            show("add_many")
            show("show_true")
            hide("sample_button")
        }
    })

    observeEvent(input$reset, {
        new_seed <- sample.int(1e6, 1)
        shinyjs::runjs(sprintf("var url = new URL(window.location.href); url.searchParams.set('seed', %d); window.location.href = url.toString();", new_seed))
    })
}

shinyApp(ui, server)
