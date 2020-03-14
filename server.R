library(shiny)
library(plotly)
library(RColorBrewer)
source("constants.R")

shinyServer(function(input, output, session) {

  ## The data frame containing the data for the plot
  data <- reactiveValues(value = FALSE)

  ## Hide or show the tabs
  clusters <- reactive({as.numeric(input$c)})
  tabs_hide_show <- function() {
    for (i in 1:max_c) {
      if (i > clusters()) { hideTab("Folders", target = paste("Cluster", i)) }
      else { showTab("Folders", paste("Cluster", i), select = FALSE, session = getDefaultReactiveDomain()) }
    }
  }
  observeEvent(tabs_hide_show(), NULL)

  ## Update the parameters
  updated <- reactiveValues(value = FALSE)
  observeEvent(input$update, {
    for (i in 1:max_c) {
      for (l in c("x", "y", "z")) {
        updated[[paste(l, i, sep = "")]] <- input[[paste(l, i, sep = "")]]
        updated[[paste("s", l, i, sep = "")]] <- input[[paste("s", l, i, sep = "")]]
      }
    }
    updated$c <- input$c
    updated$m <- input$m
    data_app <- data.frame()
    for (i in 1:updated$c) {
      temp <- list()
      for (l in c("x", "y", "z")) {
        temp[[l]] <- rnorm(updated$m,
                           mean = updated[[paste(l, i, sep = "")]],
                           sd = updated[[paste("s", l, i, sep = "")]])
      }
      temp[["c"]] <- rep(i, updated$m)
      temp[["k"]] <- rep(0, updated$m)
      data_app <- rbind(data_app, data.frame(temp))
    }
    data$x <- data_app$x
    data$y <- data_app$y
    data$z <- data_app$z
    data$c <- data_app$c
    data$s <- ifelse(data_app$c == 5, 10, 12)
    data$k <- data_app$k
  }, ignoreNULL = FALSE)

  ## Reset the parameters
  reset <- reactiveValues(value = FALSE)
  observe({
    input$reset
    for (i in 1:max_c) {
      for (l in c("x", "y", "z")) {
        j <- list("x" = 1, "y" = 2, "z" = 3)[[l]]
        updateSliderInput(session, inputId = paste(l, i, sep = ""), value = init_coord[[i]][j])
        updateSliderInput(session, inputId = paste("s", l, i, sep = ""), value = init_sigma_coord)
      }
    }
    updateSelectInput(session, inputId = "c", selected = init_c)
    updateSelectInput(session, inputId = "m", selected = init_m)
  })

  ## Run the algorithm
  observeEvent(input$run, {
    data_app <- data.frame(x = data$x, y = data$y, z = data$z, k = data$k)
    random_init_indices <- sample(1:nrow(data_app), input$k)
    centroids <- list()
    for (i in 1:input$k) {
      centroids[[i]] <- data.frame(x = data_app$x[random_init_indices[i]],
                                   y = data_app$y[random_init_indices[i]],
                                   z = data_app$z[random_init_indices[i]])
    }
    distances_app <- list()
    for (i in 1:input$n) {
      ## Calculate the nearest centroid for each point (Assignment step)
      for (j in 1:input$k) {
        distances_app[[paste("dist", j, sep = "_")]] <- sqrt((data_app$x - centroids[[j]]$x[1])^2 +
                                                             (data_app$y - centroids[[j]]$y[1])^2 +
                                                             (data_app$z - centroids[[j]]$z[1])^2)
      }
      if (class(distances_app) == "list") { distances_app <- data.frame(distances_app) }
      data_app$k <- apply(distances_app, 1, which.min)
      ## Recalculate the position of the centroids (Update step)
      lost_centroids <- integer(0)
      for (j in 1:input$k) {
        nearest_points <- data_app[data_app$k == j,]
        n_points <- dim(nearest_points)[1]
        if (n_points > 0) {
          centroids[[j]] <- rbind(data.frame(x = sum(nearest_points$x) / n_points,
                                             y = sum(nearest_points$y) / n_points,
                                             z = sum(nearest_points$z) / n_points),
                                  centroids[[j]])
        }
        else { lost_centroids <- c(lost_centroids, j) }
      }
      ## Reinitialize lost centroids
      random_init_indices <- sample(1:nrow(data_app), length(lost_centroids))
      for (j in lost_centroids) {
        centroids[[j]] <- rbind(data.frame(x = data_app$x[random_init_indices[j]],
                                           y = data_app$y[random_init_indices[j]],
                                           z = data_app$z[random_init_indices[j]]),
                                centroids[[j]])
      }
    }
    data$k <- data_app$k
  })
  

  ## Create the plot
  output$plot <- renderPlotly( {
    colors <- c("black", brewer.pal(9, "Set1"), "lightblue")
    n_colors <- dim(table(data$k))
    if (min(data$k) == 0) {
      colors <- colors[1:n_colors]
    }
    else {
      colors <- colors[2:(n_colors+1)]
    }
    plot_ly(x = data$x, y = data$y, z = data$z,
            type = "scatter3d",
            mode = "markers",
            color = as.factor(data$k),
            colors = colors,
            symbol = as.factor(data$c),
            symbols = c("circle", "cross", "square", "diamond", "x"),
            size = data$s,
            width = 800, height = 800) %>%
      layout(scene = list(xaxis = list(title = "Average transaction value", range = c(0, 100)),
                          yaxis = list(title = "Transactions per year", range = c(0, 100)),
                          zaxis = list(title = "Customer seniority", range = c(0, 100)),
                          camera = list(eye = list(x = 1.6,
                                                   y = 1.6,
                                                   z = 0.5))),
             showlegend = FALSE)
  })

})
