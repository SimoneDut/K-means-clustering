library(shiny)
library(plotly)
source("constants.R")

shinyUI(navbarPage("K-means clustering",
                   tabPanel("App",
                            sidebarLayout(
                              sidebarPanel(
                                splitLayout(selectInput(inputId = "c", label = "Number of clusters", choices = 1:max_c,
                                                        selected = init_c, selectize = FALSE),
                                            selectInput(inputId = "m", label = "Elements per cluster", choices = choice_m,
                                                        selected = init_m, selectize = FALSE)
                                ),
                                tabsetPanel(type = "tabs", id = "Folders",
                                            tabPanel("Cluster 1", br(),
                                                     h5(tags$b("Average transaction value")),
                                                     splitLayout(
                                                       sliderInput(inputId = "x1", label = "Mean", min = min_coord, max = max_coord, value = init_coord[[1]][1]),
                                                       sliderInput(inputId = "sx1", label = "Standard deviation", min = min_sigma_coord, max = max_sigma_coord, value = init_sigma_coord)
                                                     ),
                                                     h5(tags$b("Transactions per year")),
                                                     splitLayout(
                                                       sliderInput(inputId = "y1", label = "Mean", min = min_coord, max = max_coord, value = init_coord[[1]][2]),
                                                       sliderInput(inputId = "sy1", label = "Standard deviation", min = min_sigma_coord, max = max_sigma_coord, value = init_sigma_coord)
                                                     ),
                                                     h5(tags$b("Customer seniority")),
                                                     splitLayout(
                                                       sliderInput(inputId = "z1", label = "Mean", min = min_coord, max = max_coord, value = init_coord[[1]][3]),
                                                       sliderInput(inputId = "sz1", label = "Standard deviation", min = min_sigma_coord, max = max_sigma_coord, value = init_sigma_coord)
                                                     )
                                            ),
                                            tabPanel("Cluster 2", br(),
                                                     h5(tags$b("Average transaction value")),
                                                     splitLayout(
                                                       sliderInput(inputId = "x2", label = "Mean", min = min_coord, max = max_coord, value = init_coord[[2]][1]),
                                                       sliderInput(inputId = "sx2", label = "Standard deviation", min = min_sigma_coord, max = max_sigma_coord, value = init_sigma_coord)
                                                     ),
                                                     h5(tags$b("Transactions per year")),
                                                     splitLayout(
                                                       sliderInput(inputId = "y2", label = "Mean", min = min_coord, max = max_coord, value = init_coord[[2]][2]),
                                                       sliderInput(inputId = "sy2", label = "Standard deviation", min = min_sigma_coord, max = max_sigma_coord, value = init_sigma_coord)
                                                     ),
                                                     h5(tags$b("Customer seniority")),
                                                     splitLayout(
                                                       sliderInput(inputId = "z2", label = "Mean", min = min_coord, max = max_coord, value = init_coord[[2]][3]),
                                                       sliderInput(inputId = "sz2", label = "Standard deviation", min = min_sigma_coord, max = max_sigma_coord, value = init_sigma_coord)
                                                     )
                                            ),
                                            tabPanel("Cluster 3", br(),
                                                     h5(tags$b("Average transaction value")),
                                                     splitLayout(
                                                       sliderInput(inputId = "x3", label = "Mean", min = min_coord, max = max_coord, value = init_coord[[3]][1]),
                                                       sliderInput(inputId = "sx3", label = "Standard deviation", min = min_sigma_coord, max = max_sigma_coord, value = init_sigma_coord)
                                                     ),
                                                     h5(tags$b("Transactions per year")),
                                                     splitLayout(
                                                       sliderInput(inputId = "y3", label = "Mean", min = min_coord, max = max_coord, value = init_coord[[3]][2]),
                                                       sliderInput(inputId = "sy3", label = "Standard deviation", min = min_sigma_coord, max = max_sigma_coord, value = init_sigma_coord)
                                                     ),
                                                     h5(tags$b("Customer seniority")),
                                                     splitLayout(
                                                       sliderInput(inputId = "z3", label = "Mean", min = min_coord, max = max_coord, value = init_coord[[3]][3]),
                                                       sliderInput(inputId = "sz3", label = "Standard deviation", min = min_sigma_coord, max = max_sigma_coord, value = init_sigma_coord)
                                                     )
                                            ),
                                            tabPanel("Cluster 4", br(),
                                                     h5(tags$b("Average transaction value")),
                                                     splitLayout(
                                                       sliderInput(inputId = "x4", label = "Mean", min = min_coord, max = max_coord, value = init_coord[[4]][1]),
                                                       sliderInput(inputId = "sx4", label = "Standard deviation", min = min_sigma_coord, max = max_sigma_coord, value = init_sigma_coord)
                                                     ),
                                                     h5(tags$b("Transactions per year")),
                                                     splitLayout(
                                                       sliderInput(inputId = "y4", label = "Mean", min = min_coord, max = max_coord, value = init_coord[[4]][2]),
                                                       sliderInput(inputId = "sy4", label = "Standard deviation", min = min_sigma_coord, max = max_sigma_coord, value = init_sigma_coord)
                                                     ),
                                                     h5(tags$b("Customer seniority")),
                                                     splitLayout(
                                                       sliderInput(inputId = "z4", label = "Mean", min = min_coord, max = max_coord, value = init_coord[[4]][3]),
                                                       sliderInput(inputId = "sz4", label = "Standard deviation", min = min_sigma_coord, max = max_sigma_coord, value = init_sigma_coord)
                                                     )
                                            ),
                                            tabPanel("Cluster 5", br(),
                                                     h5(tags$b("Average transaction value")),
                                                     splitLayout(
                                                       sliderInput(inputId = "x5", label = "Mean", min = min_coord, max = max_coord, value = init_coord[[5]][1]),
                                                       sliderInput(inputId = "sx5", label = "Standard deviation", min = min_sigma_coord, max = max_sigma_coord, value = init_sigma_coord)
                                                     ),
                                                     h5(tags$b("Transactions per year")),
                                                     splitLayout(
                                                       sliderInput(inputId = "y5", label = "Mean", min = min_coord, max = max_coord, value = init_coord[[5]][2]),
                                                       sliderInput(inputId = "sy5", label = "Standard deviation", min = min_sigma_coord, max = max_sigma_coord, value = init_sigma_coord)
                                                     ),
                                                     h5(tags$b("Customer seniority")),
                                                     splitLayout(
                                                       sliderInput(inputId = "z5", label = "Mean", min = min_coord, max = max_coord, value = init_coord[[5]][3]),
                                                       sliderInput(inputId = "sz5", label = "Standard deviation", min = min_sigma_coord, max = max_sigma_coord, value = init_sigma_coord)
                                                     )
                                            )
                                ),
                                splitLayout(
                                  actionButton("update", "Update"),
                                  actionButton("reset", "Reset")
                                ),
                                selectInput(inputId = "k", label = "Number of centroids", choices = 1:max_k,
                                            selected = init_k, selectize = FALSE),
                                sliderInput(inputId = "n", label = "Number of iterations", min = min_n, max = max_n, value = init_n),
                                splitLayout(
                                  actionButton("run", "Run")
                                )
                              ),
                              mainPanel(
                                plotlyOutput("plot")
                                )
                              )
                            ),
                   tabPanel("Instructions",
                            mainPanel(
                              h4(tags$b("Purpose of this App"), align = "center"),
                              br(),
                              h5("This App allows users to generate random datasets composed of up to 5 clusters, each one normally distributed across 3 dimensions.", align = "justify"),
                              h5("Additionally, it is possible to run a 'K-means clustering' algortihm on the datasets.", align = "justify"),
                              br(),
                              h4(tags$b("How to create the random datasets"), align = "center"),
                              br(),
                              h5("At the top of the page, it is possible to set the 'Number of clusters' that the user wants to simulate, as well as the 'Elements per cluster' parameter.", align = "justify"),
                              h5("The parameters defining each cluster can be accessed under the tabs 'Cluster 1' to 'Cluster 5' (some of them may be hidden, depending on how many clusters have been choosen).", align = "justify"),
                              h5("For each cluster, it is possible to set the 'Mean' and 'Standard deviation' values for the 3 dimensions defining the cluster.
                                 The dimensions are: 'Average transaction value', 'Transactions per year', and 'Customer seniority'.", align = "justify"),
                              h5("Once everything is set, by pressing the button 'Update' the dataset is generated and the plot is updated. Using the button 'Reset' instead, all the parameters will go back to their standard values.", align = "justify"),
                              br(),
                              h4(tags$b("How to run the algorithm"), align = "center"),
                              br(),
                              h5("First of all, the user must choose the 'Number of centroids' he wants (from 1 to 10), or in other words how many clusters should the algorithm look for.
                                 This number can be the same, smaller, or bigger than the actual 'Number of clusters' defined above.", align = "justify"),
                              h5("Furthermore, it is possible to modify the 'Number of iterations' (the number of iterative steps for the algorithm).", align = "justify"),
                              h5("Finally, by pressing the button 'Run' the algorithm is executed, and the clusters found are shown on the plot using different colors.", align = "justify"),
                              width = 4)
                            )
                   )
        )
