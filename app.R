#
# This is a Shiny web application for analyzing the roll data for D&D polyhedral dice and
# determining of the dice should be considered fair or not.
#

library(shiny)
library(tidyverse)

bayesian_dice_analysis <- function(roll_data, faces = 20, beta_prior_a = 1, beta_prior_b = 1) {
  
  faces_start <- 1
  faces_sep <- 1
  
  # Deal with the d100. It has 10 faces starting at 0 instead of 1 and increasing in increments of 10.
  if (faces == 100) {
    roll_data <- as.numeric(roll_data)
    faces_start <- 0
    faces_sep <- 10
  }
  
  # Check that no rolls are non-numeric
  if (sum(is.na(roll_data)) > 0) {
    stop("Non-numeric rolls were found in the data. Please ensure that the data is valid.")
  } 
  
  # Check that no rolls fall outside 1:faces
  if ((sum(roll_data > faces) + sum(roll_data < 0)) > 0) {
    stop(sprintf("Values outside the range 0 to %i were found in the data. Please ensure the faces parameter is correct and check that the data is valid.", faces))
  } 
  
  # Check that alpha and beta are non-negative
  if (beta_prior_a < 0 || beta_prior_b < 0) {
    stop("The alpha and beta priors must both be non-negtive. Please ensure the values specified are correct.") 
  }
  
  results_table <- tibble(face = seq(from = faces_start, to = faces_start + (faces - faces_sep), by = faces_sep))
  
  # Figure our the number of successes per face
  # Not sure how to do this inside a mutate function below
  successes <- c()
  for (i in results_table$face) {
    successes <- rbind(successes, sum(roll_data == i))
  }
  
  # Add columns for the count of binomial successes and failures
  results_table <- results_table %>% 
    mutate(successes = successes,
           failures = length(roll_data) - successes)
  
  # Add the values related to the posterior distribution
  results_table <- results_table %>%
    mutate(beta_post_a = beta_prior_a + successes,
           beta_post_b = beta_prior_b + failures,
           posterior_mean = beta_post_a / (beta_post_a + beta_post_b),
           posterior_median = qbeta(0.5, beta_post_a, beta_post_b),
           posterior_mode = (beta_post_a - 1) / (beta_post_a + beta_post_b - 2),
           ci_95_lower = qbeta(0.025, beta_post_a, beta_post_b),
           ci_95_upper = qbeta(0.975, beta_post_a, beta_post_b))
  
  return(results_table)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Bayesian D&D Dice Analysis"),
   
   uiOutput("description"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("dice_selection", "Dice Type",
                    choices = c("d20", "d12", "d10", "d100", "d8", "d6", "d4")),
        textAreaInput("dice_data", "Input Data (one roll result per line)", height = "500px")
      ),
      
      # Show a plot of the generated distributio
      mainPanel(
        plotOutput("dice_plot"),
        tableOutput("dice_table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$description <- renderUI({
    description_text_1 <- "This Shiny app allows you to determine better estimates of the true probabilities for rolling each face of a polyhedral dice using bayesian statistics. Roughly speaking, the dice should not be considered fair when one or more of the 95% credible interval lines fails to contain the vertical red line in it's range. To learn more about how this app works, please visit this "
    blog_link <-  a("blog post.", href = "https://everettsprojects.com/2017/10/20/bayesian-dnd.html")
    description_text_2 <- " The code for this shiny app can be found on "
    github_link <- a("GitHub.", href = "https://github.com/evjrob/bayesian-dnd")
    tagList(p(), description_text_1, blog_link, description_text_2, github_link, p())})
  
  # Create plot of the dice face probabilities
  output$dice_plot <- renderPlot({
      dice_selection <- input$dice_selection
      faces <- switch(dice_selection,
                      d20 = 20,
                      d12 = 12,
                      d10 = 10,
                      d100 = 100,
                      d8 = 8,
                      d6 = 6,
                      d4 = 4)
      dice_data <- as.numeric(unlist(strsplit(input$dice_data,"\n")))
      true_faces <- ifelse(faces == 100, 10, faces)
      
      dice_result <- bayesian_dice_analysis(dice_data, faces, beta_prior_a = 100/true_faces, beta_prior_b = 100 - 100/true_faces)
      
      ggplot(dice_result, aes(x = face, y = posterior_mean)) + 
        geom_point() +
        geom_errorbar(aes(ymin = ci_95_lower, ymax = ci_95_upper), colour = "black", width = 0.1) +
        geom_hline(aes(yintercept = 1/true_faces, color = "red")) +
        scale_x_continuous(breaks = round(seq(from = 0, to = faces, by = ifelse(faces == 20, 2, ifelse(faces == 100, 10, 1))))) +
        theme(legend.position = "none") +
        coord_flip() +
        ggtitle("Posterior Probablities for Each Face of Your Dice") +
        labs(x = "Face of the Dice",
             y = "Posterior Probability Distribution")
  })
  # Create table of the dice face probabilities
  output$dice_table <- renderTable({
    dice_selection <- input$dice_selection
    faces <- switch(dice_selection,
                    d20 = 20,
                    d12 = 12,
                    d10 = 10,
                    d100 = 100,
                    d8 = 8,
                    d6 = 6,
                    d4 = 4)
    dice_data <- as.numeric(unlist(strsplit(input$dice_data,"\n")))
    true_faces <- ifelse(faces == 100, 10, faces) # The d100 doesn't actually have 100 faces
    
    dice_result <- bayesian_dice_analysis(dice_data, faces, beta_prior_a = 100/true_faces, beta_prior_b = 100 - 100/true_faces)
    dice_result <- dice_result %>% 
      select(-beta_post_a, - beta_post_b, -posterior_median, -posterior_mode)
    dice_result$face <- as.integer(dice_result$face) 
    names(dice_result) <- c("Face", "Successes", "Failures", "Posterior Mean", "Lower 95% C.I.", "Upper 95% C.I.")
    dice_result
  }, digits = 4)
}

# Run the application 
shinyApp(ui = ui, server = server)

