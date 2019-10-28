# Call libraries
library(shiny)
library(EnvStats)
library(plyr)
library(ggplot2)
library(plotly)


##################################UI
ui <- fluidPage(
  
  tags$br(),
  tags$strong("CALCULATE THE CLINICAL AND ECONOMIC BURDEN OF DECISION UNCERTAINTY IN THE TREATMENT OF STATUS EPILEPTICUS!"),
  tags$br(),
  tags$br(),
  tags$strong("EVALUATION OF NON-INTRAVENOUS BENZODIAZEPINES. DEFAULT INPUT VALUES WERE ESTIMATED FOR ADULT PATIENTS IN 2019."),
  tags$br(),
  tags$br(),
  tags$strong("Use the most updated estimates for effectiveness and cost to explore the potential gains of optimizing status epilepticus treatment."),
  tags$br(),  
  tags$header("The current default input values were estimated for adult patients in 2019. Modify as appropriate."),
  tags$header("Remember to enter appropriate inputs into the model: proportions should go from 0 to 1, costs cannot be negative. With inappropriate inputs the model will return inappropriate outputs or errors."),
  tags$br(),  
  tags$header("WARNING: This app performs heavy computations in the background and may take up to 1 minute at the beginning before displaying results and every time that any input is modified."),
  tags$br(), 
  tags$br(), 
  
  sidebarLayout(
    
    ##### sidebarPanel
    sidebarPanel(
      tags$strong("INPUT"),
      tags$br(),
      tags$br(),
      tags$strong("LOW-EFFECTIVENESS NON-INTRAVENOUS BENZODIAZEPINE"),
      tags$br(),
      tags$br(),
      column(6,
      tags$br(),
      textInput(inputId = "mu_p_LE_nonIVBZD_SS",
                label = "Mean effectiveness",
                value = 0.75,
                width = 150),
      tags$br(),
      textInput(inputId = "sd_p_LE_nonIVBZD_SS",
                label = "Standard deviation of the effectiveness",
                value = 0.075,
                width = 150)), ######### End of column
      column(6,
      textInput(inputId = "min_c_LE_nonIVBZD",
                label = "Minimum cost",
                value = 376.2,
                width = 150),
      textInput(inputId = "lik_c_LE_nonIVBZD",
                label = "Likeliest cost",
                value = 388.4,
                width = 150),
      textInput(inputId = "max_c_LE_nonIVBZD",
                label = "Maximum cost",
                value = 400.5,
                width = 150),
      tags$br(),
      tags$br()
      ),######### End of column
      
      
      tags$strong("HIGH-EFFECTIVENESS NON-INTRAVENOUS BENZODIAZEPINE"),   
      tags$br(),
      tags$br(),
      column(6,
      tags$br(),
      textInput(inputId = "mu_p_HE_nonIVBZD_SS",
                label = "Mean effectiveness",
                value = 0.89,
                width = 150),
      tags$br(),
      textInput(inputId = "sd_p_HE_nonIVBZD_SS",
                label = "Standard deviation of the effectiveness",
                value = 0.04,
                width = 150)), ######### End of column
      column(6,
      textInput(inputId = "min_c_HE_nonIVBZD",
                label = "Minimum cost",
                value = 659.9,
                width = 150),
      textInput(inputId = "lik_c_HE_nonIVBZD",
                label = "Likeliest cost",
                value = 660,
                width = 150),
      textInput(inputId = "max_c_HE_nonIVBZD",
                label = "Maximum cost",
                value = 660.1,
                width = 150),
      tags$br(),
      tags$br()
      ), ######### End of column
      
      
      column(6, 
      tags$strong("COST OF ED VISITS"),    
      tags$br(),
      tags$br(),
      textInput(inputId = "min_c_ED",
                label = "Minimum cost",
                value = 1079,
                width = 150),
      textInput(inputId = "lik_c_ED",
                label = "Likeliest cost",
                value = 2081,
                width = 150),
      textInput(inputId = "max_c_ED",
                label = "Maximum cost",
                value = 4119,
                width = 150)), ######### End of column
      column(6,
      tags$strong("COST OF HOSPITAL ADMISSIONS"), 
      tags$br(),
      tags$br(),
      textInput(inputId = "min_c_HA",
                label = "Minimum cost",
                value = 6072,
                width = 150),
      textInput(inputId = "lik_c_HA",
                label = "Likeliest cost",
                value = 9697,
                width = 150),
      textInput(inputId = "max_c_HA",
                label = "Maximum cost",
                value = 18284,
                width = 150)), ############ End of column
      img(src="Figuree1A.png", height="100%", width="100%")
    ),
    ##### sidebarPanel
    
    
    
    
    ##### mainPanel
    mainPanel(
      tags$br(),
      tags$strong("OUTPUT"),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$strong("Table with the median (p25-p75) estimates of hospital admissions and costs."),
      tags$header("It may take up to 1 minute to calculate the initial results."),
      tags$header("Every time an input is modified, the table will gray out for up to 1 minute until the new values are calculated."),
      tags$br(),
      tags$br(),
      tableOutput("nonIVBZDtable"),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$strong("Cost-effectiveness plot comparing the high-effectiveness (green triangles) and low-effectiveness (red squares) non-IV BZD."),
      tags$header("It may take up to 1 minute to calculate the initial results."),
      tags$header("Every time an input is modified, it may take up to 1 minute to calculate the new results."),
      tags$br(),
      tags$br(),
      plotlyOutput("nonIVBZD_CEplot", width = 800, height = 500)
    )
    ##### mainPanel
    
  )
)
##################################UI



##################################SERVER
server <- function(input, output) {
  
  ##### table
  output$nonIVBZDtable <- renderTable({
    
    ## FUNCTION TO OBTAIN ALPHA AND BETA PARAMETERS FOR BETA DISTRIBUTION FROM MEAN AND STANDARD DEVIATION
    beta_distribution_alpha_beta <- function(mu, SD) {
      # INPUTS:
      # mu: mean of the distribution
      # SD: standard deviation of the distribution
      # OUTPUTS:
      # alpha: alpha parameter of the beta distribution
      # beta: beta parameter of the beta distribution
      var = SD^2
      alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
      beta <- alpha * (1 / mu - 1)
      return(params = list(alpha = alpha, beta = beta))
    }    
    ## FUNCTION TO OBTAIN ALPHA AND BETA PARAMETERS FOR BETA DISTRIBUTION FROM MEAN AND STANDARD DEVIATION
    
    
    ############################ADMISSIONS
    ## Input probabilities
    mu_p_LE_nonIVBZD_SS <- as.numeric(input$mu_p_LE_nonIVBZD_SS)
    sd_p_LE_nonIVBZD_SS <- as.numeric(input$sd_p_LE_nonIVBZD_SS)
    
    mu_p_HE_nonIVBZD_SS <- as.numeric(input$mu_p_HE_nonIVBZD_SS)
    sd_p_HE_nonIVBZD_SS <- as.numeric(input$sd_p_HE_nonIVBZD_SS)
    
    
    ## Alpha and beta parameters for the beta distribution
    alpha_LE_nonIVBZD_SS <- as.numeric(beta_distribution_alpha_beta(mu_p_LE_nonIVBZD_SS, sd_p_LE_nonIVBZD_SS)[1])
    beta_LE_nonIVBZD_SS <- as.numeric(beta_distribution_alpha_beta(mu_p_LE_nonIVBZD_SS, sd_p_LE_nonIVBZD_SS)[2])
    
    alpha_HE_nonIVBZD_SS <- as.numeric(beta_distribution_alpha_beta(mu_p_HE_nonIVBZD_SS, sd_p_HE_nonIVBZD_SS)[1])
    beta_HE_nonIVBZD_SS <- as.numeric(beta_distribution_alpha_beta(mu_p_HE_nonIVBZD_SS, sd_p_HE_nonIVBZD_SS)[2])
    
    
    ## Generate the second-order Monte-Carlo simulation parameters
    # Initialize empty vectors
    p_admiss_LE <- c()
    p_admiss_HE <- c()
    p_admiss_diff <- c()
    # Loop over the iterations (following convention 10000)
    for (i in 1:10000) {
      # Generate and save in the vector the probability of admission for the low-effectiveness arm
      p_admiss_LE[i] <- (1-as.numeric(rbeta(1, alpha_LE_nonIVBZD_SS, beta_LE_nonIVBZD_SS))) * (1-as.numeric(rbeta(1, alpha_LE_nonIVBZD_SS, beta_LE_nonIVBZD_SS)))
      # Generate and save in the vector the probability of admission for the high-effectiveness arm
      p_admiss_HE[i] <- (1-as.numeric(rbeta(1, alpha_HE_nonIVBZD_SS, beta_HE_nonIVBZD_SS))) * (1-as.numeric(rbeta(1, alpha_HE_nonIVBZD_SS, beta_HE_nonIVBZD_SS)))
      # Generate and save in the vector the difference in probability of admission between low-effectiveness and low-effectiveness arms
      p_admiss_diff[i] <- p_admiss_LE[i] - p_admiss_HE[i]
    }
    
    
    ## Calculate results: admissions
    admissions_LE <- paste0( as.character(round(summary(p_admiss_LE)[3]*100,1)), " (", as.character(round(summary(p_admiss_LE)[2]*100,1)), " to ",  as.character(round(summary(p_admiss_LE)[5]*100,1)), ")" )
    admissions_HE <- paste0( as.character(round(summary(p_admiss_HE)[3]*100,1)), " (", as.character(round(summary(p_admiss_HE)[2]*100,1)), " to ",  as.character(round(summary(p_admiss_HE)[5]*100,1)), ")" )
    admissions_diff <- paste0( as.character(round(summary(p_admiss_diff)[3]*100,1)), " (", as.character(round(summary(p_admiss_diff)[2]*100,1)), " to ",  as.character(round(summary(p_admiss_diff)[5]*100,1)), ")" )
    ############################ADMISSIONS
    
    
    
    
    ############################COSTS
    ## Input costs
    min_c_LE_nonIVBZD <- as.numeric(input$min_c_LE_nonIVBZD)
    lik_c_LE_nonIVBZD <- as.numeric(input$lik_c_LE_nonIVBZD)
    max_c_LE_nonIVBZD <- as.numeric(input$max_c_LE_nonIVBZD)
    
    min_c_HE_nonIVBZD <- as.numeric(input$min_c_HE_nonIVBZD)
    lik_c_HE_nonIVBZD <- as.numeric(input$lik_c_HE_nonIVBZD)
    max_c_HE_nonIVBZD <- as.numeric(input$max_c_HE_nonIVBZD)
    
    min_c_ED <- as.numeric(input$min_c_ED)
    lik_c_ED <- as.numeric(input$lik_c_ED)
    max_c_ED <- as.numeric(input$max_c_ED)
    
    min_c_HA <- as.numeric(input$min_c_HA)
    lik_c_HA <- as.numeric(input$lik_c_HA)
    max_c_HA <- as.numeric(input$max_c_HA)
    
    ## Generate the second-order Monte-Carlo simulation parameters
    # Initialize empty vectors
    c_LE <- c()
    c_HE <- c()
    c_diff <- c()
    # Loop over the iterations (10000 as conventional)
    for (i in 1:10000) {
      # Generate and save in the vector the costs associated with the low-effectiveness arm
      c_LE[i] <- ( as.numeric(rtri(n=1, min=min_c_LE_nonIVBZD, max=max_c_LE_nonIVBZD, mode=lik_c_LE_nonIVBZD)) * as.numeric(rbeta(1, alpha_LE_nonIVBZD_SS, beta_LE_nonIVBZD_SS)) ) +
        
        (
          ( 
            ( 2*as.numeric(rtri(n=1, min=min_c_LE_nonIVBZD, max=max_c_LE_nonIVBZD, mode=lik_c_LE_nonIVBZD)) + 
                as.numeric(rtri(n=1, min=min_c_ED, max=max_c_ED, mode=lik_c_ED)) ) * 
              as.numeric(rbeta(1, alpha_LE_nonIVBZD_SS, beta_LE_nonIVBZD_SS))
          ) + 
            
            (
              2*as.numeric(rtri(n=1, min=min_c_LE_nonIVBZD, max=max_c_LE_nonIVBZD, mode=lik_c_LE_nonIVBZD)) +
                as.numeric(rtri(n=1, min=min_c_ED, max=max_c_ED, mode=lik_c_ED)) +
                as.numeric(rtri(n=1, min=min_c_HA, max=max_c_HA, mode=lik_c_HA))
            ) * (1 - as.numeric(rbeta(1, alpha_LE_nonIVBZD_SS, beta_LE_nonIVBZD_SS)))
        ) * (1 - as.numeric(rbeta(1, alpha_LE_nonIVBZD_SS, beta_LE_nonIVBZD_SS)))
      
      
      # Generate and save in the vector the costs associated with the high-effectiveness arm
      c_HE[i] <- ( as.numeric(rtri(n=1, min=min_c_HE_nonIVBZD, max=max_c_HE_nonIVBZD, mode=lik_c_HE_nonIVBZD)) * as.numeric(rbeta(1, alpha_HE_nonIVBZD_SS, beta_HE_nonIVBZD_SS)) ) +
        
        (
          ( 
            ( 2*as.numeric(rtri(n=1, min=min_c_HE_nonIVBZD, max=max_c_HE_nonIVBZD, mode=lik_c_HE_nonIVBZD)) + 
                as.numeric(rtri(n=1, min=min_c_ED, max=max_c_ED, mode=lik_c_ED)) ) * 
              as.numeric(rbeta(1, alpha_HE_nonIVBZD_SS, beta_HE_nonIVBZD_SS))
          ) + 
            
            (
              2*as.numeric(rtri(n=1, min=min_c_HE_nonIVBZD, max=max_c_HE_nonIVBZD, mode=lik_c_HE_nonIVBZD)) +
                as.numeric(rtri(n=1, min=min_c_ED, max=max_c_ED, mode=lik_c_ED)) +
                as.numeric(rtri(n=1, min=min_c_HA, max=max_c_HA, mode=lik_c_HA))
            ) * (1 - as.numeric(rbeta(1, alpha_HE_nonIVBZD_SS, beta_HE_nonIVBZD_SS)))
        ) * (1 - as.numeric(rbeta(1, alpha_HE_nonIVBZD_SS, beta_HE_nonIVBZD_SS)))
      
      # Generate and save in the vector the difference in costs between low-effectiveness and high-effectiveness arms
      c_diff[i] <- c_LE[i] - c_HE[i]
    }
    
    
    ## Calculate results: costs
    costs_LE <- paste0( as.character(round(summary(c_LE)[3])), " (", as.character(round(summary(c_LE)[2])), " to ",  as.character(round(summary(c_LE)[5])), ")" )
    costs_HE <- paste0( as.character(round(summary(c_HE)[3])), " (", as.character(round(summary(c_HE)[2])), " to ",  as.character(round(summary(c_HE)[5])), ")" )
    costs_diff <- paste0( as.character(round(summary(c_diff)[3])), " (", as.character(round(summary(c_diff)[2])), " to ",  as.character(round(summary(c_diff)[5])), ")" )    
    ############################COSTS
    
    
    #####################FINAL TABLE
    ## Final table
    
    admissions <- c(admissions_LE, admissions_HE, admissions_diff)
    costs <- c(costs_LE, costs_HE, costs_diff)
    final_table <- data.frame(admissions, costs)
    
    # Save variables globally for use in the plot without calculating them again
    p_admiss_LE <<- p_admiss_LE
    p_admiss_HE <<- p_admiss_HE
    c_LE <<- c_LE
    c_HE <<- c_HE
        
    final_table <- t(final_table)
    rownames(final_table) <- c("Admissions (per 100 cases)", "Costs ($ per case)")
    colnames(final_table) <- c("Low-effectiveness", "High-effectiveness", "Difference")
    final_table <- cbind(Category=rownames(final_table), final_table)
    final_table
    

  })
  ##### table
  
  
  
  ########################PLOT
  output$nonIVBZD_CEplot <- renderPlotly({
    
    nonIVBZD_data_for_plot <- data.frame(c(1-p_admiss_LE, 1-p_admiss_HE), c(c_LE, c_HE), c(rep("LE", 10000), rep("HE", 10000)))
    colnames(nonIVBZD_data_for_plot) <- c("p_SS", "c_LE", "LEvsHE")
    
    plot_CE <- ggplot(aes(x=p_SS, y=c_LE, color=LEvsHE), data=nonIVBZD_data_for_plot) + geom_point(alpha=0.25, pch=c(rep(15, 10000), rep(17, 10000)), color=c(rep("red", 10000), rep("darkgreen", 10000))) + xlim(0,1) + ylim(0, round_any(max(nonIVBZD_data_for_plot$c_LE), 500, f=ceiling)) +
      theme(panel.background = element_rect(fill = "white"), 
            axis.text = element_text(size = 10, color = "black", face = "bold"),
            axis.title = element_text(size = 16, color = "black", face = "bold"),
            axis.ticks.length = unit(0.2, "cm"), axis.ticks = element_line(size = 1),
            axis.line.x = element_line(color="black", size = 1),
            axis.line.y = element_line(color="black", size = 1), 
            legend.key = element_rect(fill = "white"), legend.position = "bottom") + 
      labs(x= "Effectiveness (SS)", y= "Cost ($)")
    
    ggplotly(plot_CE)
  })
  ########################PLOT
  
}







##################################SERVER


shinyApp(ui = ui, server = server)