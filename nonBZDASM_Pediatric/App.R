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
  tags$strong("EVALUATION OF NON-BENZODIAZEPINE ANTISEIZURE MEDICATIONS. DEFAULT INPUT VALUES WERE ESTIMATED FOR PEDIATRIC PATIENTS IN 2019."),
  tags$br(),
  tags$br(),
  tags$strong("Use the most updated estimates for effectiveness and cost to explore the potential gains of optimizing status epilepticus treatment."),
  tags$br(),  
  tags$header("The current default input values were estimated for pediatric patients in 2019. Modify as appropriate."),
  tags$header("Remember to enter appropriate inputs into the model: proportions should go from 0 to 1, costs cannot be negative. With inappropriate inputs the model will return inappropriate outputs or errors."),
  tags$br(),  
  tags$header("Frequently, the first non-benzodiapine antiseizure medication is not repeated, but changed to a different second non-benzodiazepine antiseizure medication. This app allows for selecting two different low-effectiveness non-benzodiazepine antiseizure medications (1 and 2). In contrast, the high-effectiveness non-benzodiazepine antiseizure medication is the same as there is only one optimal non-benzodiazepine antiseizure medication."),
  tags$br(), 
  tags$header("WARNING: This app performs heavy computations in the background and may take up to 7 minutes at the beginning before displaying results and every time that any input is modified."),
  tags$br(), 
  tags$br(), 
  
  sidebarLayout(
    
    ##### sidebarPanel
    sidebarPanel(
      tags$strong("INPUT"),
      tags$br(),
      tags$br(),
      tags$strong("LOW-EFFECTIVENESS NON-BENZODIAZEPINE ANTISEIZURE MEDICATION 1"),
      tags$br(),
      tags$br(),
      column(6,
      tags$br(),
      textInput(inputId = "mu_p_first_LE_nonBZDASM_SS",
                label = "Mean effectiveness",
                value = 0.53,
                width = 150),
      tags$br(),
      textInput(inputId = "sd_p_first_LE_nonBZDASM_SS",
                label = "Standard deviation of the effectiveness",
                value = 0.07,
                width = 150)), ######### End of column
      column(6,
      textInput(inputId = "min_c_first_LE_nonBZDASM",
                label = "Minimum cost",
                value = 7.43,
                width = 150),
      textInput(inputId = "lik_c_first_LE_nonBZDASM",
                label = "Likeliest cost",
                value = 10.15,
                width = 150),
      textInput(inputId = "max_c_first_LE_nonBZDASM",
                label = "Maximum cost",
                value = 21.38,
                width = 150),
      tags$br(),
      tags$br()
      ),######### End of column
      

      tags$strong("LOW-EFFECTIVENESS NON-BENZODIAZEPINE ANTISEIZURE MEDICATION 2"),
      tags$br(),
      tags$br(),
      column(6,
             tags$br(),
             textInput(inputId = "mu_p_second_LE_nonBZDASM_SS",
                       label = "Mean effectiveness",
                       value = 0.8,
                       width = 150),
             tags$br(),
             textInput(inputId = "sd_p_second_LE_nonBZDASM_SS",
                       label = "Standard deviation of the effectiveness",
                       value = 0.055,
                       width = 150)), ######### End of column
      column(6,
             textInput(inputId = "min_c_second_LE_nonBZDASM",
                       label = "Minimum cost",
                       value = 367.4,
                       width = 150),
             textInput(inputId = "lik_c_second_LE_nonBZDASM",
                       label = "Likeliest cost",
                       value = 563.5,
                       width = 150),
             textInput(inputId = "max_c_second_LE_nonBZDASM",
                       label = "Maximum cost",
                       value = 789.5,
                       width = 150),
             tags$br(),
             tags$br()
      ),######### End of column
      
            
      tags$strong("HIGH-EFFECTIVENESS NON-BENZODIAZEPINE ANTISEIZURE MEDICATION"),   
      tags$br(),
      tags$br(),
      column(6,
      tags$br(),
      textInput(inputId = "mu_p_HE_nonBZDASM_SS",
                label = "Mean effectiveness",
                value = 0.8,
                width = 150),
      tags$br(),
      textInput(inputId = "sd_p_HE_nonBZDASM_SS",
                label = "Standard deviation of the effectiveness",
                value = 0.005,
                width = 150)), ######### End of column
      column(6,
      textInput(inputId = "min_c_HE_nonBZDASM",
                label = "Minimum cost",
                value = 367.4,
                width = 150),
      textInput(inputId = "lik_c_HE_nonBZDASM",
                label = "Likeliest cost",
                value = 563.5,
                width = 150),
      textInput(inputId = "max_c_HE_nonBZDASM",
                label = "Maximum cost",
                value = 789.5,
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
                width = 150),
      tags$br(),
      tags$br()
      ), ######### End of column
      column(6,
      tags$strong("COST OF HOSPITAL ADMISSIONS"), 
      tags$br(),
      tags$br(),
      textInput(inputId = "min_c_HA",
                label = "Minimum cost",
                value = 3908,
                width = 150),
      textInput(inputId = "lik_c_HA",
                label = "Likeliest cost",
                value = 7039,
                width = 150),
      textInput(inputId = "max_c_HA",
                label = "Maximum cost",
                value = 14869,
                width = 150), 
      tags$br(),
      tags$br()
      ), ############ End of column

            
      column(6, 
             tags$strong("COST OF HOSPITAL ADMISSIONs FOR REFRACTORY STATUS EPILEPTICUS"),    
             tags$br(),
             tags$br(),
             textInput(inputId = "min_c_RSEA",
                       label = "Minimum cost",
                       value = 8916,
                       width = 150),
             textInput(inputId = "lik_c_RSEA",
                       label = "Likeliest cost",
                       value = 14116,
                       width = 150),
             textInput(inputId = "max_c_RSEA",
                       label = "Maximum cost",
                       value = 23664,
                       width = 150)), ######### End of column
      column(6,
             tags$strong("COST OF HOSPITAL ADMISSIONS FOR SUPER-REFRACTORY STATUS EPILEPTICUS"), 
             tags$br(),
             tags$br(),
             textInput(inputId = "min_c_SRSEA",
                       label = "Minimum cost",
                       value = 69154,
                       width = 150),
             textInput(inputId = "lik_c_SRSEA",
                       label = "Likeliest cost",
                       value = 148520,
                       width = 150),
             textInput(inputId = "max_c_SRSEA",
                       label = "Maximum cost",
                       value = 244843,
                       width = 150)), ############ End of column
      
      
      img(src="Figuree1B.png", height="100%", width="100%")
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
      tags$header("It may take up to 7 minutes to calculate the initial results."),
      tags$header("Every time an input is modified, the table will gray out for up to 7 minutes until the new values are calculated."),
      tags$br(),
      tags$br(),
      tableOutput("nonBZDASMtable"),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$strong("Cost-effectiveness plot comparing the high-effectiveness (green diamonds) and low-effectiveness (red circles) non-BZD ASM."),
      tags$header("It may take up to 7 minutes to calculate the initial results."),
      tags$header("Every time an input is modified, it may take up to 7 minutes to calculate the new results."),
      tags$br(),
      tags$br(),
      plotlyOutput("nonBZDASM_CEplot", width = 800, height = 500)
    )
    ##### mainPanel
    
  )
)
##################################UI



##################################SERVER
server <- function(input, output) {
  
  ##### table
  output$nonBZDASMtable <- renderTable({
    
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
    mu_p_first_LE_nonBZDASM_SS <- as.numeric(input$mu_p_first_LE_nonBZDASM_SS)
    sd_p_first_LE_nonBZDASM_SS <- as.numeric(input$sd_p_first_LE_nonBZDASM_SS)
    
    mu_p_second_LE_nonBZDASM_SS <- as.numeric(input$mu_p_second_LE_nonBZDASM_SS)
    sd_p_second_LE_nonBZDASM_SS <- as.numeric(input$sd_p_second_LE_nonBZDASM_SS)
    
    mu_p_HE_nonBZDASM_SS <- as.numeric(input$mu_p_HE_nonBZDASM_SS)
    sd_p_HE_nonBZDASM_SS <- as.numeric(input$sd_p_HE_nonBZDASM_SS)
    
    
    ## Alpha and beta parameters for the beta distribution
    alpha_first_LE_nonBZDASM_SS <- as.numeric(beta_distribution_alpha_beta(mu_p_first_LE_nonBZDASM_SS, sd_p_first_LE_nonBZDASM_SS)[1])
    beta_first_LE_nonBZDASM_SS <- as.numeric(beta_distribution_alpha_beta(mu_p_first_LE_nonBZDASM_SS, sd_p_first_LE_nonBZDASM_SS)[2])
    
    alpha_second_LE_nonBZDASM_SS <- as.numeric(beta_distribution_alpha_beta(mu_p_second_LE_nonBZDASM_SS, sd_p_second_LE_nonBZDASM_SS)[1])
    beta_second_LE_nonBZDASM_SS <- as.numeric(beta_distribution_alpha_beta(mu_p_second_LE_nonBZDASM_SS, sd_p_second_LE_nonBZDASM_SS)[2])
    
    alpha_HE_nonBZDASM_SS <- as.numeric(beta_distribution_alpha_beta(mu_p_HE_nonBZDASM_SS, sd_p_HE_nonBZDASM_SS)[1])
    beta_HE_nonBZDASM_SS <- as.numeric(beta_distribution_alpha_beta(mu_p_HE_nonBZDASM_SS, sd_p_HE_nonBZDASM_SS)[2])
    
    
    ## Generate the second-order Monte-Carlo simulation parameters
    # Initialize empty vectors
    p_ICUadmiss_LE <- c()
    p_ICUadmiss_HE <- c()
    p_ICUadmiss_diff <- c()
    # Loop over the iterations (10000 as conventional)
    for (i in 1:10000) {
      # Generate and save in the vector the probability of ICU admission for the low-effectiveness arm
      p_ICUadmiss_LE[i] <- (1-as.numeric(rbeta(1, alpha_second_LE_nonBZDASM_SS, beta_second_LE_nonBZDASM_SS))) * (1-as.numeric(rbeta(1, alpha_first_LE_nonBZDASM_SS, beta_first_LE_nonBZDASM_SS)))
      # Generate and save in the vector the probability of ICU admission for the high-effectiveness arm
      p_ICUadmiss_HE[i] <- (1-as.numeric(rbeta(1, alpha_HE_nonBZDASM_SS, beta_HE_nonBZDASM_SS))) * (1-as.numeric(rbeta(1, alpha_HE_nonBZDASM_SS, beta_HE_nonBZDASM_SS)))
      # Generate and save in the vector the difference in probability of admission between low-effectiveness and high-effectiveness arms
      p_ICUadmiss_diff[i] <- p_ICUadmiss_LE[i] - p_ICUadmiss_HE[i]
    }
    
    
    ## Calculate results: admissions
    admissions_LE <- paste0( as.character(round(summary(p_ICUadmiss_LE)[3]*100,1)), " (", as.character(round(summary(p_ICUadmiss_LE)[2]*100,1)), " to ",  as.character(round(summary(p_ICUadmiss_LE)[5]*100,1)), ")" )
    admissions_HE <- paste0( as.character(round(summary(p_ICUadmiss_HE)[3]*100,1)), " (", as.character(round(summary(p_ICUadmiss_HE)[2]*100,1)), " to ",  as.character(round(summary(p_ICUadmiss_HE)[5]*100,1)), ")" )
    admissions_diff <- paste0( as.character(round(summary(p_ICUadmiss_diff)[3]*100,1)), " (", as.character(round(summary(p_ICUadmiss_diff)[2]*100,1)), " to ",  as.character(round(summary(p_ICUadmiss_diff)[5]*100,1)), ")" )
    ############################ADMISSIONS
    
    
    
    
    ############################COSTS
    ## Input costs
    min_c_first_LE_nonBZDASM <- as.numeric(input$min_c_first_LE_nonBZDASM)
    lik_c_first_LE_nonBZDASM <- as.numeric(input$lik_c_first_LE_nonBZDASM)
    max_c_first_LE_nonBZDASM <- as.numeric(input$max_c_first_LE_nonBZDASM)
    
    min_c_second_LE_nonBZDASM <- as.numeric(input$min_c_second_LE_nonBZDASM)
    lik_c_second_LE_nonBZDASM <- as.numeric(input$lik_c_second_LE_nonBZDASM)
    max_c_second_LE_nonBZDASM <- as.numeric(input$max_c_second_LE_nonBZDASM)
    
    min_c_HE_nonBZDASM <- as.numeric(input$min_c_HE_nonBZDASM)
    lik_c_HE_nonBZDASM <- as.numeric(input$lik_c_HE_nonBZDASM)
    max_c_HE_nonBZDASM <- as.numeric(input$max_c_HE_nonBZDASM)

    min_c_ED <- as.numeric(input$min_c_ED)
    lik_c_ED <- as.numeric(input$lik_c_ED)
    max_c_ED <- as.numeric(input$max_c_ED)
    
    min_c_HA <- as.numeric(input$min_c_HA)
    lik_c_HA <- as.numeric(input$lik_c_HA)
    max_c_HA <- as.numeric(input$max_c_HA)
    
    min_c_RSEA <- as.numeric(input$min_c_RSEA)
    lik_c_RSEA <- as.numeric(input$lik_c_RSEA)
    max_c_RSEA <- as.numeric(input$max_c_RSEA)
    
    min_c_SRSEA <- as.numeric(input$min_c_SRSEA)
    lik_c_SRSEA <- as.numeric(input$lik_c_SRSEA)
    max_c_SRSEA <- as.numeric(input$max_c_SRSEA)
    
    
    ## Generate the second-order Monte-Carlo simulation parameters
    # Initialize empty vectors
    c_LE <- c()
    c_HE <- c()
    c_diff <- c()
    # Loop over the iterations (10000 as conventional)
    for (i in 1:10000) {
      # Generate and save in the vector the costs associated with the low-effectiveness arm
      c_LE[i] <- ( ( as.numeric(rtri(n=1, min=min_c_first_LE_nonBZDASM, max=max_c_first_LE_nonBZDASM, mode=lik_c_first_LE_nonBZDASM) +
                                  as.numeric(rtri(n=1, min=min_c_ED, max=max_c_ED, mode=lik_c_ED)) +
                                  as.numeric(rtri(n=1, min=min_c_HA, max=max_c_HA, mode=lik_c_HA)) ) ) * as.numeric(rbeta(1, alpha_first_LE_nonBZDASM_SS, beta_first_LE_nonBZDASM_SS)) ) +
        
        (
          ( 
            ( ( as.numeric(rtri(n=1, min=min_c_first_LE_nonBZDASM, max=max_c_first_LE_nonBZDASM, mode=lik_c_first_LE_nonBZDASM)) + as.numeric(rtri(n=1, min=min_c_second_LE_nonBZDASM, max=max_c_second_LE_nonBZDASM, mode=lik_c_second_LE_nonBZDASM)) ) + 
                as.numeric(rtri(n=1, min=min_c_ED, max=max_c_ED, mode=lik_c_ED)) +
                as.numeric(rtri(n=1, min=min_c_HA, max=max_c_HA, mode=lik_c_HA))  ) * 
              as.numeric(rbeta(1, alpha_second_LE_nonBZDASM_SS, beta_second_LE_nonBZDASM_SS))
          ) + 
            
            (
              ( as.numeric(rtri(n=1, min=min_c_first_LE_nonBZDASM, max=max_c_first_LE_nonBZDASM, mode=lik_c_first_LE_nonBZDASM)) +
                  as.numeric(rtri(n=1, min=min_c_second_LE_nonBZDASM, max=max_c_second_LE_nonBZDASM, mode=lik_c_second_LE_nonBZDASM)) ) +
                as.numeric(rtri(n=1, min=min_c_ED, max=max_c_ED, mode=lik_c_ED)) +
                0.8*as.numeric(rtri(n=1, min=min_c_RSEA, max=max_c_RSEA, mode=lik_c_RSEA)) +
                0.2*as.numeric(rtri(n=1, min=min_c_SRSEA, max=max_c_SRSEA, mode=lik_c_SRSEA))
            ) * (1 - as.numeric(rbeta(1, alpha_second_LE_nonBZDASM_SS, beta_second_LE_nonBZDASM_SS)))
        ) * (1 - as.numeric(rbeta(1, alpha_first_LE_nonBZDASM_SS, beta_first_LE_nonBZDASM_SS)))
      
      
      # Generate and save in the vector the costs associated with the high-effectiveness arm
      c_HE[i] <- ( ( as.numeric(rtri(n=1, min=min_c_HE_nonBZDASM, max=max_c_HE_nonBZDASM, mode=lik_c_HE_nonBZDASM)) + as.numeric(rtri(n=1, min=min_c_ED, max=max_c_ED, mode=lik_c_ED)) +
                       as.numeric(rtri(n=1, min=min_c_HA, max=max_c_HA, mode=lik_c_HA)) ) * as.numeric(rbeta(1, alpha_HE_nonBZDASM_SS, beta_HE_nonBZDASM_SS)) ) +
        
        (
          ( 
            ( ( as.numeric(rtri(n=1, min=min_c_HE_nonBZDASM, max=max_c_HE_nonBZDASM, mode=lik_c_HE_nonBZDASM)) + as.numeric(rtri(n=1, min=min_c_HE_nonBZDASM, max=max_c_HE_nonBZDASM, mode=lik_c_HE_nonBZDASM)) ) + 
                as.numeric(rtri(n=1, min=min_c_ED, max=max_c_ED, mode=lik_c_ED)) +
                as.numeric(rtri(n=1, min=min_c_HA, max=max_c_HA, mode=lik_c_HA))  ) * 
              as.numeric(rbeta(1, alpha_HE_nonBZDASM_SS, beta_HE_nonBZDASM_SS))
          ) + 
            
            (
              ( as.numeric(rtri(n=1, min=min_c_HE_nonBZDASM, max=max_c_HE_nonBZDASM, mode=lik_c_HE_nonBZDASM)) +
                  as.numeric(rtri(n=1, min=min_c_HE_nonBZDASM, max=max_c_HE_nonBZDASM, mode=lik_c_HE_nonBZDASM)) ) +
                as.numeric(rtri(n=1, min=min_c_ED, max=max_c_ED, mode=lik_c_ED)) +
                0.8*as.numeric(rtri(n=1, min=min_c_RSEA, max=max_c_RSEA, mode=lik_c_RSEA)) +
                0.2*as.numeric(rtri(n=1, min=min_c_SRSEA, max=max_c_SRSEA, mode=lik_c_SRSEA))
            ) * (1 - as.numeric(rbeta(1, alpha_HE_nonBZDASM_SS, beta_HE_nonBZDASM_SS)))
        ) * (1 - as.numeric(rbeta(1, alpha_HE_nonBZDASM_SS, beta_HE_nonBZDASM_SS)))
      
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
    p_ICUadmiss_LE <<- p_ICUadmiss_LE
    p_ICUadmiss_HE <<- p_ICUadmiss_HE
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
  output$nonBZDASM_CEplot <- renderPlotly({
    
    nonBZDASM_data_for_plot <- data.frame(c(1-p_ICUadmiss_LE, 1-p_ICUadmiss_HE), c(c_LE, c_HE), c(rep("LE", 10000), rep("HE", 10000)))
    colnames(nonBZDASM_data_for_plot) <- c("p_SS", "c_LE", "LEvsHE")
    
    plot_CE <- ggplot(aes(x=p_SS, y=c_LE, color=LEvsHE), data=nonBZDASM_data_for_plot) + geom_point(alpha=0.25, pch=c(rep(16, 10000), rep(18, 10000)), color=c(rep("red", 10000), rep("darkgreen", 10000))) + xlim(0,1) + ylim(0, round_any(max(nonBZDASM_data_for_plot$c_LE), 500, f=ceiling)) +
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