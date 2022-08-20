library(shiny)
library(shinythemes)
library(ggplot2)
library(data.table)
library(DescTools)
library(dplyr)
library(googlesheets4)
library(rsconnect)

not_sel <- "Not Selected"

googlesheets4::gs4_deauth()

#reading the data
data_df <- read_sheet("https://docs.google.com/spreadsheets/d/1kXjrVRg5O0T5DR503XWJDT5_KOGKSkooAk9MmTo3e94/edit?usp=sharing")

#data_2_df <- read_sheet("https://docs.google.com/spreadsheets/d/1R_NL8fJfA8W0inE27qeqGAXafVmwP7yCip7zhdqV1jg/edit?usp=sharing")

data <- as.data.table(data_df)
#data_2 <- as.data.table(data_2_df)

draw_plot_1 <- function(data_input, num_var_1, num_var_2, fact_var_1, fact_var_2){
  if(fact_var_1!=not_sel & fact_var_2!= not_sel){
    data_input[,(fact_var_1):= as.factor(data_input[,get(fact_var_1)])]
    data_input[,(fact_var_2):= as.factor(data_input[,get(fact_var_2)])]
  }
  if(num_var_1 != not_sel & num_var_2 != not_sel){
    data_input[,(num_var_1):= as.numeric(data_input[,get(num_var_1)])]
    data_input[,(num_var_2):= as.numeric(data_input[,get(num_var_2)])]
  }
  if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var_1 != not_sel & fact_var_2 == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1, y = num_var_2, color = fact_var_1)) +
      geom_point()
  }
  else if (num_var_1 != not_sel & num_var_2 != not_sel & fact_var_1 == not_sel & fact_var_2 != not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1, y = num_var_2, color = fact_var_2)) +
      geom_line()
  }
  else if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var_1 == not_sel & fact_var_2== not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1, y = num_var_2)) +
      geom_line(na.rm = TRUE)
  }
  else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var_1 != not_sel & fact_var_2 == not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var_1, y = num_var_1, fill=fact_var_1)) +
      geom_violin()
  }
  else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var_1 != not_sel & fact_var_2 == not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var_1, y = num_var_2, fill=fact_var_1)) +
      geom_violin()
  }
  else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var_1 == not_sel & fact_var_2 != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var_2, y = num_var_2, fill=fact_var_2)) +
      geom_violin()
  }
  else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var_1 == not_sel & fact_var_2 != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var_2, y = num_var_1, fill=fact_var_2)) +
      geom_violin()
  }
  else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var_1 == not_sel & fact_var_2 == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1)) +
      geom_boxplot(stat = "boxplot", na.rm = TRUE, outlier.colour = "red", fill="lightblue", position = "dodge2") #+ scale_fill_gradient("Count", low="green", high="red")
  }
  else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var_1 == not_sel & fact_var_2 == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_2)) +
      geom_boxplot(stat = "boxplot", na.rm = TRUE, outlier.colour = "red", fill="lightblue", position = "dodge2") #+ scale_fill_gradient("Count", low="green", high="red")
  }
  else if(num_var_1 == not_sel & num_var_2 == not_sel & fact_var_1 != not_sel & fact_var_2 == not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var_1, fill=fact_var_1)) +
      geom_bar(position = "dodge", na.rm = TRUE) + geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black")
  }
  else if(num_var_1 == not_sel & num_var_2 == not_sel & fact_var_1 == not_sel & fact_var_2 != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var_2, fill=fact_var_2)) +
      geom_bar(position = "dodge", na.rm = TRUE) + geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black")
  }
  else if(num_var_1 == not_sel & num_var_2 == not_sel & fact_var_1 != not_sel & fact_var_2 != not_sel){
    ggplot(data = data_input,
           aes_string(x=fact_var_1, fill = fact_var_2)) +
      geom_bar(position = "dodge", na.rm = TRUE)
  }
  else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var_1 != not_sel & fact_var_2 != not_sel){
    ggplot(data = data_input,
           aes_string(x=fact_var_1, y=num_var_1, fill = fact_var_2)) +
      geom_bar(position = "dodge", stat = "identity", na.rm = TRUE)
  }
  else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var_1 != not_sel & fact_var_2 != not_sel){
    ggplot(data = data_input,
           aes_string(x=fact_var_1, y=num_var_2, fill = fact_var_2)) +
      geom_bar(position = "dodge", stat="identity", na.rm = TRUE)
  }
}


# draw_plot_2 <- function(data_input, data_input_2, num_var_1, num_var_2, endline_num_var){
#   if (endline_num_var != not_sel){
#     data_input_2[,(endline_num_var):= as.numeric(data_input_2[,get(endline_num_var)])]
#   }
#   #if (endline_fact_var != not_sel){
#     #data_input_2[,(endline_fact_var):= as.factor(data_input_2[,get(endline_fact_var)])]
#   #}
#   if (num_var_1 != not_sel & endline_num_var != not_sel & num_var_2 != not_sel){
#     ggplot(data = data_input, 
#            aes_string(x=num_var_1, y= num_var_2)) + geom_line() + lines( x= num_var_1, y = endline_num_var, col= "red")
#   }
# }



main_page <- tabPanel(
  title = "Analysis",
  titlePanel("Data Selector For Analysis"),
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
      selectInput("num_var_1", "First Numerical Variable", choices = c(not_sel)),
      selectInput("num_var_2", "Second Numerical Variable", choices = c(not_sel)),
      selectInput("fact_var_1","First Factor Variable", choices = c(not_sel)),
      selectInput("fact_var_2", "Second Factor Variable", choices = c(not_sel)),
      actionButton("run_button", "Run Analysis", icon = icon("play")),
      downloadButton("download_button","download", class = "btn-block", icon = shiny::icon("download"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Plot",
          plotOutput("plot_1")
        ),
        tabPanel(
          title = "Statistics",
          fluidRow(
            column(width = 4, strong(textOutput("num_var_1_title"))),
            column(width = 4, strong(textOutput("num_var_2_title"))),
            column(width = 4, strong(textOutput("fact_var_1_title"))),
            column(width = 4, strong(textOutput("fact_var_2_title")))
          ),
          fluidRow(
            column(width = 4, tableOutput("num_var_1_summary_table")),
            column(width = 4, tableOutput("num_var_2_summary_table")),
            column(width = 4, tableOutput("fact_var_1_summary_table")),
            column(width = 4, tableOutput("fact_var_2_summary_table"))
          ),
          fluidRow(
            column(width = 6, strong("Combined Statistics"))
          ),
          fluidRow(
            column(width = 6, tableOutput("Combined_summary_table"))
          )
        )
      )
    )
  )
)


# compare_page <- tabPanel(
#   title = "Compare Data",
#   titlePanel("Compare Data"),
#   sidebarLayout(
#     sidebarPanel(
#       title = "endline",
#       selectInput("endline_num_var", "Endline Numerical Variable", choices = c(not_sel)),
#       selectInput("endline_fact_var","Endline Factor Variable", choices = c(not_sel)),
#       actionButton("run_button_2", "Run Comparison", icon = icon("play"))
#     ),
#     mainPanel(
#       tabsetPanel(
#         tabPanel(
#           title = "Plot_2",
#           plotOutput("plot_2")
#         ),
#         tabPanel(
#           title = "Comparison Statistics",
#           fluidRow(
#             column(width = 6, tableOutput("side_by_side_comparison"))
#           )
#         )
#         )
#       )
#     )
# )

about_page <- tabPanel(
  title = "About Survey",
  titlePanel("About"), 
  sidebarLayout(
    sidebarPanel(
      tags$img(src = "logo1.png", height = 250, width = 300)
    ),
    mainPanel(
      title = "About Page",
      p(strong("A Case of Digitizing Sanduks in Sudan - South Sudan Border")),
      p(),
      p("In May 2021, the", strong("Africa Borderlands Centre (ABC)"), "launched the 2021 Innovation Challenge themed as: Improving Livelihoods for Informal Cross Border Traders and Trading Communities. The goal of the challenge was to curate, experiment and upscale local borderland innovations that have become the lynchpin of entrepreneurship culture which has sustained Informal Cross-Border Trade (ICBT).
        The ideas of interest approved include those capable of providing durable solutions to the barriers against ICBT, particularly through the expansion of livelihoods opportunities to women and youths. The work plan for all ten participating Accelerator Laboratories has five components including local sensing, innovation design, solution exploration, monitoring and evaluation and reporting.", style = "font-family: 'times'; font-si30pt"),
      p("South Sudan-Sudan AccLabs completed activities in the first component. On innovation design, AccLabs are required to conduct a baseline survey, identify participants to the experimental and control groups and finalize their work plan and budget.  To this end, South Sudan AccLab conducted a field mission to Warawar to collect baseline data from 6th to 17th December 2021.
        The data from the survey questionnaires was consolidated and sent to ABC innovation team for analysis on 26th January 2022. Results of the analysis identified areas of improvement which required technical input to enhance the proof-of-concept. ", style = "font-family: 'times'; font-si30pt")
    )
  )
  )



ui <- navbarPage(
  title = "South Sudan Survey Data Analyzer",
  theme = shinytheme("cerulean"),
  main_page,
  about_page
)

server <- function(input, output, session){
  data_input <- reactive({
    data
  })
  observeEvent(data_input(),{
    choices <- c(not_sel, names(data_input()))
    updateSelectInput(inputId = "num_var_1", choices = choices)
    updateSelectInput(inputId = "num_var_2", choices = choices)
    updateSelectInput(inputId = "fact_var_1", choices = choices)
    updateSelectInput(inputId = "fact_var_2", choices = choices)
  })
  num_var_1 <- eventReactive(input$run_button,input$num_var_1)
  num_var_2 <- eventReactive(input$run_button,input$num_var_2)
  fact_var_1 <- eventReactive(input$run_button,input$fact_var_1)
  fact_var_2 <- eventReactive(input$run_button, input$fact_var_2)
  
  #drawing the plot
  plot_1 <- eventReactive(input$run_button, {
    draw_plot_1(data_input(), num_var_1(),num_var_2(),fact_var_1(), fact_var_2())
  })
  output$plot_1 <- renderPlot(plot_1())
  
  
  # creating the table
  output$num_var_1_title <- renderText(paste("Numerical Var 1:", num_var_1()))
  output$num_var_2_title <- renderText(paste("Numerical Var 2:", num_var_2()))
  output$fact_var_1_title <- renderText(paste("Factor Var 1:", fact_var_1()))
  output$fact_var_2_title <- renderText(paste("Factor Var 2:", fact_var_2()))
  
  # creating the reactive expression to run a function
  num_var_1_summary_table <- eventReactive(input$run_button, {
    create_num_var_table(data_input(), num_var_1())
  })
  
  num_var_2_summary_table <- eventReactive(input$run_button, {
    create_num_var_table(data_input(), num_var_2())
  })
  
  fact_var_1_summary_table <- eventReactive(input$run_button, {
    create_fact_var_table(data_input(), fact_var_1())
  })
  
  fact_var_2_summary_table <- eventReactive(input$run_button, {
    create_fact_var_table(data_input(), fact_var_2())
  })
  
  Combined_summary_table <- eventReactive(input$run_button, {
    create_comb_var_table(data_input(), num_var_1(), num_var_2())
  })
  
  
  # creating the numerical function
  create_num_var_table <- function(data_input, num_var){
    if(num_var != not_sel){
      col <- data_input[,get(num_var)]
      if(length(col) > 5000) col_norm <- sample(col, 5000) else col_norm <- col
      norm_test <- shapiro.test(col_norm)
      statistic <- c("mean", "median","25th percentile", "95th Percentile", "Standard Deviation", "Shapiro Statistic", "Shapiro P-value")
      value <- c(round(mean(col), 2), round(median(col), 2), 
                 round(quantile(col, 0.25), 2), round(quantile(col, 0.95), 2),
                 round(sd(col), 2),
                 norm_test$statistic, norm_test$p.value)
      data.table(statistic, value)
    }
  }
  
  
  # creating the factor function
  create_fact_var_table <- function(data_input, fact_var){
    if(fact_var != not_sel){
      fact <- data_input[,get(fact_var)]
      getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
      }
      statistic2 <- c("Count", "Levels", "Mode")
      value2 <- c(length(fact), length(levels(fact)), getmode(fact))
      data.table(statistic2, value2)
    }
  }
  
  
  # creating the combined summary table data
  create_comb_var_table <- function( data_input, num_var_1, num_var_2){
    if(num_var_1 != not_sel && num_var_2 != not_sel){
      col_1 <- data_input[,get(num_var_1)]
      col_2 <- data_input[,get(num_var_2)]
      statistic <- c("Correlation")
      value <- c(cor(col_1, col_2, method = "pearson"))
      data.table(statistic, value)
    }
  }
  
  # render the data table
  output$num_var_1_summary_table <- renderTable(num_var_1_summary_table(), colnames = FALSE)
  
  output$num_var_2_summary_table <- renderTable(num_var_2_summary_table(), colnames = FALSE)
  
  output$fact_var_1_summary_table <- renderTable(fact_var_1_summary_table(), colnames = FALSE)
  
  output$fact_var_2_summary_table <- renderTable(fact_var_2_summary_table(), colnames = FALSE)
  
  output$Combined_summary_table <- renderTable(Combined_summary_table(), colnames = FALSE)
  
  #loading the endline data
  # data_input_2 <- reactive({
  #   data_2
  # })
  # observeEvent(data_input_2(),{
  #   choices <- c(not_sel, names(data_input_2()))
  #   updateSelectInput(inputId = "endline_num_var", choices = choices)
  #   updateSelectInput(inputId = "endline_fact_var", choices = choices)
  # })
  # endline_num_var <- eventReactive(input$run_button_2, input$endline_num_var)
  # endline_fact_var <- eventReactive(input$run_button_2, input$endline_fact_var)
  # 
  # # drawing 2nd plot
  # plot_2 <- eventReactive(input$run_button_2, {
  #   draw_plot_2(data_input(), data_input_2(), num_var_1(),num_var_2(),endline_num_var())
  # })
  # output$plot_2 <- renderPlot(plot_2())
  # 
  # # reactive expression
  # Comparison_table <- eventReactive(input$run_button_2, {
  #   create_comparison_table(data_input(), data_input_2(), num_var_1(), num_var_2(), fact_var_1(), fact_var_2(), endline_num_var(), endline_fact_var())
  # })
  # # creating the comparison table
  # create_comparison_table <- function(data_input, data_input_2, num_var_1, num_var_2, fact_var_1, fact_var_2, endline_num_var, endline_fact_var){
  #   if (endline_num_var != not_sel){
  #     data_input_2[,(endline_num_var):= as.numeric(data_input_2[,get(endline_num_var)])]
  #   }
  #   if (endline_fact_var != not_sel){
  #     data_input_2[,(endline_fact_var):= as.factor(data_input_2[,get(endline_fact_var)])]
  #   }
  #   if (num_var_1 != not_sel & num_var_2 == not_sel & fact_var_1 == not_sel & fact_var_2 == not_sel & endline_num_var != not_sel & endline_fact_var == not_sel){
  #     col_1 <- data_input[,get(num_var_1)]
  #     col_2 <- data_input_2[,get(endline_num_var)]
  #     variable <- c(names(data_input[,num_var_1]), names(data_input_2[,endline_num_var]))
  #     min <- c(min(col_1), min(col_2))
  #     mean <- c(mean(col_1), mean(col_2))
  #     max <- c(max(col_1), max(col_2))
  #     std <- c(sd(col_1), sd(col_2))
  #     
  #     data.table(variable, min, mean, max, std)
  #   }
  # }
  # 
  # # render the table
  # output$side_by_side_comparison <- renderTable(Comparison_table())
  
  # downloading the output
  output$download_button <- downloadHandler(
    filename = "output.pdf",
    content = function(file) {
      pdf(file)
      plot_1()
      dev.off()
    }
  )
  
}

shinyApp(ui, server)

