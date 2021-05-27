################################################################################
# title: "2nd Part: Advanced Regression and Prediction"
# author: "Roberto J. Alcaraz Molina"
# date: "27/05/2021"
# https://mastering-shiny.org/
# SERVER FUNCTION
################################################################################

# install.packages("pacman")
pacman::p_load(shiny, tidyverse, tidymodels, COVID19, shinythemes, emo, thematic,
               bslib, plotly, tidyquant, mixOmics, stacks, GGally, ggpubr, 
               modeltime, timetk)
# devtools::install_github("hadley/emo")

# spain <- COVID19::covid19(country = "spain")
# saveRDS(spain, "spain.RDS")

spain <- readRDS("../00_data/spain.RDS")
spain_new_data <- readRDS("../00_data/spain_new_data.RDS")
spain_deaths <- readRDS("../00_data/spain_deaths.RDS")
spain_confirmed <- readRDS("../00_data/spain_confirmed.RDS")

workflow_results_deaths <- readRDS("../02_results/workflow_results_deaths.RDS")
workflow_results_confirmed <- readRDS("../02_results/workflow_results_confirmed.RDS")

best_fit_deaths <- readRDS("../02_results/best_fit_deaths.RDS")
best_fit_confirmed <- readRDS("../02_results/best_fit_confirmed.RDS")

deaths_stack_model_fit <- readRDS("../02_results/deaths_stack_model_fit.RDS")
confirmed_stack_model_fit <- readRDS("../02_results/confirmed_stack_model_fit.RDS")
results1 <- readRDS("../02_results/results1.RDS")
results2 <- readRDS("../02_results/results2.RDS")


server <- function(input, output){
  thematic::thematic_shiny()
  
  output$dataset <- renderDataTable(
    spain[, input$data], 
    options = list(
      pageLength = 10
    )
  )
  
  output$deaths_ts <- renderPlotly({
    spain %>%
      plot_time_series(.date_var = date, 
                       deaths_week, 
                       .smooth = F, 
                       .interactive = T, .line_size = 1,
                       .title = "Weekly deaths")
  })
  
  output$confirmed_ts <- renderPlotly({
    spain %>%
      plot_time_series(.date_var = date, 
                       confirmed_week, 
                       .smooth = F, 
                       .interactive = T, .line_size = 1,
                       .title = "Weekly confirmed cases")
    
  })
  
  
  
  output$metrics1 <- renderPlot({
    autoplot(workflow_results_deaths, select_best = T) +
      ggtitle("Results for the deaths model") +
      scale_color_tq() +
      theme_tq()
  })
  
  output$metrics2 <- renderPlot({
    autoplot(workflow_results_confirmed, select_best = T) +
      ggtitle("Results for the confirmed model") +
      scale_color_tq() +
      theme_tq()
  })
  
  output$stack1 <- renderPlot({
    p1 <- autoplot(deaths_stack_model_fit, type = "weights")
    p2 <- deaths_stack_model_fit$data_stack %>%
      rename(xgboost_deaths_046 = xgboost_deaths_wf_6_1_046, 
             svm_rbf_deaths_025 = svm_rbf_deaths_wf_3_1_025, 
             mlp_deaths_050 = mlp_deaths_wf_7_1_050, 
             mlp_deaths_056 = mlp_deaths_wf_7_1_056,
             mlp_deaths_033 = mlp_deaths_wf_7_1_033, 
             mlp_deaths_040 = mlp_deaths_wf_7_1_040,
             mlp_deaths_004 = mlp_deaths_wf_7_1_004, 
             mlp_deaths_045 = mlp_deaths_wf_7_1_045, 
             mlp_deaths_048 = mlp_deaths_wf_7_1_048,
             mlp_deaths_075 = mlp_deaths_wf_7_1_075) %>%
      dplyr::select(xgboost_deaths_046, svm_rbf_deaths_025, mlp_deaths_050, mlp_deaths_056,
                    mlp_deaths_033, mlp_deaths_040, mlp_deaths_004, mlp_deaths_045,
                    mlp_deaths_048, mlp_deaths_075) %>% 
      ggcorr(label = T, digits = 2, label_alpha = T, label_round = 2)
    
    
    ggarrange(p1, p2)
  })
  
  output$stack2 <- renderPlot({
    p1 <- autoplot(confirmed_stack_model_fit, type = "weights")
    p2 <- confirmed_stack_model_fit$data_stack %>%
      rename(svm_rbf_025 = svm_rbf_confirmed_wf_3_1_025, 
             mlp_015 = mlp_confirmed_wf_7_1_015, 
             dec_tree_058 = dec_tree_confirmed_wf_4_1_058, 
             dec_tree_002 = dec_tree_confirmed_wf_4_1_002
      ) %>%
      dplyr::select(svm_rbf_025, mlp_015, dec_tree_058, dec_tree_002) %>% 
      ggcorr(label = T, digits = 2, label_alpha = T, label_round = 2) +
      ggtitle("Correlation plot")
    
    
    ggarrange(p1, p2)
  })
  
  output$train <- renderPlotly({
    if (input$optFit1 == "Deaths"){
      pred_train_deaths <- best_fit_deaths %>%
        predict(new_data = spain)
      
      p1 <- spain %>%
        mutate(predictions = pred_train_deaths$.pred) %>%
        mutate(predictions = ifelse(predictions <0, 0, predictions)) %>% 
        dplyr::select(date, deaths_week, predictions) %>%
        pivot_longer(cols = c("deaths_week", "predictions"), values_to = "value", names_to = "Variable") %>%
        ggplot(aes(x = date, y = value)) +
        geom_line(aes(color = Variable), size = 1) +
        scale_color_manual(values = c("darkred", "steelblue")) +
        labs(
          y = "Deaths per week",
          x = "Date"
        ) 
      p1 +
        ggtitle("Predicted deaths in the training set by the RBF SVM") +
        theme(plot.title = element_text(hjust = 0.5, size = 20))
      
    } else if (input$optFit1 == "Confirmed") {
      pred_train_confirmed <- best_fit_confirmed %>%
        predict(new_data = spain)
      
      p2 <- spain %>%
        mutate(predictions = pred_train_confirmed$.pred) %>%
        mutate(predictions = ifelse(predictions < 0, 0, predictions)) %>% 
        dplyr::select(date, confirmed_week, predictions) %>%
        pivot_longer(cols = c("confirmed_week", "predictions"), values_to = "value", names_to = "Variable") %>%
        ggplot(aes(x = date, y = value)) +
        geom_line(aes(color = Variable), size = 1) +
        scale_color_manual(values = c("darkred", "steelblue")) +
        labs(
          y = "Confirmed cases per week",
          x = "Date"
        )  
      p2 +
        ggtitle("Predicted confirmed cases in the training set by the XGBoost model") +
        theme(plot.title = element_text(hjust = 0.5, size = 20))
    }
  })
  
  output$stack_train <- renderPlotly({
    if (input$optFit2 == "Deaths"){
      pred_train_deaths <- deaths_stack_model_fit %>%
        predict(new_data = spain)
      
      p1 <- spain %>%
        mutate(predictions = pred_train_deaths$.pred) %>%
        mutate(predictions = ifelse(predictions <0, 0, predictions)) %>% 
        dplyr::select(date, deaths_week, predictions) %>%
        pivot_longer(cols = c("deaths_week", "predictions"), values_to = "value", names_to = "Variable") %>%
        ggplot(aes(x = date, y = value)) +
        geom_line(aes(color = Variable), size = 1) +
        scale_color_manual(values = c("darkred", "steelblue")) +
        labs(
          y = "Deaths per week",
          x = "Date"
        ) 
      p1 +
        ggtitle("Predicted deaths in the training set by the ensemble model") +
        theme(plot.title = element_text(hjust = 0.5, size = 20))
      
    } else if (input$optFit2 == "Confirmed") {
      pred_train_confirmed <- confirmed_stack_model_fit %>%
        predict(new_data = spain)
      
      p2 <- spain %>%
        mutate(predictions = pred_train_confirmed$.pred) %>%
        mutate(predictions = ifelse(predictions < 0, 0, predictions)) %>% 
        dplyr::select(date, confirmed_week, predictions) %>%
        pivot_longer(cols = c("confirmed_week", "predictions"), values_to = "value", names_to = "Variable") %>%
        ggplot(aes(x = date, y = value)) +
        geom_line(aes(color = Variable), size = 1) +
        scale_color_manual(values = c("darkred", "steelblue")) +
        labs(
          y = "Confirmed cases per week",
          x = "Date"
        ) 
      p2 +
        ggtitle("Predicted confirmed cases in the training set by the ensemble model") +
        theme(plot.title = element_text(hjust = 0.5, size = 20))
    }
    
    
  })
  
  output$test <- renderDataTable(
    results1,
    options = list(
      searching = FALSE,
      paging = FALSE
    )
  )
  
  output$stack_test <- renderDataTable(
    results2,
    options = list(
      searching = FALSE,
      paging = FALSE
    )
  )
}

