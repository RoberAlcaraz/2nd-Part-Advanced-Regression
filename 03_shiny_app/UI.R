################################################################################
# title: "2nd Part: Advanced Regression and Prediction"
# author: "Roberto J. Alcaraz Molina"
# date: "27/05/2021"
# UI FUNCTION
################################################################################

pacman::p_load(shiny, tidyverse, tidymodels, COVID19, shinythemes, emo, shiny.semantic,
               thematic, bslib, plotly)
# devtools::install_github("hadley/emo")

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

introPanel <- tabPanel(
  "1. Introduction",
  sidebarLayout(
    # position = "right",
    sidebarPanel(
      h4(
        strong("Description of the variables: ")),
      HTML(paste0("<ul><li>",  
                  code("date"), ": Observation date. </li><li>", 
                  code("month"), ": Observation month </li><li>", 
                  code("year"), ": Observation year </li><li>", 
                  code("deaths_week"), ": Cumulative number of deaths per week. </li><li>", 
                  code("lag_3_deaths_week"), ": Cumulative number of deaths three weeks ago. </li><li>", 
                  code("confirmed_week"), ": Cumulative number of confirmed cases per week. </li><li>", 
                  code("lag_3_confirmed_week"), ": Cumulative number of confirmed cases three weeks ago. </li><li>", 
                  code("vaccines_week"), ": Cumulative number of doses administered (single dose) per week. </li><li>", 
                  code("stay_home"), ": Indicates the measures of staying at home. </li><li>", 
                  code("school_closing"), ": Indicates the measures in education. </li><li>", 
                  code("workplace_closing"), ": Indicates the measures of the workplace. </li><li>",
                  code("gatherings_restrictions"), ": Indicates the measures of gatherings. </li><li>",
                  code("internal_movement_restrictions"), ": Indicates the measures of the movements between regions.
                  </li></ul>")
           )
      ),
    mainPanel(
      h4("By:", em("Roberto J. Alcaraz Molina")),
      h4(em("27/05/2021")),
      br(),
      h1(strong("Introduction")),
      br(),
      p("This project is the second and final project of the Advanced Regression 
        and Prediction course. As we did in the first part, we are going to analyze 
        and try predict the behavior of the coronavirus crisis in Spain, but this time,
        we are going to use machine learning tools."),
      br(),
      p("All the analysis of this project, from the data cleaning until the model
        results will be done mainly with the", code("tidyverse"), "and ", code("tidymodels"),
        " packages. The first one is well known for R users but the second, even 
        though is still in development, has enough tools for modeling and machine 
        learning."),
      br(),
      p("All the code with the detailed explanations can be founded in the Rmarkdown
        of this repository.")
      )
  )
)

dataPanel <- wellPanel(
  shiny::selectInput("data", "Select the variables to be shown: ",
                     choices = colnames(spain), multiple = T, 
                     selected = c("date", "deaths_week", "confirmed_week")),
  dataTableOutput("dataset")
)

edaPanel <- tabPanel(
  "2. COVID-19 Data Set",
  sidebarLayout(
    sidebarPanel(
      h4(strong("Exploratory Data Analysis: ")),
      br(),
      p("In this section we can observe the data set we will be working on. We can
        select some variables of our data set, filter the rows, look for some specific
        value or show more entries."),
      br(),
      p("Also, we can observe on the right the time series plot of the deaths and
        for the confirmed cases per week."),
      br(),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Set", dataPanel),
        tabPanel("Deaths per week", plotlyOutput("deaths_ts")),
        tabPanel("Confirmed cases per week", plotlyOutput("confirmed_ts"))
      )
    )
  )
)


deathmodPanel <- wellPanel(
  br(),
  plotOutput("metrics1")
)

confmodPanel <- wellPanel(
  br(),
  plotOutput("metrics2")
)

tunePanel <- tabPanel(
  "3. Machine Learning Models",
  sidebarLayout(
    sidebarPanel(
      h4(strong("Machine Learning models: ")),
      br(),
      p("We are going to try 7 different models and in the left we have the mean
        results with their confidence interval of the RMSE (root mean square error) 
        and RSE (R square). The models will be:"),
      br(),
      p("- K-Nearest Neigbors."),
      p("- Polynomial Support Vector Regression"),
      p("- Radial Basis Function Support Vector Regression"),
      p("- Regression Trees"),
      p("- Random Forest"),
      p("- eXtreme Gradient Boosting"),
      p("- Neural Network (Multi-Layer Perceptron)")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Deaths model", deathmodPanel),
        tabPanel("Confirmed model", confmodPanel)
      )
    )
  )
)

deathstackPanel <- wellPanel(
  br(),
  plotOutput("stack1")
)

confstackPanel <- wellPanel(
  br(),
  plotOutput("stack2")
)

stackPanel <- tabPanel(
  "4. Stacking ensemble",
  sidebarLayout(
    sidebarPanel(
      h4(strong("Stacking process: ")),
      br(),
      p("We start by creating a data set with the outcome in the validation set 
        (in our case, every 3 weeks validation sets) and all the predictions from 
        each candidate member. Once we have our data set ready, we will apply a
        model to select the best set of candidates."),
      br(),
      p("Then, since the outputs of each member can be highly correlated, we should
        consider a model that skips some of the methods. Therefore, the models that
        are used in these steps are the ones we used in the previous project: 
        ridge regression, lasso or elastic net"),
      br(),
      p("After fitting one of these models, we get a coefficient for each member,
        which is called weight, that determines which models will be taken as final
        members of the ensemble."),
      br(),
      p("In the right, we can observe the selected models and their weights, as well
        as their correlation plot.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Deaths model", deathstackPanel),
        tabPanel("Confirmed model", confstackPanel)
      )
    )
  )
)


trainPanel <- wellPanel(
  shiny::selectInput("optFit1", label = "Select the model: ",
                     choices = c("Deaths", "Confirmed")),
  plotlyOutput("train")
)
testPanel <- wellPanel(
  br(),
  dataTableOutput("test")
)

stacktrainPanel <- wellPanel(
  shiny::selectInput("optFit2", label = "Select the model: ",
                     choices = c("Deaths", "Confirmed")),
  plotlyOutput("stack_train")
)
stacktestPanel <- wellPanel(
  br(),
  dataTableOutput("stack_test")
)

selPanel <- tabPanel(
  "5. Model selection and prediction",
  sidebarLayout(
    sidebarPanel(
      h4(strong("Model selection and prediction: ")),
      br(),
      p("For the deaths model, the model that has outperformed the rest is the radial
        basis function support vector machines,  which has around 130 RMSE and almost 
        0.7 RSQ on average."),
      br(),
      p("On the other hand, we can observe that the best model seem to be the decision tree,
        however, it does  not have any value for RSQ. It occurs when the model predicts
        a single value for all samples, so it is not what we want. Therefore, we 
        will select the second best in RMSE, which is also the best in RSQ: the XGBoost."),
      br(),
      p("On the right, we are able to see the model predictions in the training
        and testing sets for the selected and the ensemble models."),
      br(),
      p("Even though the ensemble's predictions in the training set are almost perfect,
        in the testing set they are not doing a good job. It may happen due to overfitting
        and lack of information. On the other hand, the selected models do a great
        job in the testing set.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Training set Pred.", trainPanel),
        tabPanel("Testing set Pred.", testPanel),
        tabPanel("Training set Ensemble Pred.", stacktrainPanel),
        tabPanel("Testing set Ensemble Pred.", stacktestPanel)
      )
    )
  )
)

refPanel <- tabPanel(
  "References",
  mainPanel(
    p(tags$button(class="btn btn-default", 
                  `data-toggle`="collapse", 
                  `data-target`="#collapseExample",
                  "References")),
    
    div(class="collapse", id="collapseExample",
        div(class="card card-body",
            includeMarkdown("references.md")
        ))
  )
)

ui <- navbarPage("Advanced Regression And Prediction",
                 theme = bslib::bs_theme(bootswatch = "flatly"),
                 introPanel,
                 edaPanel,
                 tunePanel,
                 stackPanel,
                 selPanel,
                 refPanel
)





